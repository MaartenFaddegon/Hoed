{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ViewPatterns          #-}
-- This file is part of the Haskell debugger Hoed.
--
-- Copyright (c) Maarten Faddegon, 2014-2017
module Debug.Hoed.Console(debugSession, showGraph) where

import           Control.Monad
import           Data.Bifunctor
import           Data.Char
import           Data.Foldable            as F
import           Data.Indexable
import           Data.Graph.Libgraph      as G
import           Data.List                as List (foldl', group, mapAccumL, nub, sort)
import qualified Data.Map.Strict          as Map
import           Data.Maybe
import           Data.Sequence            (Seq, ViewL (..), viewl, (<|))
import qualified Data.Sequence            as Seq
import qualified Data.Set                 as Set
import qualified Data.Vector              as V
import           Debug.Hoed.Compat
import           Debug.Hoed.CompTree
import           Debug.Hoed.Observe
import           Debug.Hoed.Prop
import           Debug.Hoed.ReadLine
import           Debug.Hoed.Render
import           Debug.Hoed.Serialize
import           Prelude                  hiding (Right)
import           System.Directory
import           System.Exit
import           System.IO
import           System.Process
import           Text.PrettyPrint.FPretty
import           Text.Regex.TDFA
import           Web.Browser

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

--------------------------------------------------------------------------------
-- events
type Id = Int
-- | A representation of an event as a span
data Span = Span { spanStart, spanEnd :: Id, polarity :: Bool } deriving (Eq, Ord, Show)
-- | A representation of a trace as a span diagram: a list of columns of spans.
type Spans = [ [Span] ]
-- | The nesting depth of a span
type Depth = Int

-- | Recover the spans from the trace and arrange the columns such that they reflect
--   span containment. A span @b@ is contained in another span @a@ if
--
--   > start b > start a && end b < end a
--
--   The span in column N is contained below N other spans.
--   The renderer should draw the columns left to right.
traceToSpans :: Trace -> Spans
traceToSpans =
  map sort .
  Map.elems .
  Map.fromListWith (++) .
  map (second (: [])) . (\(_, b, _) -> b) . foldl' traceToSpans1' ([], [], 0)

-- Implements a loop where the state is a stack of open spans.
-- Push into the stack when an event starts a new span.
-- Pop when an event closes a span.
-- In order to skip the Observe->Start->Fun sequence
--  , the state also carries a skip counter
-- XXX this version doesn't work well with curried functions
traceToSpans1' :: (Seq Id, [(Depth, Span)], Int) -> Event -> (Seq Id, [(Depth, Span)], Int)
traceToSpans1' (stack, result, n) Event{change=Observe{}} = (stack, result, n+2)
traceToSpans1' (stack, result, 0) e@Event {..}
  | isStart change = (eventUID <| stack, result, 0)
  | start :< stack' <- viewl stack
  , isEnd change =
    (stack', (Seq.length stack', Span start eventUID True) : result, 0)
  | otherwise = (stack, result, 0)
  where
    isStart Enter {} = True
    isStart _ = False
    isEnd Cons {} = True
    --isEnd Fun {} = True
    isEnd _ = False
traceToSpans1' (stack, result, n) e = (stack, result, n-1)

-- This version tries to distinguish between Fun events that terminate a span,
-- and Fun events that signal a second application of a function
-- TODO work in progress
traceToSpans2'
  :: V.Vector Event
  -> (Seq Id, [(Depth, Span)])
  -> Event
  -> (Seq Id, [(Depth, Span)])
traceToSpans2' events (stack, result) e@Event {..}
  | isStart change = (eventUID <| stack, result)
  | start :< stack' <- viewl stack
  , isEnd change =
    (stack', (Seq.length stack', Span start eventUID True) : result)
  | otherwise = (stack, result)
  where
    isStart Enter {} = True
    isStart _ = False
    isEnd Cons {} = True
    --isEnd Fun {} = True
    isEnd _ = False

printTrace :: Trace -> IO ()
printTrace trace = putStrLn $ renderTrace' (traceToSpans trace, trace)

-- | TODO to be improved
renderTrace :: (Spans, Trace) -> IO ()
renderTrace (spans, trace) = do
  putStrLn "Events"
  putStrLn "------"
  mapM_ print trace
  putStrLn ""
  putStrLn "Spans"
  putStrLn "-----"
  mapM_ print spans

renderTrace' :: (Spans, Trace) -> String
renderTrace' (columns, events) = unlines renderedLines
    -- roll :: State -> (Event, Maybe ColumnEvent) -> (State, String)
  where
    depth = length columns
    ((_, evWidth), renderedLines) =
      mapAccumL roll (replicate (depth + 1) ' ', 0) $ align (toList events) columnEvents
    -- Merge trace events and column events
    align (ev:evs) (colEv@(rowIx, colIx, isStart):colEvs)
      | eventUID ev == rowIx = (ev, Just (colIx, isStart)) : align evs colEvs
      | otherwise = (ev, Nothing) : align evs (colEv : colEvs)
    align [] [] = []
    align ev [] = map (, Nothing) ev
    -- Produce the output in three columns: spans, events, and explains
    -- For spans: keep a state of the open spans
    --   (the state is the rendering in characters)
    -- For events: keep a state of the widest event
    -- For explains: circularly reuse the widest event result
    roll (state, width) (ev, Nothing)
      | (w, s) <- showWithExplains ev = ((state, max width w), state ++ s)
    roll (state, width) (ev, Just (col, True))
      | state' <- update state col '|'
      , state'' <- update state col '↑'
      , (w, s) <- showWithExplains ev = ((state', max width w), state'' ++ s)
    roll (state, width) (ev, Just (col, False))
      | state' <- update state col ' '
      , state'' <- update state col '↓'
      , (w, s) <- showWithExplains ev = ((state', max width w), state'' ++ s)
    -- columnEvents :: [(Line,ColIndex,StartOrEnd)]
    -- view the column spans as an event stream
    -- there's an event when a span starts or ends
    columnEvents =
      sortOn
        (\(a, b, c) -> a)
        [ (rowIx, colIx, isStart)
        | (colIx, spans) <- zip [0 ..] columns
        , Span {..} <- spans
        , (rowIx, isStart) <- [(spanStart, True), (spanEnd, False)]
        ]
    -- update element n of a list with a new value v
    update [] _ _ = []
    update (_:xs) 0 v = v : xs
    update (x:xs) n v = x : update xs (n - 1) v
    -- fast lookup via an array
    lookupEvent = (indexableAt events)
    -- show the event, add the necessary padding to fill the event col width,
    -- and append the explain.
    showWithExplains ev
      | showEv <- show ev
      , l <- length showEv =
        (l, showEv ++ replicate (evWidth - l) ' ' ++ explain ev)
    explain Event {eventParent = Parent p 0, change = Enter}
      | Event {change = Fun, eventParent = Parent p' _} <- lookupEvent p
      , Event {change = Observe name} <- lookupEvent p' =
        "-- request arg of " ++ name
    explain Event {eventParent = Parent p 1, change = Enter}
      | Event {change = Fun, eventParent = Parent p' _} <- lookupEvent p
      , Event {change = Observe name} <- lookupEvent p' =
        "-- request result of " ++ name
    explain Event {eventParent = Parent p 0, change = Cons {}}
      | Event {change = Fun, eventParent = Parent p' _} <- lookupEvent p
      , Event {change = Observe name} <- lookupEvent p' =
        "-- return arg of " ++ name
    explain Event {eventParent = Parent p 1, change = Cons {}}
      | Event {change = Fun, eventParent = Parent p' _} <- lookupEvent p
      , Event {change = Observe name} <- lookupEvent p' =
        "-- return result of " ++ name
    explain _ = ""
--------------------------------------------------------------------------------
-- Debug session

debugSession :: Trace -> CompTree -> [Propositions] -> IO ()
debugSession trace tree ps =
  case filter (not . isRootVertex) vs of
    []    -> putStrLn $ "No functions annotated with 'observe' expressions"
                        ++ " or annotated functions not evaluated"
    (v:_) -> do noBuffering
                mainLoop v trace tree ps
  where
  (Graph _ vs _) = tree

--------------------------------------------------------------------------------
-- Execution loop

type Frame state = state -> IO (Transition state)

data Transition state
  = Down (Frame state)
  | Up   (Maybe state)
  | Next state
  | Same

executionLoop :: [Frame state] -> state -> IO ()
executionLoop [] _ = return ()
executionLoop stack@(runFrame : parents) state = do
  transition <- runFrame state
  case transition of
    Same         -> executionLoop stack state
    Next st      -> executionLoop stack st
    Up Nothing   -> executionLoop parents state
    Up (Just st) -> executionLoop parents st
    Down loop    -> executionLoop (loop : stack) state

--------------------------------------------------------------------------------
-- Commands

type Args = [String]

data Command state = Command
  { name        :: String
  , argsDesc    :: [String]
  , commandDesc :: Doc
  , parse       :: Args -> Maybe (state -> IO (Transition state))
  }

interactiveFrame :: String -> [Command state] -> Frame state
interactiveFrame prompt commands state = do
  input <- readLine (prompt ++ " ") (map name commands)
  let run = fromMaybe (\_ -> Same <$ showHelp commands) $ selectCommand input
  run state
  where
    selectCommand = selectFrom commands

showHelp :: [Command state] -> IO ()
showHelp commands =
  putStrLn (pretty 80 $ vcat $ zipWith compose commandsBlock descriptionsBlock)
  where
    compose c d = text (pad c) <+> align d
    commandsBlock = [unwords (name : argsDesc) | Command {..} <- commands]
    descriptionsBlock = map commandDesc commands
    colWidth = maximum $ map length commandsBlock
    pad x = take (colWidth + 1) $ x ++ spaces
    spaces = repeat ' '

helpCommand :: [Command state1] -> Command state2
helpCommand commands =
  Command "help" [] "Shows this help screen." $ \case
    [] -> Just $ \_ -> Same <$ showHelp commands
    _  -> Nothing

selectFrom :: [Command state] -> String -> Maybe (state -> IO (Transition state))
selectFrom commands =
  \case
    "" -> Nothing
    xx -> do
      let (h:t) = words xx
      c <- Map.lookup h commandsMap
      parse c t
  where
    commandsMap = Map.fromList [(name c, c) | c <- commands]


--------------------------------------------------------------------------------
-- main menu

data State = State
  { cv       :: Vertex
  , trace    :: Trace
  , compTree :: CompTree
  , ps       :: [Propositions]
  }

adbCommand, graphCommand, observeCommand, listCommand, exitCommand :: Command State
adbCommand =
  Command "adb" [] "Start algorithmic debugging." $ \case
    [] -> Just $ \_ -> return $ Down adbFrame
    _  -> Nothing

observeCommand =
  Command
    "observe"
    ["[regexp]"]
    ("Print computation statements that match the regular expression." </>
     "Omitting the expression prints all the statements.") $ \case
    args -> Just $ \State {..} ->
      let regexp = case args of [] -> ".*" ; _ -> unwords args
      in Same <$ printStmts compTree regexp

listCommand =
  Command "list" [] "List all the observables collected." $
    \args -> Just $ \State{..} ->
      let regexp = makeRegex $ case args of [] -> ".*" ; _ -> unwords args
      in Same <$ listStmts compTree regexp

graphCommand =
  Command "graph" ["regexp"]
    ("Show the computation graph of an expression." </>
     "Requires graphviz dotp.") $ \case
        regexp -> Just $ \State{..} -> Same <$ graphStmts (unwords regexp) compTree

eventsCommand =
  Command "events" [] "Print the Event trace (useful only for debugging Hoed)" $ \case
    [] -> Just $ \State{..} -> Same <$ printTrace trace
    _  -> Nothing

exitCommand =
  Command "exit" [] "Leave the debugging session." $ \case
    [] -> Just $ \_ -> return (Up Nothing)
    _  -> Nothing

mainLoopCommands :: [Command State]
mainLoopCommands =
  sortOn name
    [ adbCommand
    , eventsCommand
    , graphCommand
    , listCommand
    , observeCommand
    , exitCommand
    , helpCommand mainLoopCommands
    ]

mainLoop :: Vertex -> Trace -> CompTree -> [Propositions] -> IO ()
mainLoop cv trace compTree ps =
  executionLoop [interactiveFrame "hdb>" mainLoopCommands] $
  State cv trace compTree ps

--------------------------------------------------------------------------------
-- list

listStmts :: CompTree -> Regex -> IO ()
listStmts g regex =
  putStrLn $
  unlines $
  snub $
  map (stmtLabel . vertexStmt . G.root) $
  selectVertices (\v -> matchLabel v && isRelevantToUser g v) g
  where
    matchLabel RootVertex = False
    matchLabel v          = match regex (stmtLabel $ vertexStmt v)
    snub = map head . List.group . sort

-- Restricted to statements for lambda functions or top level constants.
-- Discards nested constant bindings
isRelevantToUser :: Graph Vertex arc -> Vertex -> Bool
isRelevantToUser _ Vertex {vertexStmt = CompStmt {stmtDetails = StmtLam {}}} =
    True
isRelevantToUser g v@Vertex {vertexStmt = CompStmt {stmtDetails = StmtCon {}}} =
    RootVertex `elem` preds g v
isRelevantToUser _ RootVertex = False

-- | Returns the vertices satisfying the predicate. Doesn't alter the graph.
selectVertices :: (Vertex->Bool) -> CompTree -> [CompTree]
selectVertices pred g = [ g{G.root = v} | v <- vertices g, pred v]

matchRegex :: Regex -> Vertex -> Bool
matchRegex regex v = match regex $ noNewlines (vertexRes v)

subGraphFromRoot :: Ord v => Graph v a -> Graph v a
subGraphFromRoot g = subGraphFrom (G.root g) g

subGraphFrom :: Ord v => v -> Graph v a -> Graph v a
subGraphFrom v g = Graph {root = v, vertices = filteredV, arcs = filteredA}
  where
    filteredV = getPreorder $ getDfs g {G.root = v}
    filteredSet = Set.fromList filteredV
    filteredA =
      [ a
      | a <- arcs g
      , Set.member (source a) filteredSet && Set.member (target a) filteredSet
      ]

--------------------------------------------------------------------------------
-- observe


printStmts :: CompTree -> String -> IO ()
printStmts g regexp
    | null vs_filtered  =
      putStrLn $ "There are no computation statements matching \"" ++ regexp ++ "\"."
    | otherwise = forM_ (zip [0..] $ nubOrd $ map printStmt vs_filtered) $ \(n,s) -> do
    putStrLn $ "--- stmt-" ++ show n ++ " ------------------------------------------"
    putStrLn s
  where
  vs_filtered =
    map subGraphFromRoot .
    sortOn (vertexRes . G.root) .
    selectVertices (\v -> matchRegex r v && isRelevantToUser g v) $
    g
  r = makeRegex regexp
  nubOrd = nub -- We want nubOrd from the extra package

printStmt :: CompTree -> String
printStmt g = unlines $
    show(vertexStmt $ G.root g) :
    concat
      [ "  where" :
        map ("    " ++) locals
      | not (null locals)]
  where
    locals =
          -- constants
          [ stmtRes c
          | Vertex {vertexStmt = c@CompStmt {stmtDetails = StmtCon{}}} <-
              succs g (G.root g)
          ] ++
          -- function calls
          [ stmtRes c
          | Vertex {vertexStmt = c@CompStmt {stmtDetails = StmtLam{}}} <-
              succs g (G.root g)
          ]

--------------------------------------------------------------------------
-- graph
graphStmts :: String -> CompTree -> IO ()
graphStmts "" g = renderAndOpen g
graphStmts (makeRegex -> r) g = do
          let matches =
                map subGraphFromRoot $
                selectVertices (\v -> matchRegex r v && isRelevantToUser g v) g
          case matches of
            [one] -> renderAndOpen one
            _ ->
              putStrLn "More than one match, please select only one expression."

renderAndOpen g = do
  tempDir <- getTemporaryDirectory
  (tempFile, hTempFile) <- openTempFile tempDir "hoed.svg"
  hClose hTempFile
  cmd "dot" ["-Tsvg", "-o", tempFile] (showGraph g)
  _success <- openBrowser ("file:///" ++ tempFile)
  return ()

showGraph g = showWith g showVertex showArc
  where
    showVertex RootVertex = ("\".\"", "shape=none")
    showVertex v          = ("\"" ++ (escape . showCompStmt) v ++ "\"", "")
    showArc _ = ""
    showCompStmt = show . vertexStmt

cmd line args inp = do
  putStrLn $ unwords (line:args)
  (exit, stdout, stderr) <- readProcessWithExitCode line args inp
  unless (exit == ExitSuccess) $ do
    putStrLn $ "Failed with code: " ++ show exit
    putStrLn stdout
    putStrLn stderr
  return exit

--------------------------------------------------------------------------------
-- algorithmic debugging

adbCommands :: [Command State]
adbCommands = [judgeCommand Right, judgeCommand Wrong]

judgeCommand :: Judgement -> Command State
judgeCommand judgement =
  Command
    verbatim
    []
    ("Judge computation statements" </>
     text verbatim </>
     " according to the intended behaviour/specification of the function.") $ \case
    [] -> Just $ \st -> adb_judge judgement st
    _  -> Nothing
  where
    verbatim | Right <- judgement = "right"
             | Wrong <- judgement = "wrong"

adbFrame :: State -> IO (Transition State)
adbFrame st@State{..} =
  case cv of
    RootVertex -> do
      putStrLn "Out of vertexes"
      return $ Up Nothing
    _ -> do
      adb_stats compTree
      print $ vertexStmt cv
      case lookupPropositions ps cv of
        Nothing   -> interactive st
        Just prop -> do
          judgement <- judge trace prop cv unjudgedCharacterCount compTree
          case judgement of
            (Judge Right)                    -> adb_judge Right st
            (Judge Wrong)                    -> adb_judge Wrong st
            (Judge (Assisted msgs))          -> do
              mapM_ (putStrLn . toString) msgs
              interactive st
            (AlternativeTree newCompTree newTrace) -> do
              putStrLn "Discovered simpler tree!"
              let cv' = next RootVertex newCompTree
              return $ Next $ State cv' newTrace newCompTree ps
  where
    interactive = interactiveFrame "?" adbCommands
    toString (InconclusiveProperty s) = "inconclusive property: " ++ s
    toString (PassingProperty s)      = "passing property: "      ++ s

adb_stats :: CompTree -> IO ()
adb_stats compTree = putStrLn
  $  "======================================================================= ["
  ++ show (length vs_w) ++ "-" ++ show (length vs_r) ++ "/" ++ show (length vs) ++ "]"
  where
  vs   = filter (not . isRootVertex) (vertices compTree)
  vs_r = filter isRight vs
  vs_w = filter isWrong vs

adb_judge :: Judgement -> State -> IO (Transition State)
adb_judge jmt State{..} = case faultyVertices compTree' of
  (v:_) -> do adb_stats compTree'
              putStrLn $ "Fault located! In:\n" ++ vertexRes v
              return $ Up $ Just $ State cv trace compTree' ps
  []    -> return $ Next $ State cv_next trace compTree' ps
  where
  cv_next     = next cv' compTree'
  compTree'   = mapGraph replaceCV compTree
  replaceCV v = if vertexUID v === vertexUID cv' then cv' else v
  cv'         = setJudgement cv jmt

faultyVertices :: CompTree -> [Vertex]
faultyVertices = findFaulty_dag getJudgement

next :: Vertex -> CompTree -> Vertex
next v ct = case getJudgement v of
  Right -> up
  Wrong -> down
  _     -> v
  where
  (up:_)   = preds ct v
  (down:_) = filter unjudged (succs ct v)

unjudged :: Vertex -> Bool
unjudged = unjudged' . getJudgement
  where
  unjudged' Right = False
  unjudged' Wrong = False
  unjudged' _     = True

