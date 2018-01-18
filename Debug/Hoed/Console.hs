{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE PatternSynonyms       #-}
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
import           Control.Arrow (first, second)
import           Data.Char
import           Data.Foldable            as F
import           Data.Graph.Libgraph      as G
import           Data.List                as List (foldl', group, mapAccumL, nub, sort)
import qualified Data.Map.Strict          as Map
import qualified Data.IntMap.Strict       as IntMap
import           Data.Maybe
import           Data.Sequence            (Seq, ViewL (..), viewl, (<|))
import qualified Data.Sequence            as Seq
import qualified Data.Set                 as Set
import           Data.Text (Text, unpack)
import qualified Data.Text                as T
import qualified Data.Text.IO             as T
import qualified Data.Vector              as V
import qualified Data.Vector.Generic      as VG
import           Data.Word
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
import           Text.PrettyPrint.FPretty hiding ((<$>))
import           Text.Regex.TDFA
import           Text.Regex.TDFA.Text
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
traceToSpans :: (UID -> EventWithId) -> Trace -> Spans
traceToSpans lookupEvent =
  map sort .
  Map.elems .
  Map.fromListWith (++) .
  map (second (: [])) . snd . VG.ifoldl' (traceToSpans2' lookupEvent) ([], [])

-- This version tries to distinguish between Fun events that terminate a span,
-- and Fun events that signal a second application of a function
traceToSpans2'
  :: (UID -> EventWithId)
  -> (Seq Id, [(Depth, Span)])
  -> UID
  -> Event
  -> (Seq Id, [(Depth, Span)])
traceToSpans2' lookupEv (stack, result) uid e
  | isStart (change e) = (uid <| stack, result)
  | start :< stack' <- viewl stack
  , isEnd lookupEv uid e =
    ( stack'
    , (Seq.length stack', Span start uid (getPolarity e)) : result)
  | otherwise = (stack, result)
  where
    isStart Enter {} = True
    isStart _ = False
    getPolarity Event {change = Observe {}} = True
    getPolarity Event {eventParent = Parent p 0}
      | Event {change = Fun} <- event $ lookupEv p =
        not $ getPolarity (event $ lookupEv p)
    getPolarity Event {eventParent = Parent p _} = getPolarity (event $ lookupEv p)

isEnd lookupEv _ Event {change = Cons {}} = True
isEnd lookupEv uid me@(Event {change = Fun {}}) =
  case prevEv of
    Event{change = Enter{}} -> eventParent prevEv == eventParent me
    _ -> False
  where
    prevEv= event $ lookupEv (uid - 1)
isEnd _ _ _ = False

printTrace :: Trace -> IO ()
printTrace trace =
  putStrLn $
  renderTrace' lookupEvent lookupDescs (traceToSpans lookupEvent trace, trace)
    -- fast lookup via an array
  where
    lookupEvent i = EventWithId i (trace VG.! i)
    lookupDescs =
      (fromMaybe [] .
       (`IntMap.lookup` (IntMap.fromListWith
                           (++)
                           [ (p, [EventWithId uid e])
                           | (uid, e@Event {eventParent = Parent p _}) <- toList (VG.indexed trace)
                           ])))

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

renderTrace' :: (UID -> EventWithId)
             -> (UID -> [EventWithId])
             -> (Spans, Trace)
             -> String
renderTrace' lookupEvent lookupDescs (columns, events) = unlines renderedLines
    -- roll :: State -> (Event, Maybe ColumnEvent) -> (State, String)
  where
    depth = length columns
    ((_, evWidth), renderedLines) =
      mapAccumL roll (replicate (depth + 1) ' ', 0) $ align (uncurry EventWithId <$> toList (VG.indexed events)) columnEvents
    -- Merge trace events and column events
    align (ev:evs) (colEv@(rowIx, colIx, pol, isStart):colEvs)
      | eventUID ev == rowIx = (ev, Just (colIx, pol, isStart)) : align evs colEvs
      | otherwise = (ev, Nothing) : align evs (colEv : colEvs)
    align [] [] = []
    align ev [] = map  (\x -> (x, Nothing)) ev
    -- Produce the output in three columns: spans, events, and explains
    -- For spans: keep a state of the open spans
    --   (the state is the rendering in characters)
    -- For events: keep a state of the widest event
    -- For explains: circularly reuse the widest event result
    roll (state, width) (ev, Nothing)
      | (w, s) <- showWithExplains ev = ((state, max width w), state ++ s)
    roll (state, width) (ev, Just (col, pol, True))
      | state' <- update state col '|'
      , state'' <- update state col (if pol then '↑' else '┬')
      , (w, s) <- showWithExplains ev = ((state', max width w), state'' ++ s)
    roll (state, width) (ev, Just (col, pol, False))
      | state' <- update state col ' '
      , state'' <- update state col (if pol then '↓' else '┴')
      , (w, s) <- showWithExplains ev = ((state', max width w), state'' ++ s)
    -- columnEvents :: [(Line,ColIndex,StartOrEnd)]
    -- view the column spans as an event stream
    -- there's an event when a span starts or ends
    columnEvents =
      sortOn
        (\(a, b, c, d) -> a)
        [ (rowIx, colIx, pol, isStart)
        | (colIx, spans) <- zip [0 ..] columns
        , Span {..} <- spans
        , (rowIx, pol, isStart) <- [(spanStart, polarity, True), (spanEnd, polarity, False)]
        ]
    -- update element n of a list with a new value v
    update [] _ _ = []
    update (_:xs) 0 v = v : xs
    update (x:xs) n v = x : update xs (n - 1) v
    -- show the event, add the necessary padding to fill the event col width,
    -- and append the explain.
    showWithExplains ev
      | showEv <- show ev
      , l <- length showEv =
        (l, showEv ++ replicate (evWidth - l) ' ' ++ explain (eventUID ev) (event ev))
    -- Value requests
    explain uid Event {eventParent = Parent p 0, change = Enter}
      | Event {change = Fun, eventParent = Parent p' _} <- event $ lookupEvent p
      , (name,dist) <- findRoot (event $ lookupEvent p') =
        "-- request arg of " ++ unpack name ++ "/" ++ show (dist + 1)
    explain uid Event {eventParent = Parent p 1, change = Enter}
      | Event {change = Fun, eventParent = Parent p' _} <- event $ lookupEvent p
      , (name,dist) <- findRoot (event $ lookupEvent p') =
        "-- request result of " ++ unpack name ++ "/" ++ show (dist+1)
    explain uid Event {eventParent = Parent p 0, change = Enter}
      | Event {change = Observe name} <- event $ lookupEvent p =
        "-- request value of " ++ unpack name
    explain uid Event {eventParent = Parent p i, change = Enter}
      | Event {change = Cons ar name} <- event $ lookupEvent p =
        "-- request value of arg " ++ show i ++ " of constructor " ++ unpack name
    -- Arguments of functions
    explain uid me@Event {eventParent = Parent p 0, change = it@FunOrCons}
      | Event {change = Fun, eventParent = Parent p' _} <- event $ lookupEvent p
      , (name,dist) <- findRoot (event $ lookupEvent p') =
        "-- arg " ++ show (dist+1) ++ " of " ++ unpack name ++ " is " ++ showChange it
    -- Results of functions
    explain uid Event {eventParent = Parent p 1, change = it@Fun}
      | Event {change = Fun, eventParent = Parent p' _} <- event $ lookupEvent p
      , (name,dist) <- findRoot (event $ lookupEvent p') =
        "-- result of " ++ unpack name ++ "/" ++ show (dist+1) ++ " is a function"
    explain uid me@Event {eventParent = Parent p 1, change = Cons{}}
      | Event {change = Fun, eventParent = Parent p' _} <- event $ lookupEvent p
      , (name,dist) <- findRoot (event $ lookupEvent p')
      , arg <- findArg p =
        "-- " ++ unpack name ++ "/" ++ show (dist+1) ++ " " ++ arg ++" = " ++ findValue lookupDescs (EventWithId uid me)
    -- Descendants of Cons events
    explain uid Event {eventParent = Parent p i, change = Cons _ name}
      | Event {change = Cons ar name'} <- event $ lookupEvent p =
        "-- arg " ++ show i ++ " of constructor " ++ unpack name' ++ " is " ++ unpack name
    -- Descendants of root events
    explain uid Event {eventParent = Parent p i, change = Fun}
      | Event {change = Observe name} <- event $ lookupEvent p =
        "-- " ++ unpack name ++ " is a function"
    explain uid me@Event {eventParent = Parent p i, change = Cons{}}
      | Event {change = Observe name} <- event $ lookupEvent p =
        "-- " ++ unpack name ++ " = " ++ findValue lookupDescs (EventWithId uid me)
    explain _ _ = ""

    -- Returns the root observation for this event, together with the distance to it
    findRoot Event{change = Observe name} = (name, 0)
    findRoot Event{eventParent} = succ <$> findRoot (event $ lookupEvent $ parentUID eventParent)

    variableNames = map (:[]) ['a'..'z']

    showChange Fun = "a function"
    showChange (Cons ar name) = "constructor " ++ unpack name

    findArg eventUID =
      case [ e | e@(event -> Event{eventParent = Parent p 0, change = Cons{}}) <- lookupDescs eventUID] of
        [cons] -> findValue lookupDescs cons
        other  -> error $ "Unexpected set of descendants of " ++ show eventUID ++ ": Fun - " ++ show other

findValue :: (UID -> [EventWithId]) -> EventWithId -> String
findValue lookupDescs = go
  where
    go :: EventWithId -> String
    go EventWithId {eventUID = me, event = Event {change = ConsChar c}} = show c
    go EventWithId {eventUID = me, event = Event {change = Cons ar name}}
      | ar == 0 = unpack name
      | isAlpha (T.head name) =
        unpack name ++
        " " ++
        unwords
          (map go $
           sortOn
             (parentPosition . eventParent . event)
             [e | e@EventWithId {event = Event {change = Cons {}}} <- lookupDescs me])
      | ar == 1
      , [a] <- [e | e@EventWithId {event = Event {change = Cons {}}} <- lookupDescs me] = unpack name ++ go a
      | ar == 2
      , [a, b] <- sortOn (parentPosition . eventParent . event) [e | e@(event -> Event {change = Cons {}}) <- lookupDescs me] =
        unwords [go a, unpack name, go b]
    go EventWithId {eventUID, event = Event {change = Enter {}}}
      | [e] <- lookupDescs eventUID = go e
    go other = error $ show other

data RequestDetails = RD Int Explanation

data ReturnDetails
  = ReturnFun
  | ReturnCons { constructor :: Text, arity :: Word8, value :: String}

data Explanation
  = Observation String
  | Request RequestDetails
  | Return RequestDetails ReturnDetails

instance Show Explanation where
  show (Observation obs) = ""
  show (Request r) = "request " ++ showRequest r
  show (Return r val) = showReturn r val

showRequest (RD 0 (Observation name)) = unwords ["value of", name]
showRequest (RD 0 (Return (RD _ (Observation name)) ReturnFun)) = unwords ["arg of", name]
showRequest (RD 1 (Return (RD _ (Observation name)) ReturnFun)) = unwords ["result of", name]
showRequest (RD n (Return _ (ReturnCons name ar _))) = unwords ["arg", show n, "of constructor", unpack name ]

showReturn (RD p (Observation obs)) ReturnFun = unwords ["result of ", obs, "is a function"]
showReturn (RD p req) (ReturnCons name ar val) = unwords [show req, "=", val]

buildExplanation :: (UID -> EventWithId) -> (UID -> [EventWithId]) -> EventWithId -> Explanation
buildExplanation lookupEvent lookupDescs = go . event where
  go Event{eventParent = Parent p pos, change = Enter}
    | par <- go (event $ lookupEvent p)
    = Request (RD 0 par)
  go Event{eventParent = Parent p pos, change = Fun}
    | Request rd <- go (event $ lookupEvent p)
    = Return rd ReturnFun
  go Event{eventParent = Parent p pos, change = Cons ar name}
    | Request rd <- go (event $ lookupEvent p)
    , value <- findValue lookupDescs (lookupEvent p)
    = Return rd (ReturnCons name ar value)


eitherFunOrCons Fun{} = True
eitherFunOrCons Cons {} = True
eitherFunOrCons _ = False

pattern FunOrCons <- (eitherFunOrCons -> True)

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
  T.putStrLn $
  T.unlines $
  snub $
  map (stmtLabel . vertexStmt . G.root) $
  selectVertices (\v -> matchLabel v && isRelevantToUser g v) g
  where
    matchLabel RootVertex = False
    matchLabel v          = match regex (unpack $ stmtLabel $ vertexStmt v)
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
        map (("    " ++) . unpack) locals
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

