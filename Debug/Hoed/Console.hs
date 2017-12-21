{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
-- This file is part of the Haskell debugger Hoed.
--
-- Copyright (c) Maarten Faddegon, 2014-2017
module Debug.Hoed.Console(debugSession) where

import           Control.Monad
import           Data.Graph.Libgraph      as G
import           Data.List                as List (findIndex, group,
                                                   intersperse, nub, sort,
                                                   sortBy)
import qualified Data.Map.Strict          as Map
import           Data.Maybe
import qualified Data.Set                 as Set
import           Debug.Hoed.Compat
import           Debug.Hoed.CompTree
import           Debug.Hoed.Observe
import           Debug.Hoed.Prop
import           Debug.Hoed.ReadLine
import           Debug.Hoed.Render
import           Debug.Hoed.Serialize
import           Prelude                  hiding (Right)
import qualified Prelude
import           Text.PrettyPrint.FPretty
import           Text.Regex.TDFA


{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

debugSession :: Trace -> CompTree -> [Propositions] -> IO ()
debugSession trace tree ps =
  case filter (not . isRootVertex) vs of
    []    -> putStrLn $ "No functions annotated with 'observe' expressions"
                        ++ "or annotated functions not evaluated"
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

showHelp commands =
  putStrLn (pretty 80 $ vcat $ zipWith compose commandsBlock descriptionsBlock)
  where
    compose c d = text (pad c) <+> align d
    commandsBlock = [unwords (name : argsDesc) | Command {..} <- commands]
    descriptionsBlock = map commandDesc commands
    colWidth = maximum $ map length commandsBlock
    pad x = take (colWidth + 1) $ x ++ spaces
    spaces = repeat ' '

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

adbCommand, observeCommand, listCommand, exitCommand :: Command State
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
    [] -> Just $ \State {..} -> Same <$ printStmts compTree ".*"
    regexp -> Just $ \State {..} -> Same <$ printStmts compTree (unwords regexp)

listCommand =
  Command "list" [] "List all the observables collected." $ \case
    [] -> Just $ \State{..} -> Same <$ listStmts compTree
    _ -> Nothing

exitCommand =
  Command "exit" [] "Leave the debugging session." $ \case
    [] -> Just $ \_ -> return (Up Nothing)
    _  -> Nothing

mainLoopCommands =
  sortOn name
    [ adbCommand
    , listCommand
    , observeCommand
    , exitCommand
    , helpCommand mainLoopCommands
    ]

mainLoop cv trace compTree ps =
  executionLoop [interactiveFrame "hdb>" mainLoopCommands] $
  State cv trace compTree ps

--------------------------------------------------------------------------------
-- observe

listStmts :: CompTree -> IO ()
listStmts (Graph _ vs _) =
  putStrLn $
  unlines $
  snub
    [ stmtLabel v
    | Vertex {vertexStmt = v@CompStmt {stmtDetails = StmtLam {}}} <- vs
    ]
  where
    snub = map head . List.group . sort

printStmts :: CompTree -> String -> IO ()
printStmts g regexp
    | null vs_filtered  =
      putStrLn $ "There are no computation statements matching \"" ++ regexp ++ "\"."
    | otherwise         = printStmts' vs_filtered
  where
  vs_filtered = nodesMatching regexp g

nodesMatching :: String -> CompTree -> [CompTree]
nodesMatching regexp g@(Graph _ vs _) = map (`subGraphFrom` g) vsFiltered
  where
    vsFiltered
      | regexp == "" = vs_sorted
      | otherwise =
        filter (\v -> (noNewlines . vertexRes $ v) =~ rComp) vs_sorted
    rComp :: Regex = makeRegex regexp
    vs_sorted = sortOn vertexRes . filter vertexOfInterest $ vs
    -- Restricted to statements for lambda functions or top level constants.
    vertexOfInterest Vertex{vertexStmt=CompStmt{stmtDetails=StmtLam{}}} = True
    vertexOfInterest v@Vertex{vertexStmt=CompStmt{stmtDetails=StmtCon{}}} =
      RootVertex `elem` concatMap (preds g) (preds g v)
    vertexOfInterest _ = False
    str =~ reg = match reg str

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

printStmts' :: [CompTree] -> IO ()
printStmts' gs = do
  forM_ (zip [1 ..] gs) $ \(n, g) -> do
    putStrLn $
      "--- stmt-" ++ show n ++ " ------------------------------------------"
    (print . vertexStmt) (G.root g)
    let locals =
          [ (stmtLabel, c)
          | Vertex {vertexStmt = CompStmt {stmtDetails = StmtCon c, stmtLabel}} <-
              succs g (G.root g)
          ]
    unless (null locals) $ do
      putStrLn "  where"
      forM_ (succs g (G.root g)) $ \v ->
        case v of
          Vertex {vertexStmt = CompStmt {stmtDetails = StmtCon c, stmtLabel}} ->
            putStrLn ("    " ++ stmtLabel ++ " = " ++ c)
          _ -> return ()
  putStrLn
    "--------------------------------------------------------------------"

--------------------------------------------------------------------------------
-- algorithmic debugging

adbCommands = [judgeCommand Right, judgeCommand Wrong]

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





