-- This file is part of the Haskell debugger Hoed.
--
-- Copyright (c) Maarten Faddegon, 2014-2017
{-# LANGUAGE CPP #-}module Debug.Hoed.Pure.Console(debugSession) where
import qualified Prelude
import Prelude hiding(Right)
import Debug.Hoed.Pure.ReadLine
import Debug.Hoed.Pure.Observe
import Debug.Hoed.Pure.Render
import Debug.Hoed.Pure.EventForest
import Debug.Hoed.Pure.CompTree
import Debug.Hoed.Pure.Prop
import Debug.Hoed.Pure.Serialize
import Text.Regex.Posix.String as Regex
import Text.Regex.Posix
import Text.Regex.Posix.String
import Data.Graph.Libgraph
import Data.List(findIndex,intersperse,nub,sort,sortBy
#if __GLASGOW_HASKELL__ >= 710
                , sortOn
#endif
                )

#if __GLASGOW_HASKELL__ < 710
sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f  = map snd . sortOn' fst .  map (\x -> (f x, x))

sortOn' :: Ord b => (a -> b) -> [a] -> [a]
sortOn' f = sortBy (\x y -> compare (f x) (f y))
#endif


debugSession :: Trace -> TraceInfo -> CompTree -> EventForest -> [Propositions] -> IO ()
debugSession trace traceInfo tree frt ps =
  case filter (not . isRootVertex) $ vs of 
    []    -> putStrLn $ "No functions annotated with 'observe' expressions"
                        ++ "or annotated functions not evaluated"
    (v:_) -> do noBuffering
                mainLoop v trace traceInfo tree frt ps
  where
  (Graph _ vs _) = tree

--------------------------------------------------------------------------------
-- main menu

mainLoop :: Vertex -> Trace -> TraceInfo -> CompTree -> EventForest -> [Propositions] -> IO ()
mainLoop cv trace traceInfo compTree frt ps = do
  i <- readLine "hdb> " ["adb", "observe", "help"]
  case words i of
    ["adb"]             -> adb cv trace traceInfo compTree frt ps
    ["observe", regexp] -> do printStmts compTree regexp; loop
    ["observe"]         -> do printStmts compTree ""; loop
    ["exit"]            -> return ()
    _                   -> do help; loop
  where
  loop = mainLoop cv trace traceInfo compTree frt ps

help :: IO ()
help = putStr
  $  "help              Print this help message.\n"
  ++ "observe [regexp]  Print computation statements that match the regular\n"
  ++ "                  expression. Omitting the expression prints all statements.\n"
  ++ "adb               Start algorithmic debugging.\n"
  ++ "exit              leave debugging session\n"

--------------------------------------------------------------------------------
-- observe

printStmts :: CompTree -> String -> IO ()
printStmts (Graph _ vs _) regexp = do
  rComp <- Regex.compile defaultCompOpt defaultExecOpt regexp
  case rComp of Prelude.Left  (_, errorMessage) -> printL errorMessage
                Prelude.Right _                 -> printR
  where
  printL errorMessage = putStrLn errorMessage
  printR
    | vs_filtered == []  = printL $ "There are no computation statements matching \"" ++ regexp ++ "\"."
    | otherwise          = printStmts' vs_filtered
  vs_filtered
    | regexp == "" = vs_sorted
    | otherwise    = filter (\v -> (noNewlines . vertexRes $ v) =~ regexp) vs_sorted
  vs_sorted = sortOn (vertexRes) . filter (not . isRootVertex) $ vs

printStmts' :: [Vertex] -> IO ()
printStmts' vs = do
  mapM_ print (zip [1..] vs)
  putStrLn "--------------------------------------------------------------------"
  where
  print (n,v) = do 
    putStrLn $ "--- stmt-" ++ show n ++ " ------------------------------------------"
    (putStrLn . show . vertexStmt) v

--------------------------------------------------------------------------------
-- algorithmic debugging

adb :: Vertex -> Trace -> TraceInfo -> CompTree -> EventForest -> [Propositions] -> IO ()
adb cv trace traceInfo compTree frt ps = do
  print $ vertexStmt cv
  i <- readLine "? " ["right", "wrong", "prop", "exit"]
  case i of
    "right" -> adb_judge cv Right trace traceInfo compTree frt ps
    "wrong" -> adb_judge cv Wrong trace traceInfo compTree frt ps
    "exit"  -> mainLoop cv trace traceInfo compTree frt ps
    _       -> adb cv trace traceInfo compTree frt ps


adb_judge :: Vertex -> Judgement -> Trace -> TraceInfo -> CompTree -> EventForest -> [Propositions] -> IO ()
adb_judge cv jmt trace traceInfo compTree frt ps = case faultyVertices compTree' of
  (v:_) -> do putStrLn $ " Fault detected in: " ++ vertexRes v
              mainLoop cv trace traceInfo compTree' frt ps
  []    -> adb cv'' trace traceInfo compTree' frt ps
  where
  compTree'   = mapGraph replaceCV compTree'
  replaceCV v = if vertexUID v === vertexUID cv' then cv' else v
  cv'         = setJudgement cv jmt
  cv''        = next cv' compTree'

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

