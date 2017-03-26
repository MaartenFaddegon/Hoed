-- This file is part of the Haskell debugger Hoed.
--
-- Copyright (c) Maarten Faddegon, 2014-2016
{-# LANGUAGE CPP #-}module Debug.Hoed.Pure.Console(debugSession) where
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
debugSession trace traceInfo tree frt ps
  = do noBuffering
       mainLoop trace traceInfo tree frt ps

help :: IO ()
help = putStr
  $  "help              Print this help message.\n"
  ++ "observe [regexp]  Print computation statements that matching a regular\n"
  ++ "                  expression. The empty expression matches all statements.\n"
  ++ "adb               Start algorithmic debugging.\n"
  ++ "exit              leave debugging session\n"

mainLoop :: Trace -> TraceInfo -> CompTree -> EventForest -> [Propositions] -> IO ()
mainLoop trace traceInfo compTree frt ps = do
  i <- readLine "hdb> " ["adb", "observe", "help"]
  case words i of
    ["adb", n]          -> adb n trace traceInfo compTree frt ps
    ["observe", regexp] -> do printStmts compTree regexp; loop
    ["observe"]         -> do printStmts compTree ""; loop
    ["exit"]            -> return ()
    _                   -> do help; loop
  where
  loop = mainLoop trace traceInfo compTree frt ps

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

adb :: String -> Trace -> TraceInfo -> CompTree -> EventForest -> [Propositions] -> IO ()
adb n trace traceInfo compTree frt ps = do
  putStr "<statement> ? "
  i <- getLine
  case i of
    "exit" -> mainLoop trace traceInfo compTree frt ps
    _      -> adb n trace traceInfo compTree frt ps

