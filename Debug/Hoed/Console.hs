{-# LANGUAGE ScopedTypeVariables #-}
-- This file is part of the Haskell debugger Hoed.
--
-- Copyright (c) Maarten Faddegon, 2014-2017
{-# LANGUAGE CPP #-}
module Debug.Hoed.Console(debugSession) where
import           Data.Graph.Libgraph
import           Data.List            (findIndex, intersperse, nub, sort,
                                       sortBy, sortOn)
import           Debug.Hoed.CompTree
import           Debug.Hoed.Observe
import           Debug.Hoed.Prop
import           Debug.Hoed.ReadLine
import           Debug.Hoed.Render
import           Debug.Hoed.Serialize
import           Prelude              hiding (Right)
import qualified Prelude
import           Text.Regex.TDFA


{-# ANN module "HLint: ignore Use camelCase" #-}

#if __GLASGOW_HASKELL__ < 710
sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f  = map snd . sortOn' fst .  map (\x -> (f x, x))

sortOn' :: Ord b => (a -> b) -> [a] -> [a]
sortOn' f = sortBy (\x y -> compare (f x) (f y))
#endif


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
-- main menu

mainLoop :: Vertex -> Trace -> CompTree -> [Propositions] -> IO ()
mainLoop cv trace compTree ps = do
  i <- readLine "hdb> " ["adb", "observe", "help"]
  case words i of
    ["adb"]             -> adb cv trace compTree ps
    ["observe", regexp] -> do printStmts compTree regexp; loop
    ["observe"]         -> do printStmts compTree ".*"; loop
    ["exit"]            -> return ()
    _                   -> do help; loop
  where
  loop = mainLoop cv trace compTree ps

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
printStmts (Graph _ vs _) regexp
    | null vs_filtered  =
      putStrLn $ "There are no computation statements matching \"" ++ regexp ++ "\"."
    | otherwise         =
      printStmts' vs_filtered
  where
  rComp :: Regex = makeRegex regexp
  vs_filtered
    | regexp == "" = vs_sorted
    | otherwise    = filter (\v -> (noNewlines . vertexRes $ v) =~ rComp) vs_sorted
  vs_sorted = sortOn vertexRes . filter (not . isRootVertex) $ vs

  str =~ reg = match reg str

printStmts' :: [Vertex] -> IO ()
printStmts' vs = do
  mapM_ outp (zip [1..] vs)
  putStrLn "--------------------------------------------------------------------"
  where
  outp (n,v) = do
    putStrLn $ "--- stmt-" ++ show n ++ " ------------------------------------------"
    (print . vertexStmt) v

--------------------------------------------------------------------------------
-- algorithmic debugging

adb :: Vertex -> Trace -> CompTree -> [Propositions] -> IO ()
adb cv trace compTree ps = do
  adb_stats compTree
  print $ vertexStmt cv
  case lookupPropositions ps cv of
    Nothing     -> adb_interactive cv trace compTree ps
    (Just prop) -> do
      judgement <- judge trace prop cv unjudgedCharacterCount compTree
      case judgement of
        (Judge Right)                    -> adb_judge cv Right trace compTree ps
        (Judge Wrong)                    -> adb_judge cv Wrong trace compTree ps
        (Judge (Assisted msgs))          -> adb_advice msgs cv trace compTree ps
        (AlternativeTree newCompTree newTrace) -> do
           putStrLn "Discovered simpler tree!"
           let cv' = next RootVertex newCompTree
           adb cv' newTrace newCompTree ps

adb_advice msgs cv trace compTree ps = do
  mapM_ (putStrLn . toString) msgs
  adb_interactive cv trace compTree ps
    where
    toString (InconclusiveProperty s) = "inconclusive property: " ++ s
    toString (PassingProperty s)      = "passing property: "      ++ s

adb_interactive cv trace compTree ps = do
  i <- readLine "? " ["right", "wrong", "prop", "exit"]
  case i of
    "right" -> adb_judge cv Right trace compTree ps
    "wrong" -> adb_judge cv Wrong trace compTree ps
    "exit"  -> mainLoop cv trace compTree ps
    _       -> do adb_help
                  adb cv trace compTree ps



adb_help :: IO ()
adb_help = putStr
  $  "help              Print this help message.\n"
  ++ "right             Judge computation statements right according to the\n"
  ++ "                  intentioned behaviour/specification of the function\n"
  ++ "wrong             Judge computation statements wrong according to the\n"
  ++ "                  intentioned behaviour/specification of the function\n"
  ++ "exit              Return to main menu\n"

adb_stats :: CompTree -> IO ()
adb_stats compTree = putStrLn
  $  "======================================================================= ["
  ++ show (length vs_w) ++ "-" ++ show (length vs_r) ++ "/" ++ show (length vs) ++ "]"
  where
  vs   = filter (not . isRootVertex) (vertices compTree)
  vs_r = filter isRight vs
  vs_w = filter isWrong vs


adb_judge :: Vertex -> Judgement -> Trace -> CompTree -> [Propositions] -> IO ()
adb_judge cv jmt trace compTree ps = case faultyVertices compTree' of
  (v:_) -> do adb_stats compTree'
              putStrLn $ "Fault located! In:\n" ++ vertexRes v
              mainLoop cv trace compTree' ps
  []    -> adb cv_next trace compTree' ps
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




