{-# LANGUAGE CPP                       #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- This file is part of the Haskell debugger Hoed.
--
-- Copyright (c) Maarten Faddegon, 2014-2017
module Debug.Hoed.Console(debugSession) where

import           Control.Monad
import           Data.Graph.Libgraph
import           Data.List            (findIndex, intersperse, nub, sort,
                                       sortBy, sortOn)
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Debug.Hoed.CompTree
import           Debug.Hoed.Observe
import           Debug.Hoed.Prop
import           Debug.Hoed.ReadLine
import           Debug.Hoed.Render
import           Debug.Hoed.Serialize
import           Prelude              hiding (Right)
import qualified Prelude
import           Text.PrettyPrint.FPretty
import           Text.Regex.TDFA


{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

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

type Args = [String]

data Run = Run
  { cv        :: Vertex
  , trace     :: Trace
  , compTree  :: CompTree
  , ps        :: [Propositions]
  }

data Command = Command
  { name        :: String
  , argsDesc    :: [String]
  , commandDesc :: Doc
  , parse       :: Args -> Maybe (Run -> IO Bool)
  }

adbCommand =
  Command "adb" [] "Start algorithmic debugging." $ \case
    [] -> Just $ \Run {..} -> False <$ adb cv trace compTree ps
    _  -> Nothing

observeCommand =
  Command
    "observe"
    ["[regexp]"]
    ("Print computation statements that match the regular expression." </>
     "Omitting the expression prints all the statements.") $ \case
    [] -> Just $ \Run {..} -> True <$ printStmts compTree ".*"
    [regexp] -> Just $ \Run {..} -> True <$ printStmts compTree regexp
    _ -> Nothing

exitCommand =
  Command "exit" [] "Leave the debugging session." $ \case
    [] -> Just $ \_ -> pure False
    _  -> Nothing

helpCommand commands =
  Command "help" [] "Shows this help screen." $ \case
    [] -> Just $ \_ -> True <$ showHelp commands
    _  -> Nothing

allCommands = [adbCommand, observeCommand, exitCommand, helpCommand allCommands]

showHelp commands =
  putStrLn (pretty 80 $ vcat $ zipWith compose commandsBlock descriptionsBlock)
  where
    compose c d = text (pad c) <+> align d
    commandsBlock = [unwords (name : argsDesc) | Command {..} <- commands]
    descriptionsBlock = map commandDesc commands
    colWidth = maximum $ map length commandsBlock
    pad x = take (colWidth + 1) $ x ++ spaces
    spaces = repeat ' '

selectFrom :: [Command] -> String -> Maybe (Run -> IO Bool)
selectFrom commands =
  \case
    "" -> Nothing
    xx -> do
      let (h:t) = words xx
      c <- Map.lookup h commandsMap
      parse c t
  where
    commandsMap = Map.fromList [(name c, c) | c <- commands]

mainLoop :: Vertex -> Trace -> CompTree -> [Propositions] -> IO ()
mainLoop cv trace compTree ps = do
  i <- readLine "hdb> " (map name allCommands)
  let run = fromMaybe (\_ -> True <$ showHelp allCommands) $ selectFromAllCommands i
  repeat <- run runEnv
  when repeat loop
  where
    runEnv = Run cv trace compTree ps
    selectFromAllCommands = selectFrom allCommands
    loop = mainLoop cv trace compTree ps

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





