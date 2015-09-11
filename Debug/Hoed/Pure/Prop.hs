-- This file is part of the Haskell debugger Hoed.
--
-- Copyright (c) Maarten Faddegon, 2015

module Debug.Hoed.Pure.Prop where
-- ( judge
-- , Propositions(..)
-- ) where

import Debug.Hoed.Pure.Observe(Trace(..),UID,Event(..),Change(..))
import Debug.Hoed.Pure.Render(CompStmt(..))
import Debug.Hoed.Pure.CompTree(CompTree,Vertex(..),Graph(..),vertexUID)
import Debug.Hoed.Pure.EventForest(EventForest,mkEventForest,dfsChildren)

import Prelude hiding (Right)
import Data.Graph.Libgraph(Judgement(..),mapGraph)
import System.Directory(createDirectoryIfMissing)
import System.Process(system)
import System.Exit(ExitCode(..))
import System.IO(hPutStrLn,stderr)
import Data.Char(isAlpha)
import Data.Maybe(isNothing)
import Data.List(intersperse)

------------------------------------------------------------------------------------------------------------------------

data Propositions = Propositions {propositions :: [Proposition], propType :: PropType, funName :: String} 

data PropType     = Specify | PropertiesOf

type Proposition  = (String,Module)

data Module       = Module {moduleName :: String, searchPath :: String}

propName :: Proposition -> String
propName = fst

propModule :: Proposition -> Module
propModule = snd

------------------------------------------------------------------------------------------------------------------------

sourceFile = ".Hoed/exe/Main.hs"
buildFiles = ".Hoed/exe/Main.o .Hoed/exe/Main.hi"
exeFile    = ".Hoed/exe/Main"
outFile    = ".Hoed/exe/Main.out"

------------------------------------------------------------------------------------------------------------------------

lookupPropositions :: [Propositions] -> Vertex -> Maybe Propositions
lookupPropositions _ RootVertex = Nothing
lookupPropositions ps v = lookupWith funName lbl ps
  where lbl = (stmtLabel . vertexStmt) v

lookupWith :: Eq a => (b->a) -> a -> [b] -> Maybe b
lookupWith f x ys = case filter (\y -> f y == x) ys of
  []    -> Nothing
  (y:_) -> Just y

------------------------------------------------------------------------------------------------------------------------

judge :: Trace -> [Propositions] -> CompTree -> IO CompTree
judge trc ps compTree = do
  ws <- mapM j vs
  return $ foldl updateTree compTree ws
  where 
  vs  = vertices compTree
  j v = case lookupPropositions ps v of 
    Nothing  -> return v
    (Just p) -> judgeWithPropositions trc p v
  updateTree compTree w = mapGraph (\v -> if (vertexUID v) == (vertexUID w) then w else v) compTree


-- Use a property to judge a vertex
judgeWithPropositions :: Trace -> Propositions -> Vertex -> IO Vertex
judgeWithPropositions _ _ RootVertex = return RootVertex
judgeWithPropositions trc p v = do
  mbs <- mapM (evalProposition trc v) (propositions p)
  let j = case (propType p, any isNothing mbs) of
            (Specify,False) -> if any isJustFalse mbs then Wrong else Right
            _               -> if any isJustFalse mbs then Wrong else Unassessed
      j' = case (j, (map snd) . (filter (isJustTrue . fst)) $ zip mbs (propositions p)) of
             (Unassessed, []) -> Unassessed
             (Unassessed, ps) -> Assisted $ "With passing properties: " ++ commas (map propName ps)
             _                -> j
  hPutStrLn stderr $ "Judgement was " ++ (show . vertexJmt) v ++ ", and is now " ++ show j'
  return v{vertexJmt=j'}
  where
  isJustFalse (Just False) = True
  isJustFalse _            = False
  isJustTrue (Just True)   = True
  isJustTrue _             = False

  commas :: [String] -> String
  commas = concat . (intersperse ", ")


evalProposition :: Trace -> Vertex -> Proposition -> IO (Maybe Bool)
evalProposition trc v prop = do
  createDirectoryIfMissing True ".Hoed/exe"
  hPutStrLn stderr $ "Evaluating proposition " ++ propName prop ++ " with statement " ++ (stmtRes . vertexStmt) v
  clean
  generateCode
  compile
  exit' <- compile
  hPutStrLn stderr $ "Exitted with " ++ show exit'
  exit  <- case exit' of (ExitFailure n) -> return (ExitFailure n)
                         ExitSuccess     -> evaluate
  out  <- readFile outFile
  hPutStrLn stderr $ "Exitted with " ++ show exit
  hPutStrLn stderr $ "Output is " ++ show out
  return $ case (exit, out) of
    (ExitFailure _, _)         -> Nothing
    (ExitSuccess  , "True\n")  -> Just True
    (ExitSuccess  , "False\n") -> Just False
    (ExitSuccess  , _)         -> Nothing

    where
    clean        = system $ "rm -f " ++ sourceFile ++ " " ++ exeFile ++ " " ++ buildFiles
    generateCode = writeFile sourceFile (generate prop trc i)
    compile      = system $ "ghc  -i" ++ (searchPath . propModule) prop ++ " -o " ++ exeFile ++ " " ++ sourceFile
    evaluate     = system $ exeFile ++ " > " ++ outFile ++ " 2>&1"
    i            = (stmtIdentifier . vertexStmt) v

-- The actual logic that changes the judgement of a vertex.
judge1' :: ExitCode -> String -> Judgement -> Judgement
judge1' (ExitFailure _) _   j = j
judge1' ExitSuccess     out j
  | out == "False\n" = Wrong
  | out == "True\n"  = j
  | otherwise     = j

judge1_spec :: ExitCode -> String -> Judgement -> Judgement
judge1_spec (ExitFailure _) _   j = j
judge1_spec ExitSuccess     out j
  | out == "False\n" = Wrong
  | out == "True\n"  = Right
  | otherwise     = j

------------------------------------------------------------------------------------------------------------------------

generate :: Proposition -> Trace -> UID -> String
generate prop trc i = generateHeading prop ++ generateMain prop trc i

generateHeading :: Proposition -> String
generateHeading prop =
  "-- This file is generated by the Haskell debugger Hoed\n"
  ++ "import " ++ (moduleName . propModule) prop ++ "\n"

generateMain :: Proposition -> Trace -> UID -> String
generateMain prop trc i =
  "main = print $ " ++ propName prop ++ " " ++ generateArgs trc i ++ "\n"

generateArgs :: Trace -> UID -> String
generateArgs trc i = case dfsChildren frt e of
  [_,ma,_,_]  -> generateExpr frt ma
  xs          -> error ("generateArgs: dfsChildren (" ++ show e ++ ") = " ++ show xs)

  where frt = (mkEventForest trc)
        e   = (reverse trc) !! (i-1)

generateExpr :: EventForest -> Maybe Event -> String
generateExpr _ Nothing    = __
generateExpr frt (Just e) = -- enable to add events as comments to generated code: "{- " ++ show e ++ " -}" ++
                            case change e of
  (Cons _ s) -> let s' = if isAlpha (head s) then s else "(" ++ s ++ ")"
                in foldl (\acc c -> acc ++ " " ++ c) ("(" ++ s') cs ++ ") "
  Enter      -> ""
  _          -> "error \"cannot represent\""

  where cs = map (generateExpr frt) (dfsChildren frt e)

__ :: String
__ = "(error \"Request of value that was unevaluated in original program.\")"
