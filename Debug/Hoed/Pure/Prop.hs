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
import qualified Data.IntMap as M

import Prelude hiding (Right)
import Data.Graph.Libgraph(Judgement(..),mapGraph)
import System.Directory(createDirectoryIfMissing)
import System.Process(system)
import System.Exit(ExitCode(..))
import System.IO(hPutStrLn,stderr)
import Data.Char(isAlpha)
import Data.Maybe(isNothing,fromJust)
import Data.List(intersperse)

------------------------------------------------------------------------------------------------------------------------

data Propositions = Propositions { propositions :: [Proposition], propType :: PropType, funName :: String
                                 , extraModules :: [Module]
                                 } 

data PropType     = Specify | PropertiesOf

type Proposition  = (PropositionType,Module,String,[Int])

data PropositionType = BoolProposition | QuickCheckProposition

data Module       = Module {moduleName :: String, searchPath :: String}

propositionType :: Proposition -> PropositionType
propositionType (x,_,_,_) = x

propName :: Proposition -> String
propName (_,_,x,_) = x

argMap :: Proposition -> [Int]
argMap (_,_,_,x) = x

propModule :: Proposition -> Module
propModule (_,x,_,_) = x

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
  mbs <- mapM (evalProposition trc v (extraModules p)) (propositions p)
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


evalProposition :: Trace -> Vertex -> [Module] -> Proposition -> IO (Maybe Bool)
evalProposition trc v ms prop = do
  createDirectoryIfMissing True ".Hoed/exe"
  hPutStrLn stderr $ "Evaluating proposition " ++ propName prop ++ " with statement " ++ (stmtRes . vertexStmt) v

  let args = map (\(n,s) -> "Argument " ++ show n ++ ": " ++ (shorten s)) $ zip [0..] (generateArgs trc getEvent i)
      shorten s
        | length s < 120 = s
        | otherwise    = (take 117 s) ++ "..."
  hPutStrLn stderr $ "Statement UID = " ++ show i
  mapM (hPutStrLn stderr) args

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
    generateCode = do hPutStrLn stderr $ "Generated the following program ***\n" ++ prgm ++ "\n***" 
                      writeFile sourceFile prgm
                      where prgm :: String
                            prgm = (generate prop ms trc getEvent i)
    compile      = system $ "ghc  -i" ++ (searchPath . propModule) prop ++ " -o " ++ exeFile ++ " " ++ sourceFile
    evaluate     = system $ exeFile ++ " > " ++ outFile ++ " 2>&1"
    i            = (stmtIdentifier . vertexStmt) v

    getEvent :: UID -> Event
    getEvent j = fromJust $ M.lookup j m
      where m = M.fromList $ map (\e -> (eventUID e, e)) trc

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

generate :: Proposition -> [Module] -> Trace -> (UID->Event) -> UID -> String
generate prop ms trc getEvent i = generateHeading prop ms ++ generateMain prop trc getEvent i

generateHeading :: Proposition -> [Module] -> String
generateHeading prop ms =
  "-- This file is generated by the Haskell debugger Hoed\n"
  ++ generateImport (propModule prop)
  ++ foldl (\acc m -> acc ++ generateImport m) "" ms

generateImport :: Module -> String
generateImport m =  "import " ++ (moduleName m) ++ "\n"

generateMain :: Proposition -> Trace -> (UID->Event) -> UID -> String
generateMain prop trc getEvent i =
  foldl (\acc x -> acc ++ " " ++ getArg x) ("main = " ++ generatePrint prop ++ " $ " ++ propName prop ++ " ") (reverse . argMap $ prop) ++ "\n"
  where 
  getArg :: Int -> String
  getArg x
    | x < (length args) = args !! x
    | otherwise         = __

  args :: [String]
  args = generateArgs trc getEvent i

generatePrint :: Proposition -> String
generatePrint p = case propositionType p of
  BoolProposition       -> "print"
  QuickCheckProposition -> "do g <- newStdGen; print . fromJust . ok . (generate 1 g) . evaluate"

generateArgs :: Trace -> (UID -> Event) -> UID -> [String]
generateArgs trc getEvent i = case dfsChildren frt e of
  [_,ma,_,mr]  -> generateExpr frt ma : moreArgs trc getEvent mr
  xs           -> error ("generateArgs: dfsChildren (" ++ show e ++ ") = " ++ show xs)

  where frt = (mkEventForest trc)
        e   = getEvent i -- (reverse trc) !! (i-1)

moreArgs :: Trace -> (UID->Event) -> Maybe Event -> [String]
moreArgs trc getEvent Nothing = []
moreArgs trc getEvent (Just e)
  | change e == Fun = generateArgs trc getEvent (eventUID e)
  | otherwise       = []

generateExpr :: EventForest -> Maybe Event -> String
generateExpr _ Nothing    = __
generateExpr frt (Just e) = -- uncomment next line to add events as comments to generated code: 
                            -- "{- " ++ show e ++ " -}" ++
                            case change e of
  (Cons _ s) -> let s' = if isAlpha (head s) then s else "(" ++ s ++ ")"
                in foldl (\acc c -> acc ++ " " ++ c) ("(" ++ s') cs ++ ") "
  Enter      -> ""
  _          -> "error \"cannot represent\""

  where cs = map (generateExpr frt) (dfsChildren frt e)

__ :: String
__ = "(error \"Request of value that was unevaluated in original program.\")"
