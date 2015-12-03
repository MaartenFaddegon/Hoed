-- This file is part of the Haskell debugger Hoed.
--
-- Copyright (c) Maarten Faddegon, 2015

{-# LANGUAGE DefaultSignatures, TypeOperators, FlexibleContexts, FlexibleInstances, StandaloneDeriving, CPP #-}

module Debug.Hoed.Pure.Prop where
-- ( judge
-- , Propositions(..)
-- ) where
import Debug.Hoed.Pure.Observe(Trace(..),UID,Event(..),Change(..),ourCatchAllIO,evaluate)
import Debug.Hoed.Pure.Render(CompStmt(..),noNewlines)
import Debug.Hoed.Pure.CompTree(CompTree,Vertex(..),Graph(..),vertexUID)
import Debug.Hoed.Pure.EventForest(EventForest,mkEventForest,dfsChildren)
import qualified Data.IntMap as M
import Prelude hiding (Right)
import Data.Graph.Libgraph(Judgement(..),mapGraph)
import System.Directory(createDirectoryIfMissing)
import System.Process(system)
import System.Exit(ExitCode(..))
import System.IO(hPutStrLn,stderr)
import System.IO.Unsafe(unsafePerformIO)
import Data.Char(isAlpha)
import Data.Maybe(isNothing,fromJust)
import Data.List(intersperse)
import GHC.Generics hiding (moduleName) --(Generic(..),Rep(..),from,(:+:)(..),(:*:)(..),U1(..),K1(..),M1(..))

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
errFile    = ".Hoed/exe/Main.compilerMessages"

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

judge :: PropVarGen String -> Trace -> [Propositions] -> CompTree -> IO CompTree
judge unevalGen trc ps compTree = do
  ws <- mapM j vs
  return $ foldl updateTree compTree ws
  where 
  vs  = vertices compTree
  j v = case lookupPropositions ps v of 
    Nothing  -> return v
    (Just p) -> judgeWithPropositions unevalGen trc p v
  updateTree compTree w = mapGraph (\v -> if (vertexUID v) == (vertexUID w) then w else v) compTree

-- Use a property to judge a vertex
judgeWithPropositions :: PropVarGen String -> Trace -> Propositions -> Vertex -> IO Vertex
judgeWithPropositions unevalGen _ _ RootVertex = return RootVertex
judgeWithPropositions unevalGen trc p v = do
  pas <- mapM (evalProposition unevalGen trc v (extraModules p)) (propositions p)
  let j = case (propType p, all hasResult pas) of
            (Specify,True) -> if any disproves pas then Wrong else Right
            _              -> if any disproves pas then Wrong else Unassessed
      j' = case (j, (map snd) . (filter (holds . fst)) $ zip pas (propositions p)) of
             (Unassessed, []) -> Assisted (errorMessages pas)
             (Unassessed, ps) -> Assisted $ "With passing properties: " ++ commas (map propName ps) ++ "\n" ++ (errorMessages pas)
             _                -> Assisted (errorMessages pas)
  hPutStrLn stderr $ "Judgement was " ++ (show . vertexJmt) v ++ ", and is now " ++ show j'
  return v{vertexJmt=j'}
  where
  commas :: [String] -> String
  commas = concat . (intersperse ", ")

data PropApp = Error String | Holds | Disproves

hasResult :: PropApp -> Bool
hasResult (Error _) = False
hasResult _         = True

holds :: PropApp -> Bool
holds Holds = True
holds _     = False

disproves :: PropApp -> Bool
disproves Disproves = True
disproves _         = False

errorMessages :: [PropApp] -> String
errorMessages = foldl (\msgs (Error msg) -> msgs ++ "\n---\n" ++ msg) [] . filter (not . hasResult)

evalProposition :: PropVarGen String -> Trace -> Vertex -> [Module] -> Proposition -> IO PropApp
evalProposition unevalGen trc v ms prop = do
  createDirectoryIfMissing True ".Hoed/exe"
  hPutStrLn stderr $ "\nEvaluating proposition " ++ propName prop ++ " with statement " ++ (shorten . noNewlines . show . vertexStmt) v
  -- Uncomment to print arguments in full.
  -- let args = map (\(n,s) -> "Argument " ++ show n ++ ": " ++ s) $ zip [0..] (generateArgs trc getEvent i)
  -- let args = map (\(n,s) -> "Argument " ++ show n ++ ": " ++ (shorten s)) $ zip [0..] (generateArgs trc getEvent i)
  -- hPutStrLn stderr $ "Statement UID = " ++ show i
  -- mapM (hPutStrLn stderr) args
  clean
  prgm <- generateCode
  compile
  exit' <- compile
  err  <- readFile errFile
  -- Uncomment to print compilation messages.
  -- hPutStrLn stderr $ err
  -- hPutStrLn stderr $ "Compilation exitted with " ++ show exit'
  case exit' of 
    (ExitFailure _) -> return $ Error $ "Compilation of {{{\n" ++ prgm ++ "\n}}} failed with:\n" ++ err
    ExitSuccess     -> do 
      exit <- evaluate
      out  <- readFile outFile
      -- hPutStrLn stderr $ out
      -- hPutStrLn stderr $ "Evaluation exitted with " ++ show exit
      return $ case (exit,out) of
        (ExitFailure _, _)         -> Error out
        (ExitSuccess  , "True\n")  -> Holds
        (ExitSuccess  , "False\n") -> Disproves
        (ExitSuccess  , _)         -> Error out

    where
    clean        = system $ "rm -f " ++ sourceFile ++ " " ++ exeFile ++ " " ++ buildFiles
    generateCode = do -- Uncomment the next line to dump generated program on screen
                      -- hPutStrLn stderr $ "Generated the following program ***\n" ++ prgm ++ "\n***" 
                      writeFile sourceFile prgm
                      return prgm
                      where prgm :: String
                            prgm = (generate unevalGen prop ms trc getEvent i)
    compile      = system $ "ghc  -i" ++ (searchPath . propModule) prop ++ " -o " ++ exeFile ++ " " ++ sourceFile ++ " > " ++ errFile ++ " 2>&1"
    evaluate     = system $ exeFile ++ " > " ++ outFile ++ " 2>&1"
    i            = (stmtIdentifier . vertexStmt) v

    shorten s
      | length s < 120 = s
      | otherwise    = (take 117 s) ++ "..."

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

type PropVars = ([String],[String]) -- A tuple of used variables, and a supply of fresh variables
type PropVarGen a = PropVars -> (a,PropVars)

comp :: PropVarGen a -> PropVarGen b -> (a -> b -> c) -> PropVarGen c
comp x y f vs = let (x', vs')  = x vs
                    (y', vs'') = y vs'
                in  (f x' y', vs'')

-- MF TODO: this is exactly like liftM2
liftPV :: (a -> b -> c) -> PropVarGen a -> PropVarGen b -> PropVarGen c
liftPV f x y = comp x y f

propVars0 :: PropVars
propVars0 = ([], map (('x':) . show)  [1..])

propVarError :: PropVarGen String
propVarError = propVarReturn "(error \"Request of value that was unevaluated in original program.\")"

propVarFresh :: PropVarGen String
propVarFresh (bvs,v:fvs) = (v, (bvs,fvs))

propVarReturn :: String -> PropVarGen String
propVarReturn s vs = (s,vs)

propVarBind :: (String,PropVars) -> Proposition -> String
propVarBind (propApp,([],_))  prop = generatePrint prop ++ " $ " ++ propApp
propVarBind (propApp,(bvs,_)) prop = "quickCheck (\\" ++ bvs' ++ " -> " ++ propApp ++ ")"
  where
  bvs' = concat (intersperse " " bvs)

generatePrint :: Proposition -> String
generatePrint p = case propositionType p of
  BoolProposition       -> "print"
  QuickCheckProposition -> "do g <- newStdGen; print . fromJust . ok . (generate 1 g) . evaluate"

------------------------------------------------------------------------------------------------------------------------

generate :: PropVarGen String -> Proposition -> [Module] -> Trace -> (UID->Event) -> UID -> String
generate unevalGen prop ms trc getEvent i = generateHeading prop ms ++ generateMain unevalGen prop trc getEvent i

generateHeading :: Proposition -> [Module] -> String
generateHeading prop ms =
  "-- This file is generated by the Haskell debugger Hoed\n"
  ++ generateImport (propModule prop)
  ++ foldl (\acc m -> acc ++ generateImport m) "" ms

generateImport :: Module -> String
generateImport m =  "import " ++ (moduleName m) ++ "\n"

generateMain :: (PropVarGen String) -> Proposition -> Trace -> (UID->Event) -> UID -> String
generateMain unevalGen prop trc getEvent i
  = "main = " ++ propVarBind (foldl accArg ((propName prop),propVars0) (reverse . argMap $ prop)) prop ++ "\n"
    where 
    accArg :: (String,PropVars) -> Int -> (String,PropVars)
    accArg (acc,propVars) x = let (s,propVars') = getArg x propVars in (acc ++ " " ++ s, propVars')
    getArg :: Int -> PropVarGen String
    getArg x
      | x < (length args) = args !! x
      | otherwise         = propVarError

    args :: [PropVarGen String]
    args = generateArgs unevalGen trc getEvent i

generateArgs :: (PropVarGen String) -> Trace -> (UID -> Event) -> UID -> [PropVarGen String]
generateArgs unevalGen trc getEvent i = case dfsChildren frt e of
  [_,ma,_,mr]    -> generateExpr unevalGen frt ma : moreArgs unevalGen trc getEvent mr
  [Nothing,_,mr] -> unevalGen                     : moreArgs unevalGen trc getEvent mr
  xs             -> error ("generateArgs: dfsChildren (" ++ show e ++ ") = " ++ show xs)
  where
  frt = (mkEventForest trc)
  e   = getEvent i -- (reverse trc) !! (i-1)

moreArgs :: PropVarGen String -> Trace -> (UID->Event) -> Maybe Event -> [PropVarGen String]
moreArgs _ trc getEvent Nothing = []
moreArgs unevalGen trc getEvent (Just e)
  | change e == Fun = generateArgs unevalGen trc getEvent (eventUID e)
  | otherwise       = []

generateExpr :: PropVarGen String -> EventForest -> Maybe Event -> PropVarGen String
generateExpr unevalGen _ Nothing    = unevalGen
generateExpr unevalGen frt (Just e) = case change e of
  (Cons _ s) -> let s' = if isAlpha (head s) then s else "(" ++ s ++ ")"
                in liftPV (++) ( foldl (liftPV $ \acc c -> acc ++ " " ++ c)
                                 (propVarReturn ("(" ++ s')) cs
                               ) 
                               ( propVarReturn ") "
                               )
  Enter      -> propVarReturn ""
  _          -> propVarReturn "error \"cannot represent\""

  where cs :: [PropVarGen String]
        cs = map (generateExpr unevalGen frt) (dfsChildren frt e)

------------------------------------------------------------------------------------------------------------------------

class ParEq a where
  (===) :: a -> a -> Maybe Bool
  default (===) :: (Generic a, GParEq (Rep a)) => a -> a -> Maybe Bool
  x === y = gParEq (from x) (from y)

class GParEq rep where
  gParEq :: rep a -> rep a -> Maybe Bool

orNothing :: IO (Maybe Bool) -> Maybe Bool
orNothing mb = unsafePerformIO $ ourCatchAllIO mb (\_ -> return Nothing)

catchEq :: Eq a => a -> a -> Maybe Bool
catchEq x y = orNothing $ do mb <- evaluate (x == y); return (Just mb)

catchGEq :: GParEq rep => rep a -> rep a -> Maybe Bool
catchGEq x y = orNothing $ x `seq` y `seq` (evaluate $ gParEq x y)

-- Sums: encode choice between constructors
instance (GParEq a, GParEq b) => GParEq (a :+: b) where
  gParEq x y = let r = gParEq_ x y in r
    where gParEq_ (L1 x) (L1 y) = x `catchGEq` y
          gParEq_ (R1 x) (R1 y) = x `catchGEq` y
          gParEq_ _      _      = Just False

-- Products: encode multiple arguments to constructors
instance (GParEq a, GParEq b) => GParEq (a :*: b) where
  gParEq x y = let r = gParEq_ x y in r
    where gParEq_ (x :*: x') (y :*: y')
            | any (== (Just False)) mbs = Just False
            | all (== (Just True))  mbs = Just True
            | otherwise                 = Nothing
            where mbs = [(catchGEq x y) `seq` (catchGEq x y), (catchGEq x' y') `seq` (catchGEq x' y')]

-- Unit: used for constructors without arguments
instance GParEq U1 where
  gParEq x y = let r = gParEq_ x y in r
    where gParEq_ x y = catchEq x y

-- Constants: additional parameters and recursion of kind *
instance (ParEq a) => GParEq (K1 i a) where
  gParEq x y = let r = gParEq_ x y in r
    where gParEq_ (K1 x) (K1 y) = x === y

-- Meta: data types
instance (GParEq a) => GParEq (M1 D d a) where
  gParEq x y = let r = gParEq_ x y in r
    where gParEq_ (M1 x) (M1 y) = x `catchGEq` y

-- Meta: Selectors
instance (GParEq a, Selector s) => GParEq (M1 S s a) where
  gParEq x y = let r = gParEq_ x y in r
    where gParEq_ (M1 x) (M1 y) = x `catchGEq` y
        
-- Meta: Constructors
instance (GParEq a, Constructor c) => GParEq (M1 C c a) where
  gParEq x y = let r = gParEq_ x y in r
    where gParEq_ (M1 x) (M1 y) = x `catchGEq` y

instance (ParEq a)          => ParEq [a]
instance (ParEq a, ParEq b) => ParEq (a,b)
instance (ParEq a)          => ParEq (Maybe a)
instance ParEq Int          where x === y = Just (x == y)
instance ParEq Bool         where x === y = Just (x == y)
instance ParEq Integer      where x === y = Just (x == y)
instance ParEq Float        where x === y = Just (x == y)
instance ParEq Double       where x === y = Just (x == y)
instance ParEq Char         where x === y = Just (x == y)
