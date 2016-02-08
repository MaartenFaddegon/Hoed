-- This file is part of the Haskell debugger Hoed.
--
-- Copyright (c) Maarten Faddegon, 2015

{-# LANGUAGE DefaultSignatures, TypeOperators, FlexibleContexts, FlexibleInstances, StandaloneDeriving, CPP, DeriveGeneric #-}

module Debug.Hoed.Pure.Prop where
-- ( judge
-- , Propositions(..)
-- ) where
import Debug.Hoed.Pure.Observe(Observable(..),Trace(..),UID,Event(..),Change(..),ourCatchAllIO,evaluate)
import Debug.Hoed.Pure.Render(CompStmt(..),noNewlines)
import Debug.Hoed.Pure.CompTree(CompTree,Vertex(..),Graph(..),vertexUID,vertexRes)
import Debug.Hoed.Pure.EventForest(EventForest,mkEventForest,dfsChildren)
import Debug.Hoed.Pure.Serialize
import qualified Data.IntMap as M
import Prelude hiding (Right)
import Data.Graph.Libgraph(Judgement(..),AssistedMessage(..),mapGraph)
import System.Directory(createDirectoryIfMissing)
import System.Process(system)
import System.Exit(ExitCode(..))
import System.IO(hPutStrLn,stderr)
import System.IO.Unsafe(unsafePerformIO)
import Data.Char(isAlpha)
import Data.Maybe(isNothing,fromJust)
import Data.List(intersperse,isInfixOf)
import GHC.Generics hiding (moduleName) --(Generic(..),Rep(..),from,(:+:)(..),(:*:)(..),U1(..),K1(..),M1(..))
import Control.Monad(foldM)

------------------------------------------------------------------------------------------------------------------------

data Propositions = Propositions { propositions :: [Proposition], propType :: PropType, funName :: String
                                 , extraModules :: [Module]
                                 }

data PropType = Specify | PropertiesOf deriving Eq

data Signature 
  = Argument Int
  | SubjectFunction
  | Random
  deriving Show

type Proposition = (PropositionType,Module,String,[Signature])

data PropositionType 
  = IOProposition
  | BoolProposition
  | LegacyQuickCheckProposition
  | QuickCheckProposition 
  deriving Show

data Module = Module {moduleName :: String, searchPath :: String} 
  deriving Show

data PropRes 
  = Error Proposition String 
  | Hold Proposition 
  | HoldWeak Proposition
  | Disprove Proposition
  | DisproveBy Proposition [String] 
  deriving Show

propositionType :: Proposition -> PropositionType
propositionType (x,_,_,_) = x

propName :: Proposition -> String
propName (_,_,x,_) = x

signature :: Proposition -> [Signature]
signature (_,_,_,x) = x

propModule :: Proposition -> Module
propModule (_,x,_,_) = x

------------------------------------------------------------------------------------------------------------------------

sourceFile   = ".Hoed/exe/Main.hs"
buildFiles   = ".Hoed/exe/Main.o .Hoed/exe/Main.hi"
exeFile      = ".Hoed/exe/Main"
outFile      = ".Hoed/exe/Main.out"
errFile      = ".Hoed/exe/Main.compilerMessages"
treeFilePath = ".Hoed/savedCompTree."
traceFilePath = ".Hoed/savedTrace."

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

judgeAll :: UnevalHandler -> Trace -> [Propositions] -> CompTree -> IO CompTree
judgeAll handler trc ps compTree = undefined

data Judge = Judge Judgement | AlternativeTree CompTree Trace

-- First tries restricted and bottom for unevaluated expressions,
-- then unrestricted and random values for unevaluated expressions.
-- MF TODO: in between try unrestricted with bottom for unevaluated expressions (still need to switch trees if Wrong!)
judge :: Trace -> Propositions -> Vertex -> (CompTree -> Int) -> CompTree -> IO Judge
judge trc p v complexity curTree = do
  putStrLn $ take 50 (cycle "-")
  putStrLn $ "Evaluating properties to judge statement: " ++ vertexRes v
  putStrLn $ take 50 (cycle "-")
  pas <- evalPropositions Bottom trc p v
  let j | propType p == Specify && all holds pas  = return (Judge Right)
        | any disproves pas                       = return (Judge Wrong)
        | otherwise                               = do
            pas' <- evalPropositions Forall trc p v
            let j' | propType p == Specify && all holds pas'  = return (Judge Right)
                   | any disproves pas'                       = do 
                       let curComplexity = complexity curTree
                       (bestComplexity,bestTree,bestTrace) <- simplestTree complexity (extraModules p) pas' (curComplexity,curTree,[]) trc v
                       if bestComplexity == curComplexity
                         then return $ Judge $ Assisted $ [InconclusiveProperty $ "We found values for the unevaluated"
                                                           ++ " expressions in the current statement that falsify\n"
                                                           ++ " a property, however the resulting tree is more complex."]
                         else return (AlternativeTree bestTree bestTrace)
                   | otherwise                                = return (advice pas')
            j'
  j

holds :: PropRes -> Bool
holds (Hold _) = True
holds (HoldWeak _) = True
holds _         = False

disproves :: PropRes -> Bool
disproves (Disprove _)     = True
disproves (DisproveBy _ _) = True
disproves _                = False

disprovesBy :: PropRes -> Bool
disprovesBy (DisproveBy _ _) = True
disprovesBy _                = False

-- Using the given complexity function, compare the computation trees of the list of
-- property results and select the simplest tree.
simplestTree :: (CompTree -> Int) -> [Module] -> [PropRes] -> (Int,CompTree,Trace) -> Trace -> Vertex -> IO (Int,CompTree,Trace)
simplestTree complexity ms rs cur trc v = foldM (simple2 complexity trc v ms) cur (filter disprovesBy rs)

-- Using the given complexity function, read the computation tree of the given property
-- into memory and compare its complexity with the complexity of the currently best tree.
-- Return whichever of these two trees is simplest.
simple2 :: (CompTree -> Int)  -> Trace -> Vertex -> [Module] -> (Int,CompTree,Trace) -> PropRes -> IO (Int,CompTree,Trace)
simple2 complexity trc v ms (curComplexity,curTree,curTrace) propRes = do
  reEvalProposition propRes trc v ms
  maybeCandTree  <- restoreTree  $ treeFilePath  ++ resOf propRes
  maybeCandTrace <- restoreTrace $ traceFilePath ++ resOf propRes
  case (maybeCandTree,maybeCandTrace) of
    (Just candTree, Just candTrace) -> do 
      let candComplexity = complexity candTree
      return $ if candComplexity < curComplexity 
                 then (candComplexity,candTree,candTrace)
                 else (curComplexity,curTree,curTrace)
    _ -> do
      putStrLn $ "FAILED to to restore computation tree of " ++ resOf propRes
      return (curComplexity,curTree,curTrace)

advice :: [PropRes] -> Judge
advice rs' = case filter holds rs' of
  [] -> Judge $ Assisted $ errorMessages rs'
  rs -> Judge $ Assisted $ PassingProperty (commas (map resOf rs)) : errorMessages rs'

commas :: [String] -> String
commas = concat . (intersperse ", ")

errorMessages :: [PropRes] -> [AssistedMessage]
errorMessages = foldl (\acc (Error prop msg) -> InconclusiveProperty ("\n---\n\nApplying property " ++ propName prop ++ " gives inconclusive result:\n\n" ++ msg) : acc) [] . filter isError

isError (Error _ _) = True
isError _           = False

data UnevalHandler = Bottom | Forall | FromList [String] deriving (Eq, Show)

unevalHandler :: UnevalHandler -> PropVarGen String
unevalHandler Bottom       = propVarError
unevalHandler Forall       = propVarFresh
unevalHandler (FromList _) = propVarFresh

unevalState :: UnevalHandler -> PropVars
unevalState Bottom            = propVars0
unevalState Forall            = propVars0
unevalState (FromList values) = ([],values)

resOf :: PropRes -> String
resOf (Error p _)      = propName p
resOf (Hold p)         = propName p
resOf (HoldWeak p)     = propName p
resOf (Disprove p)     = propName p
resOf (DisproveBy p _) = propName p

evalPropositions :: UnevalHandler -> Trace -> Propositions -> Vertex -> IO [PropRes]
evalPropositions _ _ _ RootVertex = return []
evalPropositions handler trc p v = mapM (evalProposition handler trc v (extraModules p)) (propositions p)

evalProposition :: UnevalHandler -> Trace -> Vertex -> [Module] -> Proposition -> IO PropRes
evalProposition handler trc v ms prop = do
  putStrLn $ "property " ++ propName prop
  createDirectoryIfMissing True ".Hoed/exe"
  clean
  prgm <- generateCode handler trc v prop ms
  compile prop
  exit <- compile prop
  case exit of 
    (ExitFailure _) -> do
        err  <- readFile errFile
        putStrLn $ "failed to compile: " ++ err
        return $ Error prop $ "Compilation of {{{\n" ++ prgm ++ "\n}}} failed with:\n" ++ err
    ExitSuccess     -> do 
      exit <- run
      out <- readFile outFile
      putStrLn $ "evaluated to: " ++ out
      return $ mkPropRes prop exit (backspaces out)


reEvalProposition :: PropRes -> Trace -> Vertex -> [Module] -> IO PropRes
reEvalProposition (DisproveBy prop values) trc v ms = do
  putStrLn $ "RE-EVALUATE with " ++ concat (intersperse " " values) ++ " {"
  (Disprove p) <- evalProposition (FromList values) trc v ms prop
  putStrLn $ "} RE-EVALUATE"
  return (Disprove p)

clean = system $ "rm -f " ++ sourceFile ++ " " ++ exeFile ++ " " ++ buildFiles

compile prop = system $ "ghc  -i" ++ (searchPath . propModule) prop ++ " -o " ++ exeFile ++ " " ++ sourceFile ++ " > " ++ errFile ++ " 2>&1"

run = system $ exeFile ++ " > " ++ outFile ++ " 2>&1"

generateCode :: UnevalHandler -> Trace -> Vertex -> Proposition -> [Module] -> IO String
generateCode handler trc v prop ms = do 
  -- Uncomment the next line to dump generated program on screen
  -- hPutStrLn stderr $ "Generated the following program ***\n" ++ prgm ++ "\n***" 
  writeFile sourceFile prgm
  return prgm
  where 
  prgm = generate handler prop ms trc (getEventFromMap $ eventMap trc) i f
  i    = (stmtIdentifier . vertexStmt) v
  f    = (stmtLabel . vertexStmt) v


getEventFromMap m j = fromJust $ M.lookup j m

eventMap trc = M.fromList $ map (\e -> (eventUID e, e)) trc

mkPropRes :: Proposition -> ExitCode -> String -> PropRes
mkPropRes prop (ExitFailure _) out        = Error prop out
mkPropRes prop ExitSuccess out
  | out == "True\n"                       = Hold prop
  | out == "+++ OK, passed 100 tests.\n"  = HoldWeak prop
  | out == "False\n"                      = Disprove prop
  | "Failed! Falsifiable" `isInfixOf` out = DisproveBy prop (tail . lines $ out)
  | otherwise                             = Error prop out

shorten s
  | length s < 120 = s
  | otherwise    = (take 117 s) ++ "..."

backspaces :: String -> String
backspaces s = reverse (backspaces' [] s)
  where
  backspaces' s []    = s
  backspaces' s (c:t)
    | c == '\b'       = backspaces' (safeTail s) t
    | otherwise       = backspaces' (c:s)        t

  safeTail []    = []
  safeTail (c:s) = s

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
propVarFresh (bvs,v:fvs) = (v, (v:bvs,fvs))

propVarReturn :: String -> PropVarGen String
propVarReturn s vs = (s,vs)

propVarBind :: UnevalHandler -> (String,PropVars) -> Proposition -> String
propVarBind (FromList _) (propApp,_)       prop = generatePrint prop ++ propApp
propVarBind _            (propApp,([],_))  prop = generatePrint prop ++ propApp
propVarBind _            (propApp,(bvs,_)) prop = "quickCheck (\\" ++ bvs' ++ " -> " ++ propApp ++ ")"
  where
  bvs' = concat (intersperse " " bvs)

generatePrint :: Proposition -> String
generatePrint p = case propositionType p of
  IOProposition         -> ""
  BoolProposition       -> "print $ "
  QuickCheckProposition -> "(\\q -> do MkRose res ts <- protectRose .reduceRose .  unProp . (\\p->unGen p  (mkQCGen 1) 1) . unProperty $ q; print . fromJust . ok $ res) $ "
  LegacyQuickCheckProposition -> "do g <- newStdGen; print . fromJust . ok . (generate 1 g) . evaluate $ "

------------------------------------------------------------------------------------------------------------------------

generate :: UnevalHandler -> Proposition -> [Module ] -> Trace -> (UID->Event) -> UID -> String -> String
generate handler prop ms trc getEvent i f = generateHeading prop ms ++ generateMain handler prop trc getEvent i f

generateHeading :: Proposition -> [Module] -> String
generateHeading prop ms =
  "-- This file is generated by the Haskell debugger Hoed\n"
  ++ generateImport (propModule prop)
  ++ generateImport (Module "qualified Debug.Hoed.Pure as Hoed" [])
  ++ qcImports
  ++ foldl (\acc m -> acc ++ generateImport m) "" ms
  where
  qcImports = case propositionType prop of
        BoolProposition -> generateImport (Module "Test.QuickCheck" [])
        IOProposition   -> generateImport (Module "Test.QuickCheck" [])
        LegacyQuickCheckProposition -> qcImports'
        QuickCheckProposition -> qcImports'
                                 ++ generateImport (Module "Test.QuickCheck.Property" [])
                                 ++ generateImport (Module "Test.QuickCheck.Gen" [])
                                 ++ generateImport (Module "Test.QuickCheck.Random" [])
  qcImports'
    = generateImport (Module "System.Random" [])             -- newStdGen
      ++ generateImport (Module "Data.Maybe" [])             -- fromJust
      ++ generateImport (Module "Test.QuickCheck" [])

generateImport :: Module -> String
generateImport m =  "import " ++ (moduleName m) ++ "\n"

generateMain :: UnevalHandler -> Proposition -> Trace -> (UID->Event) -> UID -> String -> String
generateMain handler prop trc getEvent i f
  = "main = Hoed.runOstore \"" ++ (propName prop) ++"\" $ "
            ++ propVarBind handler (foldl accSig ((propName prop) ++ " ",unevalState handler) (signature prop)) prop
            ++ "\n"
    where 
    accSig :: (String,PropVars) -> Signature -> (String,PropVars)
    accSig (acc,propVars) x = let (s,propVars') = getSig x propVars in (acc ++ " " ++ s, propVars')
    getSig :: Signature -> PropVarGen String
    getSig SubjectFunction = cf
    getSig (Argument i) | i < length args = args !! i
    -- MF TODO: should we do something better when user gives index that is out of bounds?
    getSig Random = propVarFresh
    getSig _ = propVarError

    args :: [PropVarGen String]
    args = generateArgs (unevalHandler handler) trc getEvent i

    cf :: PropVarGen String
    cf | handler == Bottom = foldl1 (liftPV $ \acc c -> acc ++ " " ++ c)
                            [ propVarReturn $ "(" ++ genConAp (length args) f
                            , generateRes (unevalHandler handler) trc getEvent i
                            , propVarReturn $ ")"
                            ]
       | otherwise = propVarReturn f

generateRes :: (PropVarGen String) -> Trace -> (UID -> Event) -> UID -> PropVarGen String
generateRes unevalGen trc getEvent i = case dfsChildren frt e of
  [_,_,_,Nothing] -> unevalGen
  [_,_,_,mr]      -> generateRes' unevalGen trc getEvent mr
  where
  frt = (mkEventForest trc)
  e   = getEvent i

generateRes' :: PropVarGen String -> Trace -> (UID->Event) -> Maybe Event -> PropVarGen String
generateRes' unevalGen trc getEvent Nothing = unevalGen
generateRes' unevalGen trc getEvent (Just e)
  | change e == Fun = case dfsChildren frt e of [_,_    ,_,mr] -> generateRes' unevalGen trc getEvent mr
                                                [Nothing,_,mr] -> generateRes' unevalGen trc getEvent mr
                                                as  -> error $ "generateRes': event " ++ show (eventUID e) ++ ":FUN has " 
                                                               ++ show (length as) ++ " children!\nnamely: " ++ commas (map show as)
  | otherwise       = generateExpr unevalGen frt (Just e)
  where
  frt = (mkEventForest trc)

generateArgs :: (PropVarGen String) -> Trace -> (UID -> Event) -> UID -> [PropVarGen String]
generateArgs unevalGen trc getEvent i = case dfsChildren frt e of
  [_,ma,_,mr]    -> generateExpr unevalGen frt ma : moreArgs unevalGen trc getEvent mr
  [Nothing,_,mr] -> unevalGen                     : moreArgs unevalGen trc getEvent mr
  xs             -> error ("generateArgs: dfsChildren (" ++ show e ++ ") = " ++ show xs)
  where
  frt = (mkEventForest trc)
  e   = getEvent i

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

-- only works if there is 1 argument
conAp :: Observable b => (a -> b) -> b -> a -> b
conAp f r x = constrain (f x) r

genConAp :: Int -> String -> String
genConAp n f = "(\\r " ++ args ++ " -> Hoed.constrain (" ++ f ++ " " ++ args ++ ") r)"
  where args = foldl1 (\a x -> a ++ " " ++ x) xs
        xs = map (\i -> "x" ++ show i) [1..n]

------------------------------------------------------------------------------------------------------------------------
-- MF TODO: this should probably be part of the Observable class ...

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
