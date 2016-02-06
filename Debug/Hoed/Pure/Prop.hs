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
import Debug.Hoed.Pure.CompTree(CompTree,Vertex(..),Graph(..),vertexUID)
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

data PropType     = Specify | PropertiesOf deriving Eq

type Proposition  = (PropositionType,Module,String,[Int])

data PropositionType = IOProposition | BoolProposition | LegacyQuickCheckProposition | QuickCheckProposition deriving Show

data Module       = Module {moduleName :: String, searchPath :: String} deriving Show

propositionType :: Proposition -> PropositionType
propositionType (x,_,_,_) = x

propName :: Proposition -> String
propName (_,_,x,_) = x

argMap :: Proposition -> [Int]
argMap (_,_,_,x) = x

propModule :: Proposition -> Module
propModule (_,x,_,_) = x

------------------------------------------------------------------------------------------------------------------------

sourceFile   = ".Hoed/exe/Main.hs"
buildFiles   = ".Hoed/exe/Main.o .Hoed/exe/Main.hi"
exeFile      = ".Hoed/exe/Main"
outFile      = ".Hoed/exe/Main.out"
errFile      = ".Hoed/exe/Main.compilerMessages"
treeFilePath = ".Hoed/savedCompTree."

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

evalProps :: UnevalHandler -> Trace -> Propositions -> Vertex -> IO [PropApp]
evalProps _ _ _ RootVertex = return []
evalProps handler trc p v = mapM (evalProposition handler trc v (extraModules p)) (propositions p)

data Judge = Judge Judgement | AlternativeTree CompTree

-- First tries restricted and bottom for unevaluated expressions,
-- then unrestricted and random values for unevaluated expressions.
-- MF TODO: in between try unrestricted with bottom for unevaluated expressions (still need to switch trees if Wrong!)
judge :: Trace -> Propositions -> Vertex -> (CompTree -> Int) -> CompTree -> IO Judge
judge trc p v complexity curTree = do
  putStrLn "PHASE 1"
  pas <- evalProps Bottom trc p v
  let j | propType p == Specify && all holds pas  = return (Judge Right)
        | any disproves pas                       = return (Judge Wrong)
        | otherwise                               = do
            putStrLn "PHASE 2"
            pas' <- evalProps Forall trc p v
            let j' | propType p == Specify && all holds pas'  = return (Judge Right)
                   | any disproves pas'                       = do 
                       let curComplexity = complexity curTree
                       (bestComplexity,bestTree) <- simplestTree complexity pas' (curComplexity,curTree)
                       if bestComplexity == curComplexity
                         then return $ Judge $ Assisted $ [InconclusiveProperty $ "We found values for the unevaluated"
                                                           ++ " expressions in the current statement, however the resulting"
                                                           ++ " tree is more complex."]
                         else return (AlternativeTree bestTree)
                   | otherwise                                = return (advice pas')
            j'
  j

holds :: PropApp -> Bool
holds (Hold _) = True
holds (HoldWeak _) = True
holds _         = False

disproves :: PropApp -> Bool
disproves (Disprove _)     = True
disproves (DisproveBy _ _) = True
disproves _                = False

simplestTree :: (CompTree -> Int) -> [PropApp] -> (Int,CompTree) -> IO (Int,CompTree)
simplestTree complexity pas cur = foldM (simTree complexity) cur pas

simTree :: (CompTree -> Int) -> (Int,CompTree) -> PropApp -> IO (Int,CompTree)
simTree complexity (curComplexity,curTree) pa = do
  maybeCandTree <- restoreTree $ treeFilePath ++ (getTag pa)
  case maybeCandTree of
    Nothing         -> return (curComplexity,curTree)
    (Just candTree) -> do 
      let candComplexity = complexity candTree
      return $ if candComplexity < curComplexity 
                 then (candComplexity,candTree)
                 else (curComplexity,curTree)

advice :: [PropApp] -> Judge
advice pas = case filter holds pas of
  [] -> Judge $ Assisted $ errorMessages pas
  ps -> Judge $ Assisted $ PassingProperty (commas (map getTag ps)) : errorMessages pas

commas :: [String] -> String
commas = concat . (intersperse ", ")

errorMessages :: [PropApp] -> [AssistedMessage]
errorMessages = foldl (\acc (Error tag msg) -> InconclusiveProperty ("\n---\n\nApplying property " ++ tag ++ " gives inconclusive result:\n\n" ++ msg) : acc) [] . filter isError

isError (Error _ _) = True
isError _           = False

{-
-- Use a property to judge a vertex
judgeWithPropositions :: UnevalHandler -> Trace -> Propositions -> Vertex -> IO Vertex
judgeWithPropositions _ _ _ RootVertex = return RootVertex
judgeWithPropositions handler trc p v = do
  putStrLn $ "\n================\n"
  pas' <- mapM (evalProposition handler trc v (extraModules p)) (propositions p)
  putStrLn $ "judgeWithPropositions: pas=" ++ show pas'
  let pas = if handler == TrustForall then trustWeak pas' else weaken pas'
      z = zip pas (propositions p)
      a = case (map snd) . (filter (holds . fst)) $ z of
            [] -> errorMessages z
            ps -> PassingProperty ("With passing properties: " ++ commas (map propName ps))
                  : errorMessages z
      j' | propType p == Specify && all hasResult pas
                                       = if any disproves pas then Wrong else Right
         | any hasResult pas           = if any disproves pas then Wrong else Assisted a
         | otherwise                   = Assisted a
      j  | j' `moreInfo` (vertexJmt v) = j'
         | otherwise                   = vertexJmt v

  hPutStrLn stderr $ "Judgement was " ++ (show . vertexJmt) v ++ ", and is now " ++ show j
  return v{vertexJmt=j}
  where
  commas :: [String] -> String
  commas = concat . (intersperse ", ")

  moreInfo _          Unassessed   = True
  moreInfo Unassessed (Assisted _) = False
  moreInfo _          (Assisted _) = True
  moreInfo _          Right        = False
  moreInfo _          Wrong        = False
-}

data UnevalHandler = Bottom | Forall deriving (Eq, Show)

unevalHandler :: UnevalHandler -> PropVarGen String
unevalHandler Bottom = propVarError
unevalHandler Forall = propVarFresh


type Tag = String

data PropApp = Error Tag String | Hold Tag | HoldWeak Tag | Disprove Tag | DisproveBy Tag [String] deriving Show

getTag :: PropApp -> Tag
getTag (Error t _)      = t
getTag (Hold t)         = t
getTag (HoldWeak t)     = t
getTag (Disprove t)     = t
getTag (DisproveBy t _) = t

-- trustWeak :: [PropApp] -> [PropApp]
-- trustWeak = map f
--   where f (HoldsWeak tag) = Holds tag
--         f p               = p

-- weaken :: [PropApp] -> [PropApp]
-- weaken = map f
--   where f (HoldsWeak tag) = Error tag "Holds for all randomly generated values we tried."
--         f p               = p

-- hasResult :: PropApp -> Bool
-- hasResult (Error _ _)   = False
-- hasResult (HoldsWeak _) = False
-- hasResult _             = True
-- 
-- hasWeakResult :: PropApp -> Bool
-- hasWeakResult (Error _ _) = False
-- hasWeakResult _           = True



evalProposition :: UnevalHandler -> Trace -> Vertex -> [Module] -> Proposition -> IO PropApp
evalProposition handler trc v ms prop = do
  createDirectoryIfMissing True ".Hoed/exe"
  let tag = propName prop
  hPutStrLn stderr $ "Evaluating proposition " ++ tag ++ " with statement " ++ (shorten . noNewlines . show . vertexStmt) v
  clean
  prgm <- generateCode
  compile
  exit' <- compile
  err  <- readFile errFile
  hPutStrLn stderr $ err
  hPutStrLn stderr $ "Compilation exitted with " ++ show exit'
  case exit' of 
    (ExitFailure _) -> return $ Error tag $ "Compilation of {{{\n" ++ prgm ++ "\n}}} failed with:\n" ++ err
    ExitSuccess     -> do 
      exit <- evaluate
      out' <- readFile outFile
      let out = backspaces out'
      hPutStrLn stderr $ out
      hPutStrLn stderr $ "Evaluation exitted with " ++ show exit
      return $ case (exit,out) of
        (ExitFailure _, _)         -> Error tag out
        (ExitSuccess  , "True\n")  -> Hold tag
        (ExitSuccess  , "+++ OK, passed 100 tests.\n") -> HoldWeak tag
        (ExitSuccess  , "False\n") -> Disprove tag
        (ExitSuccess  , _)         -> if "Failed! Falsifiable" `isInfixOf` out
                                        then DisproveBy tag (tail . lines $ out)
                                        else Error tag out

    where
    clean        = system $ "rm -f " ++ sourceFile ++ " " ++ exeFile ++ " " ++ buildFiles
    generateCode = do -- Uncomment the next line to dump generated program on screen
                      -- hPutStrLn stderr $ "Generated the following program ***\n" ++ prgm ++ "\n***" 
                      writeFile sourceFile prgm
                      return prgm
                      where prgm :: String
                            prgm = (generate handler prop ms trc getEvent i f)
    compile      = system $ "ghc  -i" ++ (searchPath . propModule) prop ++ " -o " ++ exeFile ++ " " ++ sourceFile ++ " > " ++ errFile ++ " 2>&1"
    evaluate     = system $ exeFile ++ " > " ++ outFile ++ " 2>&1"
    i            = (stmtIdentifier . vertexStmt) v
    f            = (stmtLabel . vertexStmt) v

    shorten s
      | length s < 120 = s
      | otherwise    = (take 117 s) ++ "..."

    getEvent :: UID -> Event
    getEvent j = fromJust $ M.lookup j m
      where m = M.fromList $ map (\e -> (eventUID e, e)) trc

backspaces :: String -> String
backspaces s = reverse (backspaces' [] s)
  where
  backspaces' s []    = s
  backspaces' s (c:t)
    | c == '\b'       = backspaces' (safeTail s) t
    | otherwise       = backspaces' (c:s)        t

  safeTail []    = []
  safeTail (c:s) = s

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
propVarFresh (bvs,v:fvs) = (v, (v:bvs,fvs))

propVarReturn :: String -> PropVarGen String
propVarReturn s vs = (s,vs)

propVarBind :: (String,PropVars) -> Proposition -> String
propVarBind (propApp,([],_))  prop = generatePrint prop ++ propApp
propVarBind (propApp,(bvs,_)) prop = "quickCheck (\\" ++ bvs' ++ " -> " ++ propApp ++ ")"
  where
  bvs' = concat (intersperse " " bvs)

generatePrint :: Proposition -> String
generatePrint p = case propositionType p of
  IOProposition         -> ""
  BoolProposition       -> "print $ "
  QuickCheckProposition -> "(\\q -> do MkRose res ts <- protectRose .reduceRose .  unProp . (\\p->unGen p  (mkQCGen 1) 1) . unProperty $ q; print . fromJust . ok $ res) $ "
  LegacyQuickCheckProposition -> "do g <- newStdGen; print . fromJust . ok . (generate 1 g) . evaluate $ "

------------------------------------------------------------------------------------------------------------------------

generate :: UnevalHandler -> Proposition -> [Module] -> Trace -> (UID->Event) -> UID -> String -> String
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

generateImport :: Module -> String
generateImport m =  "import " ++ (moduleName m) ++ "\n"

generateMain :: UnevalHandler -> Proposition -> Trace -> (UID->Event) -> UID -> String -> String
generateMain handler prop trc getEvent i f
  = "main = Hoed.runOstore \"" ++ (propName prop) ++"\" $ " ++ propVarBind (foldl accArg ((propName prop)++ " ",propVars0) (argMap prop)) prop ++ "\n" -- was reverse . argMap $ prop
    where 
    accArg :: (String,PropVars) -> Int -> (String,PropVars)
    accArg (acc,propVars) x = let (s,propVars') = getArg x propVars in (acc ++ " " ++ s, propVars')
    getArg :: Int -> PropVarGen String
    getArg x
      | x < 0             = cf
      | x < (length args) = args !! x
      | otherwise         = propVarError

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
  | change e == Fun = case dfsChildren frt e of [_,_,_,mr] -> generateRes' unevalGen trc getEvent mr
                                                as  -> error ("generateRes': Fun with " ++ show (length as) ++ " event children!")
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
