-- This file is part of the Haskell debugger Hoed.
--
-- Copyright (c) Maarten Faddegon 2015-2016

{-# LANGUAGE OverloadedLists, DefaultSignatures, TypeOperators, FlexibleContexts, FlexibleInstances, StandaloneDeriving, CPP, DeriveGeneric #-}

module Debug.Hoed.Prop where
-- ( judge
-- , Propositions(..)
-- ) where
import qualified Data.Vector.Generic as VG
import Debug.Hoed.Observe(Observable(..),Trace(..),UID,Event(..),Change(..),ourCatchAllIO,evaluate,eventParent,parentPosition)
import Debug.Hoed.Render(CompStmt(..),noNewlines)
import Debug.Hoed.CompTree(CompTree,Vertex(..),Graph(..),vertexUID,vertexRes,replaceVertex,getJudgement,setJudgement)
import Debug.Hoed.EventForest(EventForest,mkEventForest,dfsChildren)
import Debug.Hoed.Serialize
import qualified Data.IntMap as M
import Prelude hiding (Right)
import Data.Graph.Libgraph(Judgement(..),AssistedMessage(..),mapGraph)
import System.Directory(createDirectoryIfMissing)
import System.Process(system)
import System.Exit(ExitCode(..))
import System.IO(hPutStrLn,stderr)
import System.IO.Unsafe(unsafePerformIO)
import Data.Char(isAlpha)
import Data.Maybe(isNothing,fromJust,isJust)
import Data.List(intersperse,isInfixOf,foldl1)
import GHC.Generics hiding (moduleName) --(Generic(..),Rep(..),from,(:+:)(..),(:*:)(..),U1(..),K1(..),M1(..))
import Control.Monad(foldM)

------------------------------------------------------------------------------------------------------------------------

data Propositions = Propositions { propositions :: [Proposition]
                                 , propType :: PropType
                                 , funName :: String
                                 , extraModules :: [Module]
                                 }

data PropType = Specify | PropertiesOf deriving Eq

data Signature 
  = Argument Int
  | SubjectFunction
  | Random
  deriving (Show,Eq)

data TestGen = TestGenQuickCheck | TestGenLegacyQuickCheck -- TestGenSmallCheck
  deriving Show

data Proposition = Proposition { propositionType :: PropositionType
                               , propModule      :: Module
                               , propName        :: String
                               , signature       :: [Signature]
                               , maxSize         :: Maybe Int
                               , testgen         :: TestGen
                               } deriving Show

mkProposition :: Module -> String -> Proposition
mkProposition m f = Proposition { propositionType = BoolProposition
                                , propModule      = m
                                , propName        = f
                                , signature       = [SubjectFunction,Argument 0]
                                , maxSize         = Nothing
                                , testgen         = TestGenQuickCheck
                                }

ofType :: Proposition -> PropositionType -> Proposition
ofType p t = p{propositionType = t}

withSignature :: Proposition -> [Signature] -> Proposition
withSignature p s = p{signature = s}

sizeHint :: Proposition -> Int -> Proposition
sizeHint p n = p{maxSize = Just n}

withTestGen :: Proposition -> TestGen -> Proposition
withTestGen p f = p{testgen=f}

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


data Judge 
  = Judge Judgement
    -- ^ Returns a Judgement (see Libgraph library).
  | AlternativeTree CompTree Trace
    -- ^ Found counter example with simpler computation tree.

-- TODO: review this function, not sure if complexity suggestion actually makes sense there...
judgeAll :: UnevalHandler -> (CompTree -> Int) -> Trace -> [Propositions] -> CompTree -> IO CompTree
judgeAll handler complexity trc ps compTree = foldM f compTree (vertices compTree)
  where
  c = complexity compTree
  f :: CompTree -> Vertex -> IO CompTree
  f curTree v = do
    putStrLn $ take 50 (cycle "-")
    putStrLn $ "Evaluating properties to judge statement: " ++ vertexRes v
    putStrLn $ take 50 (cycle "-")
    case lookupPropositions ps v of
      Nothing  -> do
        putStrLn "*** no propositions"
        return curTree
      (Just p) -> do
        j <- judge' handler trc p v complexity curTree
        case j of
          (Judge jmt)            -> do
            putStrLn $ "*** judgement is " ++ show jmt
            return $ replaceVertex curTree (setJudgement v jmt)
          (AlternativeTree t' _) -> do
             let msg = "Simpler tree suggested with complexity " ++ show (complexity t') 
                       ++ "(current tree has complexity of " ++ show c ++ ")"
             putStrLn $ "*** " ++ msg
             return $ replaceVertex curTree (setJudgement v (Assisted [InconclusiveProperty msg]))


-- |Use propositions to judge a computation statement.
-- First tries restricted and bottom for unevaluated expressions,
-- then unrestricted, and finally with randomly generated values 
-- for unevaluated expressions.
judge :: Trace -> Propositions -> Vertex -> (CompTree -> Int) -> CompTree -> IO Judge
judge trc p v complexity curTree = do
  putStrLn $ take 50 (cycle "-")
  putStrLn $ "Evaluating properties to judge statement: " ++ vertexRes v
  putStrLn $ take 50 (cycle "-")
  putStrLn "### ATTEMPT 1: with a restricted subject function\n"
  res1 <- judge' RestrictedBottom trc p v complexity curTree
  case res1 of
    (Judge (Assisted _)) -> do 
      putStrLn "### ATTEMPT 2: with an unrestricted subject function\n"
      res2 <- judge' Bottom trc p v complexity curTree
      case res2 of
        (Judge (Assisted _)) -> do 
          putStrLn "### ATTEMPT 3: with randomly generated values\n"
          judge' Forall trc p v complexity curTree
        _                    -> return res2
    (Judge _) -> return res1
  return res1

judge' RestrictedBottom trc p v complexity curTree = do
  pas <- evalPropositions RestrictedBottom trc p v
  let j | propType p == Specify && all holds pas  = (Judge Right)
        | any disproves pas                       = (Judge Wrong)
        | otherwise                               = advice pas
  return j

judge' handler trc p v complexity curTree = do
  pas <- evalPropositions handler trc p v
  let j | propType p == Specify && all holds pas  = return (Judge Right)
        | any disproves pas                       = do 
            let curComplexity = complexity curTree
            (bestComplexity,bestTree,bestTrace) <- simplestTree complexity (extraModules p) pas (curComplexity,curTree,[]) trc v
            if bestComplexity == curComplexity
              then return $ Judge $ Assisted $ [InconclusiveProperty $ "We found values for the unevaluated "
                                                ++ "expressions in the current statement that falsify\n"
                                                ++ "a property, however the resulting tree is not simpler."]
              else return (AlternativeTree bestTree bestTrace)
        | otherwise = return (advice pas)
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
simplestTree complexity ms rs cur trc v = foldM (simple2 complexity trc v ms) cur (filter disproves rs)

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
      putStrLn $ "Discovered new tree with complexity " ++ show candComplexity 
                 ++ " (current tree is " ++ show curComplexity ++ ")"
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

data UnevalHandler = RestrictedBottom | Bottom | Forall | FromList [String] deriving (Eq, Show)

unevalHandler :: UnevalHandler -> PropVarGen String
unevalHandler RestrictedBottom = propVarError
unevalHandler Bottom           = propVarError
unevalHandler Forall           = propVarFresh
unevalHandler (FromList _)     = propVarFresh

unevalState :: UnevalHandler -> PropVars
unevalState RestrictedBottom  = propVars0
unevalState Bottom            = propVars0
unevalState Forall            = propVars0
-- unevalState (FromList _)      = propVars0
-- Replace last line with 
unevalState (FromList values) = ([],values)
-- to directly substitute unevaluated expressions with the FromList values.
-- This is only valid when all randomly generated values are actually substituting
-- unevaluated expressions (this is not always the case, consider e.g. testing f in
-- quickCheck p where p f x y = f x == g x y)

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
evalProposition RestrictedBottom trc v ms prop | not (SubjectFunction `elem` (signature prop)) = do
  putStrLn $ "property " ++ propName prop ++ ": Cannot restrict subject function!"
  return $ Error prop "Cannot restrict subject function!"
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
reEvalProposition (Disprove prop) _ _ _ = return (Disprove prop) -- no need to re-evaluate
reEvalProposition (DisproveBy prop values) trc v ms = do
  putStrLn $ "RE-EVALUATE with " ++ concat valuesInBrackets ++ " {"
  (Disprove p) <- evalProposition (FromList valuesInBrackets) trc v ms prop
  putStrLn $ "} RE-EVALUATE"
  return (Disprove p)
  where
  valuesInBrackets = map (\s -> " (" ++ s ++ ") ") values

clean = system $ "rm -f " ++ sourceFile ++ " " ++ exeFile ++ " " ++ buildFiles

compile prop = do
  putStrLn cmd
  system cmd
  where cmd = "ghc -dynamic -i" ++ (searchPath . propModule) prop ++ " -o " ++ exeFile ++ " " ++ sourceFile ++ " > " ++ errFile ++ " 2>&1"

run = system $ exeFile ++ " > " ++ outFile ++ " 2>&1"

generateCode :: UnevalHandler -> Trace -> Vertex -> Proposition -> [Module] -> IO String
generateCode handler trc v prop ms = do 
  -- Uncomment the next line to dump generated program on screen
  -- hPutStrLn stderr $ "Generated the following program ***\n" ++ prgm ++ "\n***" 
  writeFile sourceFile prgm
  return prgm
  where 
  prgm = generate handler prop ms trc ((trc VG.!) . pred) i f
  i    = (stmtIdentifier . vertexStmt) v
  f    = (stmtLabel . vertexStmt) v

mkPropRes :: Proposition -> ExitCode -> String -> PropRes
mkPropRes prop (ExitFailure _) out        = Error prop out
mkPropRes prop ExitSuccess out
  | out == "True\n"                       = Hold prop
  | out == "+++ OK, passed 100 tests.\n"  = HoldWeak prop
  | out == "False\n"                      = Disprove prop
  | "Falsifiable" `isInfixOf` out         = DisproveBy prop (reverse . tail . lines $ out)
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
propVars0 = ([], map show [1..])

propVarError :: PropVarGen String
-- propVarError = propVarReturn "(error \"Request of value that was unevaluated in original program.\")"
propVarError (bvs,v:fvs) = (x, (bvs,fvs)) 
 where x = "(error \"Request of value that was unevaluated in original program (underscore " ++ v ++ " in computation statement).\")"

propVarFresh :: PropVarGen String
propVarFresh (bvs,v:fvs) = (x, (x:bvs,fvs)) where x = 'x':v

propVarReturn :: String -> PropVarGen String
propVarReturn s vs = (s,vs)

propVarBind :: UnevalHandler -> (String,PropVars) -> Proposition -> String
propVarBind (FromList _) (propApp,_)       prop = generatePrint prop ++ propApp
propVarBind _            (propApp,([],_))  prop = generatePrint prop ++ propApp
propVarBind _            (propApp,(bvs,_)) prop = qc ++ " (\\" ++ bvs' ++ " -> " ++ propApp ++ ")"
  where
  bvs' = concat (intersperse " " bvs)
  qc = case (testgen prop, maxSize prop) of
         (TestGenQuickCheck, Nothing) 
           -> "quickCheckWith stdArgs{maxDiscardRatio=50}"
         (TestGenQuickCheck, Just n)
           -> "quickCheckWith stdArgs{maxDiscardRatio=50,maxSize=" ++ show n ++ "}"
         (TestGenLegacyQuickCheck, Nothing)
           ->  "check defaultConfig{configMaxFail=5000}"
         (TestGenLegacyQuickCheck, Just n)
           -> "check defaultConfig{configMaxFail=5000,configSize=(+" ++ show n ++ ") . (`div` 2)}"

generatePrint :: Proposition -> String
generatePrint p = case propositionType p of
  IOProposition         -> ""
  BoolProposition       -> "print $ "
  QuickCheckProposition -> "(\\q -> do MkRose res ts <- reduceRose .  unProp . (\\p->unGen p  (mkQCGen 1) 1) . unProperty $ q; print . fromJust . ok $ res) $ "
  LegacyQuickCheckProposition -> "do g <- newStdGen; print . fromJust . ok . (generate 1 g) . evaluate $ "

------------------------------------------------------------------------------------------------------------------------

generate :: UnevalHandler -> Proposition -> [Module ] -> Trace -> (UID->Event) -> UID -> String -> String
generate handler prop ms trc getEvent i f = generateHeading prop ms ++ generateMain handler prop trc getEvent i f

generateHeading :: Proposition -> [Module] -> String
generateHeading prop ms =
  "-- This file is generated by the Haskell debugger Hoed\n"
  ++ generateImport (propModule prop)
  ++ generateImport (Module "qualified Debug.Hoed as Hoed" [])
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
            -- ++ appValues handler
            ++ "\n"
    where 
    -- appValues (FromList values) = concat (map (" "++) values)
    -- appValues _                 = ""
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
    cf | handler == RestrictedBottom 
           = foldl1 (liftPV $ \acc c -> acc ++ " " ++ c)
             ([ propVarReturn $ "(" ++ genConAp (length args) f
             , generateRes (unevalHandler handler) trc getEvent i
             , propVarReturn $ ")"
             ] :: [PropVarGen String])
       | otherwise = propVarReturn f

generateRes :: (PropVarGen String) -> Trace -> (UID -> Event) -> UID -> PropVarGen String
generateRes unevalGen trc getEvent i
 | areFun mres = (propVarReturn " {- generateRes -} ") `pvCat`
                 (generateRes unevalGen trc getEvent (eventUID . head . justFuns $ mres)) -- (*)
 | otherwise   = case mres of [_,mr] -> generateRes' unevalGen trc getEvent mr
 --
 -- (*) MF TODO: can there be multiple funs in mres? what then?
 --
 where
 mres = filter isJustRes children
 mr = case mres of [_,e] -> e; _ -> Nothing
 children = dfsChildren frt e
 frt = (mkEventForest trc)
 e   = getEvent i

generateRes' :: PropVarGen String -> Trace -> (UID->Event) -> Maybe Event -> PropVarGen String
generateRes' unevalGen trc getEvent Nothing = unevalGen
generateRes' unevalGen trc getEvent (Just e)
 | change e == Fun = 
  case dfsChildren frt e of 
   [_,_    ,_,mr] -> 
    generateRes' unevalGen trc getEvent mr
   [Nothing,_,mr] -> 
    generateRes' unevalGen trc getEvent mr
   as  -> 
    error $ "generateRes': event " ++ show (eventUID e) 
     ++ ":FUN has " ++ show (length as) ++ " children!\nnamely: " ++ commas (map show as)
 | otherwise       = generateExpr unevalGen trc getEvent frt (Just e)
 where
 frt = (mkEventForest trc)

generateArgs :: (PropVarGen String) -> Trace -> (UID -> Event) -> UID -> [PropVarGen String]
generateArgs unevalGen trc getEvent i =
 (propVarReturn " {- generateArgs -} ") `pvCat`
 pvArg `pvCat` (propVarReturn $ " {- more: " ++ show mres ++ " -} ") 
 : moreArgs unevalGen trc getEvent mr
 where
 pvArg | areFun marg = generateFunMap unevalGen trc getEvent (justFuns marg)
       | otherwise   = case marg of
         [Nothing] -> unevalGen
         [_,ma]    -> generateExpr unevalGen trc getEvent frt ma
 e   = getEvent i
 marg = filter nothingOrArg children
 mres = filter isJustRes children
 mr = case mres of [_,e] -> e; _ -> Nothing
 children = dfsChildren frt e
 frt = (mkEventForest trc)

nothingOrArg Nothing = True
nothingOrArg (Just e) = isArg e

noArg :: [Maybe a] -> Bool
noArg [Nothing] = True
noArg _         = False

areFun :: [Maybe Event] -> Bool
areFun (_:e:_) = isJustFun e
areFun _       = False

justFuns :: [Maybe Event] -> [Event]
justFuns = map fromJust . filter isJustFun 

isJustFun :: Maybe Event -> Bool
isJustFun (Just e) = change e == Fun
isJustFun Nothing  = False

generateFunMap :: (PropVarGen String) -> Trace -> (UID -> Event) -> [Event] -> PropVarGen String
generateFunMap unevalGen trc getEvent funs 
 | length funs > 0 = caseOf `pvCat` (pvConcat cases') `pvCat` esac
 | otherwise       = propVarReturn "{- a fun without applications? -}"
 where 
 caseOf = propVarReturn $ " {- funmap with " ++ (show . length $ funs ) ++ " cases -} " 
                          ++ "(\\y -> case y of "
 esac   = propVarReturn ")"
 cases, cases' :: [PropVarGen String]
 cases  = map (\fun -> generateCase unevalGen trc getEvent fun) funs
 cases' = intersperse (propVarReturn "; ") cases

generateCase :: (PropVarGen String) -> Trace -> (UID -> Event) -> Event -> PropVarGen String
generateCase unevalGen trc getEvent fun =
 (propVarReturn $ " {- CASE " ++ show fun ++ " -} ") `pvCat`
 case args of 
  [] -> (propVarReturn "{- catchall -} _") --> res
  _  -> (foldl1 (liftPV $ \acc c -> acc ++ " " ++ c) args) --> res
 where
 args :: [PropVarGen String]
 args  = map (generateExpr (propVarReturn "_") trc getEvent frt)
         . filter (\(Just e) -> isArg e) . filter isJust . dfsChildren frt $ fun
 res :: PropVarGen String
 res  = generateRes unevalGen trc getEvent (eventUID fun)
 (-->) :: PropVarGen String -> PropVarGen String -> PropVarGen String 
 (-->) = liftPV $ \x y -> x ++ " -> " ++ y
 frt = mkEventForest trc -- MF TODO: create just once and share?

isArg = hasParentPos 0

isRes = hasParentPos 1

isJustRes (Just e) = isRes e
isJustRes Nothing  = False

hasParentPos i = (==i) . parentPosition . eventParent

pvCat :: PropVarGen String -> PropVarGen String -> PropVarGen String
pvCat = liftPV (++)

pvConcat :: [PropVarGen String] -> PropVarGen String
pvConcat = foldl pvCat (propVarReturn "")

moreArgs :: PropVarGen String -> Trace -> (UID->Event) -> Maybe Event -> [PropVarGen String]
moreArgs _ trc getEvent Nothing = []
moreArgs unevalGen trc getEvent (Just e)
  | change e == Fun = generateArgs unevalGen trc getEvent (eventUID e)
  | otherwise       = []

generateExpr :: PropVarGen String -> Trace -> (UID -> Event) -> EventForest -> Maybe Event -> PropVarGen String
generateExpr unevalGen _ _ _ Nothing    = unevalGen
generateExpr unevalGen trc getEvent frt (Just e) = 
  -- (propVarReturn $ "{- generateExpr " ++ show e ++ "-}") `pvCat` 
  case change e of
  (Cons _ s) -> let s' = if isAlpha (head s) then s else "(" ++ s ++ ")"
                in liftPV (++) ( foldl (liftPV $ \acc c -> acc ++ " " ++ c)
                                 (propVarReturn ("(" ++ s')) cs
                               ) 
                               ( propVarReturn ") "
                               )
  Enter      -> propVarReturn ""
  -- Fun        -> generateFunMap unevalGen trc getEvent (justFuns xs)
  evnt       -> propVarReturn $ "error \"cannot represent: " ++ show evnt ++ "\""

 where cs :: [PropVarGen String]
       cs = map (generateExpr unevalGen trc getEvent frt) xs
       xs = (dfsChildren frt e)


-- only works if there is 1 argument
conAp :: Observable b => (a -> b) -> b -> a -> b
conAp f r x = constrain (f x) r

genConAp :: Int -> String -> String
genConAp n f | n > 0 = "(\\r " ++ args ++ " -> Hoed.constrain (" ++ f ++ " " ++ args ++ ") r)"
  where args = foldl1 (\a x -> a ++ " " ++ x) xs
        xs = map (\i -> "x" ++ show i) [1..n]

--------------------------------------------------------------------------------
-- MF TODO: this should probably be part of the Observable class ...


(===) :: ParEq a => a -> a -> Bool
x === y = case parEq x y of (Just b) -> b
                            Nothing  -> error "might be equal"

class ParEq a where
  parEq :: a -> a -> Maybe Bool
  default parEq :: (Generic a, GParEq (Rep a)) => a -> a -> Maybe Bool
  parEq x y = gParEq (from x) (from y)

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
            where mbs = [(catchGEq x y) `seq` (catchGEq x y), (catchGEq x' y') `seq` (catchGEq x' y')] :: [Maybe Bool]

-- Unit: used for constructors without arguments
instance GParEq U1 where
#if __GLASGOW_HASKELL__ >= 710
  gParEq x y = catchEq x y
#else
  gParEq _ _ = Nothing
#endif

-- Constants: additional parameters and recursion of kind *
instance (ParEq a) => GParEq (K1 i a) where
  gParEq x y = let r = gParEq_ x y in r
    where gParEq_ (K1 x) (K1 y) = x `parEq` y

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
instance ParEq Int          where parEq x y = Just (x == y)
instance ParEq Bool         where parEq x y = Just (x == y)
instance ParEq Integer      where parEq x y = Just (x == y)
instance ParEq Float        where parEq x y = Just (x == y)
instance ParEq Double       where parEq x y = Just (x == y)
instance ParEq Char         where parEq x y = Just (x == y)
