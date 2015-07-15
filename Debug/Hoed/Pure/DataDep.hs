-- This file is part of the Haskell debugger Hoed.
--
-- Copyright (c) Maarten Faddegon, 2015

module Debug.Hoed.Pure.DataDep 
( ConstantValue(..)
, ConstantTree(..)
, CVArc(..)
, constants
, mkDDDT
, mkResDepTree
, spans
)where
import Debug.Hoed.Pure.Observe
import Debug.Hoed.Pure.EventForest
import Data.Graph.Libgraph
import Data.Maybe(mapMaybe)
import Data.List(sortBy,nub)
import Data.Ord (comparing)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import qualified Debug.Trace as Debug

--------------------------------------------------------------------------------
-- Constant Values 

data ConstantValue = ConstantValue { valStmt :: UID, valLoc :: Location
                                   , valMin :: UID,  valMax :: UID }
                   | CVRoot
                  deriving Eq


type ConstantTree = Graph ConstantValue ()
type CVArc = Arc ConstantValue ()

instance Show ConstantValue where
  show CVRoot = "Root"
  show v = "Stmt-" ++ (show . valStmt $ v)
         ++ "-"    ++ (show . valLoc  $ v) 
         ++ ": "   ++ (show . valMin  $ v) 
         ++ "-"    ++ (show . valMax  $ v) 

shwCT :: ConstantTree -> String
shwCT g = showWith g shwCV (\_ -> "")

shwCV :: ConstantValue -> (String,String)
shwCV v = (show (valLoc v) ++ "_" ++ show (valStmt v), "")

----

type Span = ConstantValue
data SpanState = SpanState
  { appRefs        :: IntMap UID    
                   -- references from the UID of an event to the UID of the corresponding toplevel app event
  , enterEvents    :: IntMap [(ParentPosition,UID)] 
                   -- reference from parent UID (and ParentPosition) to the UID of enter event
  , locations      :: IntMap (ParentPosition -> Bool)
                   -- reference from parent UID and position to location
  , appStack       :: [UID]
  , storedStack    :: IntMap [UID]
                   -- reference from parent UID and position to previous stack
  , dependencies   :: [(UID,UID)]
  , collectedSpans :: [Span]
  }                -- the result

spans :: Trace -> [(UID,UID)]
spans trc = dependencies $ foldl loop s0 trc

  where s0 :: SpanState
        s0 = SpanState IntMap.empty IntMap.empty IntMap.empty [] IntMap.empty [] []

        cs :: IntMap [ParentPosition]
        cs = cons trc

        loop :: SpanState -> Event -> SpanState
        loop s e = let i = (eventUID e); j = (parentUID . eventParent $ e); p = parentPosition . eventParent $ e
                       (Just locFun) = (IntMap.lookup j (locations s)); loc = locFun p
                       a = case IntMap.lookup j (appRefs s) of Nothing -> j; (Just a') -> a'
                       stk = appStack s
                   in case change e of

                Observe{} -> s{locations=IntMap.insert i (\_->True) (locations s)}

                Fun{} -> s{locations=IntMap.insert i (\q->case q of 0 -> not loc; 1 -> loc) (locations s)
                          ,appRefs=case IntMap.lookup j (appRefs s) of
                             Nothing  -> IntMap.insert i i (appRefs s) -- This Fun is at the top of the tree
                             (Just a) -> IntMap.insert i a (appRefs s)
                          }

                Enter{} -> case IntMap.lookup j cs of
                        Nothing   -> s  -- We only consider Enter events with a corresponding Cons event
                        (Just ps) -> if not (p `elem` ps) then s else
                           s{appRefs=IntMap.insert i a (appRefs s)
                            ,enterEvents=insertCon j (p,i) (enterEvents s)
                            ,appStack=case loc of     True  -> a : stk
                                                      False -> if stk == [] then stk else tail stk
                            ,dependencies=case loc of True  -> case stk of []    -> (-1,a) : (dependencies s)
                                                                           (n:_) -> (n,a)  : (dependencies s)
                                                      False -> (dependencies s)
                            ,storedStack=IntMap.insert i stk (storedStack s)
                            }

                Cons{} -> case (IntMap.lookup j (enterEvents s)) >>= (lookup p) of
                                Nothing  -> error $ "Constant without enter event: " ++ show e
                                (Just k) -> s{appRefs=IntMap.insert i a (appRefs s)
                                             -- ,collectedSpans=(ConstantValue a loc k i):(collectedSpans s)
                                             ,locations=IntMap.insert i (\_->loc) (locations s)
                                             ,appStack=case loc of True  -> if stk == [] then stk else tail stk
                                                                   False -> case IntMap.lookup k (storedStack s) of (Just stk') -> stk'
                                             }

insertCon :: Int -> a -> IntMap [a] -> IntMap [a]
insertCon k x = IntMap.insertWith (\[x] xs->x:xs) k [x]

cons :: Trace -> IntMap [ParentPosition]
cons = foldl loop IntMap.empty
  where loop :: IntMap [ParentPosition] -> Event -> IntMap [ParentPosition]
        loop m e = case change e of
          Cons{} -> insertCon (parentUID . eventParent $ e) (parentPosition . eventParent $ e) m
          _      -> m

----

constants :: EventForest -> Event -> [ConstantValue]
constants frt r = dfsFold Prefix pre idVisit [] Trunk (Just r) frt
  where pre :: Visit [ConstantValue]
        pre Nothing  _ vs = vs
        pre (Just e) loc vs
          | isConstant e      = mkConstantValue e loc : vs
          | otherwise         = vs

        mkConstantValue :: Event -> Location -> ConstantValue
        mkConstantValue e loc = ConstantValue (eventUID . findApp $ e) loc
                                              (eventUID . enterEventOf frt $ e) (eventUID e) 

        apps :: [Event]
        apps = topLevelApps frt r

        findApp :: Event -> Event
        findApp e = case filter (\a -> e `elem` (eventsInTree frt a)) apps of
                     []    -> r -- no matching apps, this must be an observed constant,
                                -- use the root instead
                     (a:_) -> a

isConstant :: Event -> Bool
isConstant e = 
        case change e of Cons{} -> True; _ -> False

enterEventOf :: EventForest -> Event -> Event
enterEventOf frt e = case filter isEnter siblings of 
  []    -> error "enterEventOf: No siblings found!"
  [ent] -> ent
  es    -> error $ "enterEventOf: More than one sibling found!\n" 
                 ++ unlines (map (\e -> show e ++ (if e `elem` es then "*" else "")) siblings)

  where p :: Parent
        p = eventParent e

        siblings :: [Event]
        siblings = parentPosLookup (parentPosition p) . parentUIDLookup (parentUID p) $ frt

        isEnter :: Event -> Bool
        isEnter e'
          | change e' == Enter = True
          | otherwise          = False

--------------------------------------------------------------------------------
-- Dynamic Data Dependency Tree

type SpanDep = (Span,Span)
type SpanStack = [Span]
data SpanPointer = SpanStart UID Span | SpanEnd UID Span

mkDDDT :: [ConstantValue] -> ConstantTree
mkDDDT ss = Graph CVRoot (CVRoot : ss) (map (\(s,t) -> Arc s t ()) . nub $ ds)
  where ds = mkDD [] (worklist ss) []

mkDD :: [SpanDep] -> [SpanPointer] -> SpanStack -> [SpanDep]

mkDD ds [] _ = ds

mkDD ds ((SpanStart i s):ps) []      = mkDD ((CVRoot,s):ds) ps [s]
mkDD ds ((SpanStart i s):ps) (t:stk) = mkDD ((t,s):ds)      ps (s:t:stk)

mkDD ds ((SpanEnd i s):ps) []      = mkDD ds ps []
mkDD ds ((SpanEnd i s):ps) (t:stk) = mkDD ds ps stk

worklist :: [ConstantValue] -> [SpanPointer]
worklist ss = sortBy (comparing uid) (map mkStart ss ++ map mkEnd ss)

  where mkStart s = SpanStart (valMin s) s
        mkEnd s   = SpanStart (valMax s) s
        uid (SpanStart i _) = i
        uid (SpanEnd i _)   = i




{-- ouwe meuk

mkDDDT :: [ConstantValue] -> ConstantTree
mkDDDT vs = Graph CVRoot (CVRoot : vs) (as ++ as')
  where as  = mapMaybe (maybeDepends vs) vs         -- O(N^2) with N number of constant spans vs
        as' = map (\r -> Arc CVRoot r()) rs
        rs  = filter (notEnclosed vs) vs            -- O(N^2) with N number of constant spans vs

-- O(N) with N number of constant spans vs
notEnclosed :: [ConstantValue] -> ConstantValue -> Bool
notEnclosed vs v = all (not . (flip encloses) v) vs

-- O(N) with N number of constant spans
maybeDepends :: [ConstantValue] -> ConstantValue -> Maybe (CVArc)
maybeDepends vs v = do
  w <- strictlyEnclosing v vs
  return $ Arc w v ()

-- O(1)
encloses :: ConstantValue -> ConstantValue -> Bool
encloses v w = valMin v < valMin w && valMax v > valMax w

-- O(N) with N number of constant spans vs
strictlyEnclosing :: ConstantValue -> [ConstantValue] -> Maybe ConstantValue
strictlyEnclosing v vs = case filter (flip encloses $ v) vs of
  [] -> Nothing
  ws -> Just . head . sortBy (comparing minMaxDiff) $ ws

minMaxDiff :: ConstantValue -> Int
minMaxDiff v = (valMax v) - (valMin v)

-}

--------------------------------------------------------------------------------
-- Last Open Result Dependency Tree

mkResDepTree :: ConstantTree -> ConstantTree
mkResDepTree ddt = Graph (root ddt) 
                         (filter resOrRoot $ vertices ddt) 
                         (visit [CVRoot] (succs ddt $ root ddt) [])

  where -- visit list of children
        visit :: CVStack -> [ConstantValue] -> [CVArc] -> [CVArc]
        visit cvs vs as = foldl (\as' v -> visit' cvs v as') as vs

        -- visit one child
        visit' :: CVStack -> ConstantValue -> [CVArc] -> [CVArc]
        visit' cvs v as
          | (isResult . valLoc) v = let as' = Arc (peekCVS cvs) v () : as
                                    in  visit (pushCVS cvs v) (succs ddt v) as'
          | otherwise             =     visit (popCVS cvs)    (succs ddt v) as

        resOrRoot :: ConstantValue -> Bool
        resOrRoot CVRoot = True
        resOrRoot v = isResult . valLoc $ v
        
        isResult :: Location -> Bool
        isResult Trunk          = True
        isResult (ResultOf l)   = isResult l
        isResult (ArgumentOf _) = False
        isResult (FieldOf _ l)  = isResult l

type CVStack = [ConstantValue]

pushCVS :: CVStack -> ConstantValue -> CVStack
pushCVS cvs r = r : cvs

popCVS :: CVStack -> CVStack
popCVS []      = []
popCVS (_:cvs) = cvs

popMatchCVS :: CVStack -> ConstantValue -> CVStack
popMatchCVS []      _ = error "Pop empty Constant Value Stack!"
popMatchCVS (r:cvs) a = case (valLoc r, valLoc a) of 
  (ResultOf rloc, ArgumentOf aloc) -> if rloc == aloc then cvs else err
  _                                -> err
  where err = error "Constant Value Stack mismatch on pop!"

peekCVS :: CVStack -> ConstantValue
peekCVS []     = CVRoot
peekCVS (cv:_) = cv


