-- This file is part of the Haskell debugger Hoed.
--
-- Copyright (c) Maarten Faddegon, 2015

module Debug.Hoed.EventForest 
( EventForest(..)
, mkEventForest
, parentUIDLookup
, parentPosLookup

, InfixOrPrefix(..)
, Location(..)
, Visit
, dfsFold
, idVisit

, treeUIDs
, topLevelApps
, eventsInTree
, dfsChildren
, elems
) where
import Debug.Hoed.Observe
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

-- Searchable mapping from UID of the parent and position in list of siblings
-- to a child event.
-- type EventForest = [(UID, [(ParentPosition, Event)])]
type EventForest = IntMap [(ParentPosition, Event)]

isRoot :: Event -> Bool
isRoot e = case change e of Observe{} -> True; _ -> False

elems :: EventForest -> [[(ParentPosition, Event)]]
elems = IntMap.elems

addEvent :: EventForest -> Event -> EventForest
addEvent frt e
  | isRoot e  = frt
  | otherwise = IntMap.insert i s frt
  where i  = parentUID . eventParent $ e
        p  = parentPosition . eventParent $ e
        ms = IntMap.lookup i frt
        s  = case ms of Nothing   -> [(p,e)]
                        (Just s') -> (p,e) : s'


mkEventForest :: Trace -> EventForest
mkEventForest trc = foldl addEvent IntMap.empty trc

parentUIDLookup :: UID -> EventForest  -> [(ParentPosition,Event)]
parentUIDLookup i frt = case IntMap.lookup i frt of
  Nothing   -> []
  (Just es) -> es

parentPosLookup :: ParentPosition -> [(ParentPosition,Event)] -> [Event]
parentPosLookup p = map snd . filter ((==p) . fst)

data InfixOrPrefix = Infix | Prefix

data Location = Trunk | ArgumentOf Location | ResultOf Location | FieldOf Int Location
  deriving Eq

instance Show Location where 
   show Trunk            = ""
   show (ArgumentOf loc) = 'a' : show loc
   show (ResultOf   loc) = 'r' : show loc
   show (FieldOf n  loc) = 'f' : show n ++ show loc

type Visit a = Maybe Event -> Location -> a -> a

idVisit :: Visit a
idVisit _ _ z = z

-- Given an event, return the list of (expected) children in depth-first order.
--
-- Nothing indicates that we expect an event (e.g. the argument of an application-
-- event) but it was not there.
--
-- An abstraction (LamEvent) can have more than one application. There is no
-- particular ordering and we just return the applications (AppEvents) in the
-- order we find them in the trace (i.e. evaluation order).

dfsChildren :: EventForest -> Event -> [Maybe Event]
dfsChildren frt e = case change e of
    Enter{}              -> manyByPosition 0 -- Should be Nothing?
    (Cons l _)           -> foldl (\acc x -> acc ++ manyByPosition x) [] [0..(l-1)]
    ConsChar _           -> []
    Observe{}            -> manyByPosition 0
    Fun                  -> manyByPosition 0 ++ manyByPosition 1

  where manyByPosition :: ParentPosition -> [Maybe Event]
        manyByPosition pos = case filter (\(pos',_) -> pos == pos') cs of
          [] -> [Nothing]
          ts -> map (Just . snd) ts

        -- Events in the frt that list our event as parent (in no particular order).
        cs :: [(ParentPosition,Event)]
        cs = parentUIDLookup (eventUID e) frt

        
dfsFold :: InfixOrPrefix -> Visit a -> Visit a -> a 
        -> Location -> (Maybe Event) -> EventForest -> a

dfsFold ip pre post z loc me frt 
  = post me loc $ case me of
      Nothing -> z'
      (Just e) -> case change e of

        Fun -> let [arg,res] = cs
          in case ip of
            Prefix -> csFold $ zip cs [ArgumentOf loc,ArgumentOf loc,ResultOf loc,ResultOf loc]

            Infix  -> let z1 = dfsFold ip pre post z (ArgumentOf loc) arg frt
                          z2 = pre me loc z1
                      in  dfsFold ip pre post z2 (ResultOf loc) res frt

        Cons{} -> csFold $ zip cs $ map (\i -> FieldOf i loc) [1..]

        _ -> csFold $ zip cs (repeat loc)

  where z'  = pre me loc z

        cs :: [Maybe Event]
        cs = case me of (Just e) -> dfsChildren frt e; Nothing -> error "dfsFold filter failed"

        csFold = foldl (\z'' (c,loc') -> dfsFold ip pre post z'' loc' c frt) z'

treeUIDs :: EventForest -> Event -> [UID]
treeUIDs frt = (map eventUID) . eventsInTree frt

-- Given an event r, return depth first ordered list of events in the (sub)tree starting from r.
eventsInTree :: EventForest -> Event -> [Event]
eventsInTree frt r = reverse $ dfsFold Prefix add idVisit [] Trunk (Just r) frt
  where add (Just e) _ es = e : es
        add Nothing  _ es = es

-- Find all toplevel AppEvents for RootEvent r
topLevelApps :: EventForest -> Event -> [Event]
topLevelApps frt r = foldl appendApp []  $ dfsChildren frt r

appendApp :: [Event] -> Maybe Event -> [Event]
appendApp z me = case me of
  Nothing  -> z
  (Just e) -> case change e of Fun -> e : z
                               _   -> z
  
