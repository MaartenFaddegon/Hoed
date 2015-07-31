-- This file is part of the Haskell debugger Hoed.
--
-- Copyright (c) Maarten Faddegon, 2015
module Debug.Hoed.Pure.CompTree
( CompTree
, Vertex(..)
, mkCompTree
, isRootVertex
, vertexUID
, leafs
)where

import Debug.Hoed.Pure.Render
import Debug.Hoed.Pure.Observe
import Debug.Hoed.Pure.EventForest
import Debug.Hoed.Pure.DataDep

import Prelude hiding (Right)
import Data.Graph.Libgraph
import Data.List(nub)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

data Vertex = RootVertex | Vertex {vertexStmt :: CompStmt, vertexJmt :: Judgement} 
  deriving (Eq,Show,Ord)

vertexUID :: Vertex -> UID
vertexUID RootVertex   = -1
vertexUID (Vertex s _) = equIdentifier s

type CompTree = Graph Vertex ()

isRootVertex :: Vertex -> Bool
isRootVertex RootVertex = True
isRootVertex _          = False

leafs :: CompTree -> [Vertex]
leafs g = filter (\v -> succs g v == []) (vertices g)

--------------------------------------------------------------------------------
-- Computation Tree

-- One computation statement can have multiple result-events. Sometime these add
-- new information, often the information is just a duplicate of what we already
-- know. With nub we remove the duplicates.

mkCompTree :: [CompStmt] -> [(UID,UID)] -> CompTree
mkCompTree cs ds = Graph RootVertex (vs) as

  where vs = RootVertex : map (\cs -> Vertex cs Unassessed) cs

        as :: [Arc Vertex ()]
        as  = IntSet.fold (\i -> (:) (Arc RootVertex (findVertex i) ())) as' roots
        as' = map (\(i,j) -> Arc (findVertex i) (findVertex j) ()) (nub ds)

        roots :: IntSet
        roots = foldl (\s (_,j) -> IntSet.delete j s) uids ds

        uids :: IntSet
        uids = foldl (\s (i,j) -> (IntSet.insert i) . (IntSet.insert j) $ s) IntSet.empty ds

        -- A mapping from stmtUID to Vertex of all CompStmts in cs
        vMap :: IntMap Vertex
        vMap = foldl (\m c -> let v = Vertex c Unassessed in foldl (\m' i -> IntMap.insert i v m') m (stmtUIDs c)) IntMap.empty cs

        -- Given an UID, get corresponding CompStmt (wrapped in a Vertex)
        findVertex :: UID -> Vertex
        findVertex (-1) = RootVertex
        findVertex a = case IntMap.lookup a vMap of
          Nothing  -> error $ "mkCompTree: cannot find a statement with UID " ++ show a
          (Just v) -> v
