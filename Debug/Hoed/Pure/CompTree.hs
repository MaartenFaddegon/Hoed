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
mkCompTree cs ds = Graph r (r:vs) as

  where r  = RootVertex
        vs = map (\cs -> Vertex cs Unassessed) cs
        as = map (\(i,j) -> Arc (findVertex i) (findVertex j) ()) (nub ds)

        vMap :: IntMap Vertex
        vMap = foldl (\m c -> let v = Vertex c Unassessed in foldl (\m' i -> IntMap.insert i v m') m (stmtUIDs c)) IntMap.empty cs

        findVertex :: UID -> Vertex
        findVertex (-1) = RootVertex
        findVertex a = case IntMap.lookup a vMap of
          Nothing  -> error $ "findCompStmt: cannot find a statement with UID " ++ show a
          (Just v) -> v

        -- findVertex a = case filter (\c -> a `elem` stmtUIDs c) cs of
        --         []    -> error $ "findCompStmt: cannot find a statement with UID " ++ show a
        --         (c:_) -> Vertex c Unassessed


 --Graph r (nub vs) (nub as)
 -- where Graph r vs as = mapGraph findVertex ddt

 --       findVertex :: ConstantValue -> Vertex
 --       findVertex CVRoot = RootVertex
 --       findVertex v      = Vertex (findCompStmt cs v) Unassessed

findCompStmt :: [CompStmt] -> ConstantValue -> CompStmt
findCompStmt cs v = case filter (\c -> valStmt v `elem` stmtUIDs c) cs of
  []    -> error $ "findCompStmt: cannot find a statement with value '" ++ show v ++ "'"
  (c:_) -> c
