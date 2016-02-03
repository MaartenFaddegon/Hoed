-- This file is part of the Haskell debugger Hoed.
--
-- Copyright (c) Maarten Faddegon, 2016
{-# LANGUAGE DeriveGeneric #-}

module Debug.Hoed.Pure.Serialize
( storeJudgements
, restoreJudgements
, storeTree
, restoreTree
) where
import Prelude hiding (lookup,Right)
import qualified Prelude as Prelude
import Debug.Hoed.Pure.CompTree
import Debug.Hoed.Pure.Render(CompStmt(..))
import Data.Serialize
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import GHC.Generics
import Data.Graph.Libgraph(Judgement(..),AssistedMessage(..),mapGraph,Graph(..),Arc(..))

--------------------------------------------------------------------------------
-- Derive Serialize instances

instance (Serialize a, Serialize b) => Serialize (Graph a b)
instance (Serialize a, Serialize b) => Serialize (Arc a b)
instance Serialize Vertex
instance Serialize Judgement
instance Serialize AssistedMessage
instance Serialize CompStmt

--------------------------------------------------------------------------------
-- Tree

storeTree :: FilePath -> CompTree -> IO ()
storeTree fp = (BS.writeFile fp) . encode

restoreTree :: FilePath -> IO (Maybe CompTree)
restoreTree fp = do
        bs <- BS.readFile fp
        case decode bs of
          (Prelude.Left _)   -> return Nothing
          (Prelude.Right ct) -> return (Just ct)

--------------------------------------------------------------------------------
-- Judgements

storeJudgements :: FilePath -> CompTree -> IO ()
storeJudgements fp = (BS.writeFile fp) . encode . (foldl insert empty) . vertices

restoreJudgements :: FilePath -> CompTree -> IO CompTree
restoreJudgements fp ct = do
        bs <- BS.readFile fp
        case decode bs of
          (Prelude.Left _)   -> return ct
          (Prelude.Right db) -> return $ mapGraph (restore db) ct

restore :: DB -> Vertex -> Vertex
restore db v = case lookup db v of
  (Just Right) -> setJudgement v Right
  (Just Wrong) -> setJudgement v Wrong
  _            -> v

data DB = DB [(String, Judgement)] deriving (Generic)
instance Serialize DB

empty :: DB
empty = DB []

lookup :: DB -> Vertex -> Maybe Judgement
lookup (DB db) v = Prelude.lookup (key v) db

insert :: DB -> Vertex -> DB
insert (DB db) v = case judgement v of
  Nothing  -> DB db
  (Just j) -> DB ((key v, j) : db)

key :: Vertex -> String
key = vertexRes

judgement :: Vertex -> Maybe Judgement
judgement RootVertex = Nothing
judgement v = case vertexJmt v of
  Right -> Just Right
  Wrong -> Just Wrong
  _     -> Nothing
