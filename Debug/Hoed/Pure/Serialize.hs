-- This file is part of the Haskell debugger Hoed.
--
-- Copyright (c) Maarten Faddegon, 2016
{-# LANGUAGE DeriveGeneric #-}

module Debug.Hoed.Pure.Serialize
( storeJudgements
, restoreJudgements
) where
import Prelude hiding (lookup,Right)
import qualified Prelude as Prelude
import Debug.Hoed.Pure.CompTree
import Data.Serialize
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import GHC.Generics
import Data.Graph.Libgraph(Judgement(..),mapGraph)

--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------
-- The database

data DB = DB [(String, Judgement)] deriving (Generic)
instance Serialize DB
instance Serialize Judgement

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
