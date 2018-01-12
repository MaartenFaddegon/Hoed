-- This file is part of the Haskell debugger Hoed.
--
-- Copyright (c) Maarten Faddegon, 2016
{-# LANGUAGE DeriveGeneric #-}

module Debug.Hoed.Serialize
( storeJudgements
, restoreJudgements
, storeTree
, restoreTree
, storeTrace
, restoreTrace
) where
import Debug.Hoed.Observe
import Prelude hiding (lookup,Right)
import qualified Prelude as Prelude
import Data.Frame
import Debug.Hoed.CompTree
import Debug.Hoed.Render(CompStmt(..), StmtDetails(..))
import Data.Serialize
import qualified Data.ByteString as BS
import GHC.Exts (IsList(..))
import GHC.Generics
import Data.Graph.Libgraph(Judgement(..),AssistedMessage(..),mapGraph,Graph(..),Arc(..))

--------------------------------------------------------------------------------
-- Derive Serialize instances

-- Orphan instances
instance (Serialize a, Serialize b) => Serialize (Graph a b)
instance (Serialize a, Serialize b) => Serialize (Arc a b)
instance Serialize Vertex
instance Serialize Judgement
instance Serialize AssistedMessage
instance Serialize CompStmt
instance Serialize StmtDetails
instance Serialize Parent
instance Serialize Event
instance Serialize Change
instance Serialize a => Serialize (Frame a) where
  put = put . toList
  get = fromList <$> get

--------------------------------------------------------------------------------
-- Tree

storeTree :: FilePath -> CompTree -> IO ()
storeTree fp = (BS.writeFile fp) . encode

restoreTree :: FilePath -> IO (Maybe CompTree)
restoreTree fp = do
        bs <- BS.readFile fp
        case decode bs of
          (Prelude.Left _)   -> return Nothing
          (Prelude.Right x) -> return (Just x)

--------------------------------------------------------------------------------
-- Trace

storeTrace :: FilePath -> Trace -> IO ()
storeTrace fp = (BS.writeFile fp) . encode

restoreTrace :: FilePath -> IO (Maybe Trace)
restoreTrace fp = do
        bs <- BS.readFile fp
        case decode bs of
          (Prelude.Left _)   -> return Nothing
          (Prelude.Right x) -> return (Just x)

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
