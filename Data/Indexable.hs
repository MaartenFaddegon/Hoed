{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
module Data.Indexable where

import Data.Foldable as F
import Data.Vector (Vector)
import qualified Data.Vector.Generic as V
import GHC.Exts

data Indexable a = Indexable
  { indexableAt :: Int -> a,
    indexableCount :: !Int
  }

instance Functor Indexable where
  fmap f Indexable{..} = Indexable (f . indexableAt) indexableCount

instance Foldable Indexable where
  length  = indexableCount
  foldMap f Indexable{..} = foldMap (f . indexableAt) [0..indexableCount-1]

instance IsList (Indexable a) where
  type Item (Indexable a) = a
  toList = F.toList
  fromList = fromVector @Vector . V.fromList

fromVector :: V.Vector v a => v a -> Indexable a
fromVector v = Indexable (v V.!) (V.length v)
