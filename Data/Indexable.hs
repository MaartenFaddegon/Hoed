{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
module Data.Indexable where

import Data.Foldable as F
import Data.Vector (Vector)
import qualified Data.Vector.Generic as V
import GHC.Exts

data Indexable ix a = Indexable
  { indexableAt :: ix -> a,
    indexableLowerBound :: !ix,
    indexableUpperBound :: !ix
  }

instance Functor (Indexable ix) where
  fmap f it@Indexable{..} = it{indexableAt = f . indexableAt}

instance (Integral ix, Enum ix) => Foldable (Indexable ix) where
  length Indexable{..} = fromIntegral $ indexableUpperBound - indexableLowerBound
  foldMap f Indexable {..} =
    foldMap (f . indexableAt) [indexableLowerBound .. indexableUpperBound]

instance (Enum ix, Integral ix) => IsList (Indexable ix a) where
  type Item (Indexable ix a) = a
  toList = F.toList
  fromList = fromVector @Vector . V.fromList

fromVector :: forall v ix a . Enum ix => V.Vector v a => v a -> Indexable ix a
fromVector v = Indexable ((v V.!) . fromEnum) (toEnum 0) (toEnum $ V.length v)

mapWithIndex :: (ix -> a -> b) -> Indexable ix a -> Indexable ix b
mapWithIndex f ix = ix{indexableAt = \i -> f i (indexableAt ix i)}
