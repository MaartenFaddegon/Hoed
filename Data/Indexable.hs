{-# LANGUAGE RankNTypes #-}
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
    indexableFoldr :: forall b. (ix -> a -> b -> b) -> b -> b,
    indexableUpperBound :: !ix
  }

instance Functor (Indexable ix) where
  fmap f it@Indexable {..} =
    it
    { indexableAt = f . indexableAt
    , indexableFoldr = \f' -> indexableFoldr (\i a -> f' i (f a))
    }

instance (Integral ix, Enum ix) => Foldable (Indexable ix) where
  length Indexable{..} = fromIntegral $ indexableUpperBound - toEnum 0
  -- foldMap f Indexable {..} = foldMap (f . indexableAt) [toEnum 0 .. indexableUpperBound]
  {-# INLINE foldr #-}
  foldr f x0 Indexable{..} = indexableFoldr (const f) x0

instance (Enum ix, Integral ix) => IsList (Indexable ix a) where
  type Item (Indexable ix a) = a
  toList = F.toList
  fromList = fromVector @Vector . V.fromList

fromVector :: forall v ix a . (Enum ix, Foldable v, V.Vector v a) => v a -> Indexable ix a
fromVector v =
  Indexable
    ((v V.!) . fromEnum)
    (\f x0 -> V.ifoldr (f . toEnum) x0 v)
    (toEnum $ V.length v)

{-# INLINE mapWithIndex #-}
mapWithIndex :: (ix -> a -> b) -> Indexable ix a -> Indexable ix b
mapWithIndex f ix =
  ix
  { indexableAt = \i -> f i (indexableAt ix i)
  , indexableFoldr = \f' -> indexableFoldr ix (\i a -> f' i (f i a))
  }
