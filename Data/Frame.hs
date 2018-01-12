{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
module Data.Frame where

import Data.Foldable as F
import Data.Vector (Vector)
import qualified Data.Vector.Generic as V
import GHC.Exts

data Frame a = Frame
  { indexFrame :: Int -> a,
    countFrame :: !Int
  }

instance Functor Frame where
  fmap f Frame{..} = Frame (f . indexFrame) countFrame

instance Foldable Frame where
  length  = countFrame
  foldMap f Frame{..} = foldMap (f . indexFrame) [0..countFrame-1]

instance IsList (Frame a) where
  type Item (Frame a) = a
  toList = F.toList
  fromList = fromVector @Vector . V.fromList

fromVector :: V.Vector v a => v a -> Frame a
fromVector v = Frame (v V.!) (V.length v)
