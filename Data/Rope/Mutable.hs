{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
module Data.Rope.Mutable
  ( Rope
  , Data.Rope.Mutable.new
  , new'
  , fromList
  , write
  , reset
  , Iso(..)
  ) where

import Control.Category
import Control.Monad
import Control.Monad.Primitive
import Data.Foldable as F
import Data.Indexable
import Data.Primitive.MutVar
import Data.Proxy
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as M
import Data.Vector.Generic.Mutable as M (MVector, read, new)
import Prelude hiding ((.), id)

data RopeState m v a = RopeState
  { ropeDepth :: !Int
  , ropeLastIndex :: !Int
  , ropeElements :: [v (PrimState m) a]
  , spillOver :: v (PrimState m) a
  }

data Iso a b = Iso
  { to   :: a -> b
  , from :: b -> a
  }

instance Category Iso where
  id = Iso id id
  i1 . i2 = Iso (to i1 . to i2) (from i2 . from i1)

-- | A mutable bag-like collection with atomic O(1) inserts
data Rope m v ix a = Rope
  { ropeState      :: MutVar (PrimState m) (RopeState m v a)
  , ropeDim        :: !Int -- ^ Size of internal vectors
  , ropeIndexFun   :: Iso ix Int
  }

defaultRopeDim = 10000

write :: forall v a ix m . (PrimMonad m, MVector v a) => Rope m v ix a -> ix -> a -> m ()
write Rope {..} (to ropeIndexFun -> ix) a = join $ atomicModifyMutVar' ropeState updateState
  where
    (d, r) = divMod ix ropeDim
    updateState :: RopeState m v a -> (RopeState m v a, m ())
    updateState st@RopeState {..}
      | d < ropeDepth =
        ( st {ropeLastIndex = max ropeLastIndex ix}
        , M.write (ropeElements !! (ropeDepth - 1 - d)) r a)
      | d == ropeDepth =
        ( st
          { ropeElements = spillOver : ropeElements
          , ropeDepth = d + 1
          , ropeLastIndex = max ropeLastIndex ix
          }
        , do M.write spillOver r a
             v <- M.new ropeDim
             atomicModifyMutVar' ropeState $ \st' -> (st' {spillOver = v}, ()))
      | otherwise = error $ "index too far away: " ++ show ix

new :: forall v a m . (PrimMonad m, MVector v a) => m (Rope m v Int a)
new = new' defaultRopeDim id

new' :: forall v a ix m . (PrimMonad m, MVector v a) => Int -> Iso ix Int -> m (Rope m v ix a)
new' ropeDim ropeIndexFun = do
  spillOver  <- M.new ropeDim
  ropeState <- newMutVar (RopeState 0 (-1) [] spillOver)
  return Rope{..}

-- | Returns an immutable snapshot of the rope contents after resetting the rope to the empty state
reset :: forall v a ix m . (VG.Vector v a, PrimMonad m) => Proxy v -> Rope m (VG.Mutable v) ix a -> m (Indexable ix a)
reset proxy it@Rope{..} = do
  (ropeCount, ropeElements) <-
    atomicModifyMutVar' ropeState $ \RopeState {..} ->
      (RopeState 0 (-1) [] spillOver, (ropeLastIndex, ropeElements))
  vv' :: V.Vector(v a) <- V.fromList <$> mapM VG.unsafeFreeze ropeElements
  let indexableLowerBound = from ropeIndexFun 0
      indexableUpperBound = from ropeIndexFun ropeCount
      l = length ropeElements - 1
      indexableAt ((`divMod` ropeDim) . to ropeIndexFun -> (d,m)) = vv' V.! (l - d) VG.! m
  return Indexable{..}

fromList :: forall v m a. (PrimMonad m, MVector v a) => Int -> [a] -> m(Rope m v Int a)
fromList dim xx = do
  rope <- new' dim id
  forM_ (zip [0..] xx) $ \(i,x) -> write rope i x
  return rope

propFromList dim xx =
  (xx ==) . toList <$> (reset (Proxy @V.Vector) =<< fromList dim xx)
