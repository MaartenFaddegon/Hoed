{-# LANGUAGE ConstraintKinds #-}
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
  ) where

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

data RopeState m v a = RopeState
  { ropeDepth :: !Int
  , ropeLastIndex :: !Int
  , ropeElements :: [v (PrimState m) a]
  , spillOver :: v (PrimState m) a
  }

-- | A mutable bag-like collection with atomic O(1) inserts
data Rope m v ix a = Rope
  { ropeState      :: MutVar (PrimState m) (RopeState m v a)
  , ropeDim        :: !Int -- ^ Size of internal vectors
  }

defaultRopeDim = 10000

type Index = Enum

{-# INLINE write #-}
write :: forall v a ix m . (Index ix, PrimMonad m, MVector v a) => Rope m v ix a -> ix -> a -> m ()
write Rope {..} (fromEnum -> ix) a = join $ atomicModifyMutVar' ropeState updateState
  where
    (d, r) = divMod ix ropeDim
    updateState :: RopeState m v a -> (RopeState m v a, m ())
    updateState st@RopeState {..}
      | d < ropeDepth =
        ( st {ropeLastIndex = max ropeLastIndex ix}
        , M.unsafeWrite (ropeElements !! (ropeDepth - 1 - d)) r a)
      | d == ropeDepth =
        ( st
          { ropeElements = spillOver : ropeElements
          , ropeDepth = d + 1
          , ropeLastIndex = max ropeLastIndex ix
          }
        , do M.unsafeWrite spillOver r a
             v <- M.new ropeDim
             atomicModifyMutVar' ropeState $ \st' -> (st' {spillOver = v}, ()))
      | otherwise = error $ "index " ++ show ix ++ " too far away from the last index " ++ show ropeLastIndex

new :: forall v a m . (PrimMonad m, MVector v a) => m (Rope m v Int a)
new = new' defaultRopeDim

new' :: forall v a ix m . (PrimMonad m, MVector v a, Index ix) => Int -> m (Rope m v ix a)
new' ropeDim = do
  spillOver  <- M.new ropeDim
  ropeState <- newMutVar (RopeState 0 (-1) [] spillOver)
  return Rope{..}

-- | Returns an immutable snapshot of the rope contents after resetting the rope to the empty state
reset :: forall v a ix m . (VG.Vector v a, PrimMonad m, Enum ix) => Proxy v -> Rope m (VG.Mutable v) ix a -> m (Indexable ix a)
reset proxy it@Rope{..} = do
  (lastIndex, ropeElements) <-
    atomicModifyMutVar' ropeState $ \RopeState {..} ->
      (RopeState 0 (-1) [] spillOver, (ropeLastIndex, ropeElements))
  lv <- mapM VG.unsafeFreeze ropeElements
  let indexableUpperBound = toEnum lastIndex
      l = length ropeElements - 1
      joined :: v a
        | h:t <- lv
        = VG.concat (reverse t ++ [VG.slice 0 (lastIndex `mod` ropeDim + 1) h])
        | otherwise = VG.empty
      indexableAt = (joined VG.!) . fromEnum
      {-# INLINE indexableFoldr #-}
      indexableFoldr :: forall b. (ix -> a -> b -> b) -> b -> b
      indexableFoldr f x0 = VG.ifoldr (f . toEnum) x0 joined
  return Indexable{..}

fromList :: forall v m a. (PrimMonad m, MVector v a) => Int -> [a] -> m(Rope m v Int a)
fromList dim xx = do
  rope <- new' dim
  forM_ (zip [0..] xx) $ \(i,x) -> write rope i x
  return rope

propFromList dim xx =
  check . toList <$> (reset (Proxy @V.Vector) =<< fromList dim xx)
  where
    check xx'
      | xx == xx' = True
      | otherwise = error (show xx')
