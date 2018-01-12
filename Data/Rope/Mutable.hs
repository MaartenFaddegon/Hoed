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
  , insert
  , reset
  ) where

import Control.Monad
import Control.Monad.Primitive
import Data.Foldable as F
import Data.Frame
import Data.Primitive.MutVar
import Data.Proxy
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import Data.Vector.Generic.Mutable as M (MVector, read, write, new)

data RopeState m v a = RopeState
  { ropeCount :: !Int
  , ropeElements :: [v (PrimState m) a]
  , spillOver :: v (PrimState m) a
  }

-- | A mutable bag-like collection with atomic O(1) inserts
data Rope m v a = Rope
  { ropeState      :: MutVar (PrimState m) (RopeState m v a)
  , ropeDim        :: !Int -- ^ Size of internal vectors
  }

defaultRopeDim = 10000

insert :: forall v a m . (PrimMonad m, MVector v a) => a -> Rope m v a -> m ()
insert a Rope {..} = join $ atomicModifyMutVar' ropeState updateState
  where
    updateState :: RopeState m v a -> (RopeState m v a, m ())
    updateState st@RopeState {..}
      | ropeCount < ropeDim
      , v:_ <- ropeElements =
        ( st {ropeCount = ropeCount + 1}
        , M.write v ropeCount a)
      | otherwise =
        ( st
          { ropeElements = spillOver : ropeElements
          , ropeCount = 1
          }
        , do M.write spillOver 0 a
             v <- M.new ropeDim
             atomicModifyMutVar' ropeState $ \st' -> (st' {spillOver = v}, ()))

new :: forall v a m . (PrimMonad m, MVector v a) => m (Rope m v a)
new = new' defaultRopeDim

new' :: forall v a m . (PrimMonad m, MVector v a) => Int -> m (Rope m v a)
new' ropeDim = do
  spillOver  <- M.new ropeDim
  ropeState <- newMutVar (RopeState 0 [] spillOver)
  return Rope{..}

-- | Returns an immutable snapshot of the rope contents after resetting the rope to the empty state
reset :: forall v a m . (VG.Vector v a, PrimMonad m) => Proxy v -> Rope m (VG.Mutable v) a -> m (Frame a)
reset proxy it@Rope{..} = do
  (ropeCount, ropeElements) <-
    atomicModifyMutVar' ropeState $ \RopeState {..} ->
      (RopeState 0 [] spillOver, (ropeCount, ropeElements))
  vv' :: V.Vector(v a) <- V.fromList <$> mapM VG.unsafeFreeze ropeElements
  let countFrame   = ropeCount + ropeDim * l
      l = length ropeElements - 1
      indexFrame ((`divMod` ropeDim) -> (d,m)) = vv' V.! (l - d) VG.! m
  return Frame{..}

fromList :: forall v m a. (PrimMonad m, MVector v a) => Int -> [a] -> m(Rope m v a)
fromList dim xx = do
  rope <- new' dim
  forM_ xx (`insert` rope)
  return rope

propFromList dim xx =
  (xx ==) . toList <$> (reset (Proxy @V.Vector) =<< fromList dim xx)
