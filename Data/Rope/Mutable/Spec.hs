{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# Options -fno-warn-partial-type-signatures #-}

module Data.Rope.Mutable.Spec where

import Control.DeepSeq
import Control.Exception as E
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as Map
import Data.List
import Data.Proxy
import Data.Rope.Mutable
import Data.Semigroup(Semigroup(..))
import Data.Vector (Vector)
import qualified Data.Vector.Generic as V
import Numeric.Natural
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

main = hspec spec

-- model :: ([a] -> [b]) -> Rope m v a -> Rope m v b
-- model f = toList . f . fromList

spec :: Spec
spec = do
  describe "Boxed" $ do
    unitSpec (Proxy :: Proxy Vector)
    propSpec (Proxy :: Proxy Vector)

testSize = 2

data MutableRopeModel a
    = New
    | Write (MutableRopeModel a) Natural a
    | Reset (MutableRopeModel a)
    deriving (Eq, Show)

instance Arbitrary a => Arbitrary (MutableRopeModel a) where
  arbitrary = sized genMutableRopeModel

genMutableRopeModel 0 = return New
genMutableRopeModel n = oneof
  [ return New
  , Reset <$> genMutableRopeModel (n `div` 2)
  , Write <$> genMutableRopeModel (n `div` 2) <*> arbitrarySizedNatural <*> arbitrary]

interpretation :: MutableRopeModel a -> [Maybe a]
interpretation = fixGaps . go
  where
    fixGaps m
      | Map.null m = []
      | n <- maximum (Map.keys m) = [ Map.lookup u m | u <- [0 .. n]]
    go New = mempty
    go (Write m i x) = Map.insert (fromIntegral i) x (go m)
    go (Reset _) = mempty

implementation
  :: forall m v a.
     (_)
  => Proxy v -> MutableRopeModel a -> IO (Rope IO (V.Mutable v) (Maybe a))
implementation proxy model = do
    (res, indexes) <- flip runStateT [] $ go model
    fixGaps res indexes
  where
    fixGaps r [] = return r
    fixGaps r ii | m <- maximum ii = do
      let gaps = ([0 .. m] \\ ii)
      forM_ gaps $ \j -> write r (fromIntegral j) Nothing
      return r
    go New = lift $ new' testSize
    go (Write model x j) = do
      r <- go model
      modify (<> [x])
      lift $ write r (fromIntegral x) (Just j)
      return r
    go (Reset model) = do
      r <- go model
      ii <- get
      _ <- lift $ fixGaps r ii
      _ <- lift $ reset proxy r
      put []
      return r

propSpec :: _ => Proxy v -> Spec
propSpec proxy =
  describe "Properties" $ do
    prop "V.toList . Rope.reset . Rope.fromList = id" $ \(Positive dim) (xx :: [Bool]) -> do
      r <- reset proxy =<< fromList dim xx
      V.toList r `shouldBe` xx
    prop "lists are a model" $ \(script :: MutableRopeModel Bool) -> do
      res <- implementation proxy script >>= reset proxy
      V.toList res `shouldBe` interpretation script

unitSpec :: _ => Proxy (v :: * -> *) -> Spec
unitSpec proxy = do
    describe "new" $ do
      it "should work" $
        shouldReturn ([] :: [()]) $ \_ -> return ()
    describe "unit" $ do
      describe "from new" $ do
        it "on 0" $ shouldReturn [()] $ \r ->
          write r 0 ()
        it "on 1" $ shouldBarf $ \r ->
          write r 1 ()
        prop "random" $ \(NonEmpty xx) -> let Positive m = maximum xx in
          shouldReturn [0 .. m] $ \r -> do
            let uninitialized = [Positive 0 .. Positive m] \\ xx
            forM_ (xx ++ uninitialized) $ \(Positive i) ->
              write r i i
  where
    shouldReturn list f = do
      rope <- new' testSize
      f rope
      r <- reset proxy rope
      V.toList r `shouldBe` list
    shouldBarf f = do
      rope <- new' testSize
      f rope
      r <- reset proxy rope
      void $ evaluate (force $ V.toList r)
     `shouldThrow` \ErrorCall{} -> True
