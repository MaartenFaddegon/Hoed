{-# LANGUAGE DeriveGeneric #-}

module Types where
import Test.QuickCheck hiding ((===))
import Debug.Hoed

data Board = B [Int] deriving (Eq, Show, Generic)

instance Observable Board
instance ParEq Board

instance Arbitrary Board where 
  arbitrary = do b <- genBoard; return (B b)

genBoard :: Gen [Int]
genBoard = sized $ \n ->
  do m <- choose (0,n)
     k <- choose (0,n)
     vectorOf k (genPos m)

genPos :: Int -> Gen Int  
genPos n | n < 1 = return 1
genPos n = elements [1..n]

data Configuration = Configuration Int Board
  deriving (Show,Eq, Generic)
instance Observable Configuration
instance ParEq Configuration
