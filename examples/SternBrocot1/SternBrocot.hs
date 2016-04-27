{-#  LANGUAGE DeriveGeneric, DeriveAnyClass #-}
import Prelude hiding (Rational, fromRational)
import Debug.Hoed.Pure

data Rational = Integer :% Integer deriving (Eq, Generic, Observable, Show)
data Tree = Tree Rational Tree Tree deriving (Generic, Observable)

mkTree :: Integer -> Integer -> Integer -> Integer -> Tree
mkTree a b c d = Tree (x :% y) (mkTree a b x y) (mkTree x y c d)
  where x = a+c
        y = b+d

aproxRational :: Tree -> Float -> Float -> Rational
aproxRational (Tree x left right) maxDelta y
  | delta > 0             = aproxRational left  maxDelta y
  | delta < 0             = aproxRational right maxDelta y
  | abs delta <= maxDelta = x -- DEFECT: should be first guard!
  where delta = (fromRational x) - y

main = printO $ (observe "aproxRational" aproxRational) (mkTree 0 1 1 0) 0.05 0.3

fromRational :: Rational -> Float
fromRational (x :% y) = (fromInteger x) / (fromInteger y)
