{-#  LANGUAGE DeriveGeneric, DeriveAnyClass #-}
import Prelude hiding (Rational, fromRational)
import Debug.Hoed.Pure

data Rational = Integer :% Integer
 deriving (Eq, Generic, Observable, Show)

data Tree = Node Rational Tree Tree | Leaf
 deriving (Generic, Observable)

mkTree :: Integer -> Integer -> Integer -> Integer -> Tree
mkTree a b c d = Node (x :% y) (mkTree a b x y) (mkTree x y c d)
 where x = a+c
       y = b+d

toFloat :: Tree -> Float -> Rational
toFloat (Node x left right) y
 | delta <= 0 = x
 | delta > 0  = toFloat left  y
 | otherwise  = toFloat right y
 where delta = (fromRational x) - y

main = printO $ toFloat' (mkTree 0 1 1 0) 0.75
  where toFloat' = observe "toFloat" toFloat

fromRational :: Rational -> Float
fromRational (x :% y) = (fromInteger x) / (fromInteger y)
