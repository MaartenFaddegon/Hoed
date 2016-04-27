{-#  LANGUAGE DeriveGeneric, DeriveAnyClass #-}
import Prelude hiding (Rational, fromRational)
import Debug.Hoed.Pure

data Rational = Integer :% Integer
 deriving (Eq, Generic, Observable, Show)

data Tree a = Node a (Tree a) (Tree a) | Leaf a 
 deriving (Generic, Observable)

mkNode :: Integer -> Integer -> Integer -> Integer -> Tree Rational
mkNode a b c d = Node (x :% y) (mkNode a b x y) (mkNode x y c d)
 where x = a+c
       y = b+d

toFloat :: Tree Rational -> Float -> Rational
toFloat (Node x left right) y
 | delta <= 0 = x
 | delta > 0  = toFloat left  y
 | otherwise  = toFloat right y
 where delta = (fromRational x) - y

main = printO $ toFloat' (mkNode 0 1 1 0) 0.75
  where toFloat' = observe "toFloat" toFloat

fromRational :: Rational -> Float
fromRational (x :% y) = (fromInteger x) / (fromInteger y)
