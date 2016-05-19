{-#  LANGUAGE DeriveGeneric, DeriveAnyClass #-}
import Debug.Hoed.Pure

----------------------------------------------------------------------
-- The tree library

data Tree a = Node a (Tree a) (Tree a) | Leaf a
 deriving (Generic, Observable)

breadthFirst :: Observable a => Tree a -> [a]
breadthFirst = observe "breadthFirst" $ \tree -> fold [tree]

fold = observe "fold" fold'
fold' [] = []
fold' queue = map nodeVal queue ++ concatMap (fold . subTrees) queue
-- ^ defective; goes in depth first!
-- fold' queue = map nodeVal queue ++ fold (concatMap subTrees queue)

nodeVal (Node x t1 t2) = x
nodeVal (Leaf x) = x

subTrees (Node x t1 t2) = [t1,t2]
subTrees (Leaf x) = []

depth :: Observable => Tree a -> Int -> [a]
depth d = take ((d+1)*2) (drop (2^d-1) (breadthFirst (mkTree [])))

----------------------------------------------------------------------
-- the Coin flip application

data Coin = Head | Tail
 deriving (Eq, Generic, Observable)

mkTree c = Node c (mkTree (Head : c)) (mkTree (Tail : c))

prop_depthSound d = length h == length t
 where 
 c = concat (depth d)
 (h,t) = partition (==Head) c

main = runO (print (prop_depthSound 3))
