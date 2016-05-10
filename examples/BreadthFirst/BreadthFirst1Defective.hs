{-#  LANGUAGE DeriveGeneric, DeriveAnyClass #-}
import Debug.Hoed.Pure

data Tree a = Node a (Tree a) (Tree a) | Leaf a
 deriving (Generic, Observable)

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

data Coin = Head | Tail
 deriving (Eq, Generic, Observable)

mkTree c = Node c (mkTree (Head : c)) (mkTree (Tail : c))

afterFlip d = take ((d+1)*2) (drop (2^d-1) (breadthFirst (mkTree [])))

prop_coinTreeLength d = all (\c -> length c == d) (afterFlip d)

prop_afterFlipSound d = length h == length t
 where 
 c = concat (afterFlip d)
 h = filter (==Head) c
 t = filter (==Tail) c

main = runO (print (prop_afterFlipSound 3))
