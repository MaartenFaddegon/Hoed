import Debug.Hoed

f :: Int -> Int
f = observe "f" $ \x -> {-# SCC "f" #-} if x > 0 then g x else 0

g :: Int -> Int
g = observe "g" $ \x -> {-# SCC "g" #-} x `div` 2

main = runO [] $ print ((f 2) + (f 0))
