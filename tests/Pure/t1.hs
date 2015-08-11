import Debug.Hoed.Pure

f :: Int -> Int
f = observe "f" f'
f' x = if x > 0 then g x else 0

g :: Int -> Int
g = observe "g" g'
g' x = x `div` 2

main = logO "hoed-tests-Pure-t1.graph" $ print ((f 2) + (f 0))

