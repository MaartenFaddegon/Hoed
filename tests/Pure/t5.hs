import Debug.Hoed.Pure

f :: Maybe Int -> Int
f = observe "f" f'
f' (Just i) = g i
f' Nothing  = 0

g :: Int -> Int
g = observe "g" g'
g' x = x + x

main :: IO ()
main = logO "hoed-tests-Pure-t5.graph" $ print (f $ Just 3)
