import Debug.Hoed.Pure

k :: Int -> Int
k  = observe "k" k'
k' x = (l x) + (m $ x + 1)

l :: Int -> Int
l  = observe "l" l'
l' x  = m x

m :: Int -> Int
m  = observe "m" m'
m' x = n x

n :: Int -> Int
n  = observe "n" n'
n' x = x

main = logO "hoed-tests-Pure-t2.graph" $ print (k 1)
