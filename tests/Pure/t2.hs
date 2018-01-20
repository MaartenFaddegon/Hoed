import Debug.Hoed
import System.Process(system)
import System.Exit(exitWith)

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

main = do
  logO "hoed-tests-Pure-t2.graph" $ print (k 1)
  i <- system "diff hoed-tests-Pure-t2.graph tests/ref/hoed-tests-Pure-t2.graph"
  exitWith i
