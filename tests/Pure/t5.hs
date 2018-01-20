import Debug.Hoed
import System.Process(system)
import System.Exit(exitWith)

f :: Maybe Int -> Int
f = observe "f" f'
f' (Just i) = g i
f' Nothing  = 0

g :: Int -> Int
g = observe "g" g'
g' x = x + x

main :: IO ()
main = do
  logO "hoed-tests-Pure-t5.graph" $ print (f $ Just 3)
  i <- system "diff hoed-tests-Pure-t5.graph tests/ref/hoed-tests-Pure-t5.graph"
  exitWith i
