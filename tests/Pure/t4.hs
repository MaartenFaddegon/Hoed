-- A defective implementation of a parity function with a test property.

import Debug.Hoed
import System.Process(system)
import System.Exit(exitWith)

isOdd :: Int -> Bool
isOdd = observe "isOdd" isOdd'
isOdd' n = isEven (plusOne n)

isEven :: Int -> Bool
isEven = observe "isEven" isEven'
isEven' n = mod2 n == 0

plusOne :: Int -> Int
plusOne = observe "plusOne" plusOne'
plusOne' n = n + 1

mod2 :: Int -> Int
mod2 = observe "mod2" mod2'
mod2' n = div n 2

prop_isOdd :: Int -> Bool
prop_isOdd x = isOdd (2*x+1)

main :: IO ()
main = do
  logO "hoed-tests-Pure-t4.graph" $ print (prop_isOdd 1)
  i <- system "diff hoed-tests-Pure-t3.graph tests/ref/hoed-tests-Pure-t3.graph"
  exitWith i
