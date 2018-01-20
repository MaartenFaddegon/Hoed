-- Haskell version of the buggy insertion sort as shown in Lee Naish
-- A Declarative Debugging Scheme.

{-# LANGUAGE StandaloneDeriving #-}
import Debug.Hoed
import System.Process(system)
import System.Exit(exitWith)

-- Insertion sort.
isort :: [Int] -> [Int]
isort = observe "isort" isort'
isort' []     = []
isort' (n:ns) = insert n (isort ns)

-- Insert number into sorted list.
insert :: Int -> [Int] -> [Int]
insert = observe "insert" insert'
insert' :: Int -> [Int] -> [Int]
insert' n []      = [n]
insert' n (m:ms)
      | n <= m    = n : ms -- bug: `m' is missing in this case
      | otherwise = m : (insert n ms)

main = do
  logO "hoed-tests-Pure-t3.graph" $ print (isort [1,2])
  i <- system "diff hoed-tests-Pure-t3.graph tests/ref/hoed-tests-Pure-t3.graph"
  exitWith i
