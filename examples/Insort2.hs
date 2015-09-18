-- Haskell version of the buggy insertion sort as shown in Lee Naish
-- A Declarative Debugging Scheme.
--
-- As Insort1, but with observe rather than templated observers.

import Debug.Hoed.Pure

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

main = printO $ isort [1,2]
