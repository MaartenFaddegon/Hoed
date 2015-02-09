-- Haskell version of the buggy insertion sort as shown in Lee Naish
-- A Declarative Debugging Scheme.
--
-- As Insort1, but with observe rather than templated observers.

{-# LANGUAGE StandaloneDeriving #-}
import Debug.Hoed

-- Insertion sort.

isort :: [Int] -> [Int]
isort ns = observe "isort" (\ns -> {-# SCC "isort" #-} isort' ns) ns
isort' []     = []
isort' (n:ns) = insert n (isort ns)

-- Insert number into sorted list.

insert :: Int -> [Int] -> [Int]
insert n ms = (observe "insert" (\n ms -> {-# SCC "insert" #-} insert' n ms)) n ms
insert' :: Int -> [Int] -> [Int]
insert' n []      = [n]
insert' n (m:ms)
      | n <= m    = n : ms -- bug: `m' is missing in this case
      | otherwise = m : (insert n ms)

main = logO "hoed-tests-Insort2.graph" . print $
         (observe "result") ({-# SCC "result" #-} isort [1,2])

-- Slices, these should be generated automatically from the original code.

slices
  = [ ("result",  "isort [1,2]")
    , ("isort" ,  "isort []     = []\n"
               ++ "isort (n:ns) = insert n (isort ns)")
    , ("insert",  " insert n []       = [n]\n"
               ++ " insert n (m:ms)\n"
               ++ "       | n <= m    = n : ms\n"
               ++ "       | otherwise = m : (insert n ms)\n")
    ]
