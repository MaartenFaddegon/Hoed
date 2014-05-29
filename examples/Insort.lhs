Haskell version of the buggy insertion sort as shown in Lee Naish
A Declarative Debugging Scheme.

> {-# LANGUAGE TemplateHaskell, Rank2Types #-}
> import Debug.Hoed.Observe
> $(observedTypes "insert" [])
> $(observedTypes "isort" [])

Insertion sort.

> isort :: [Int] -> [Int]
> isort  ns = $(observe "isort")  isort'  ns
> isort' ns = {-# SCC "isort" #-} isort'' ns
> isort'' []     = []
> isort'' (n:ns) = insert n (isort ns)

Insert number into sorted list.

> insert :: Int -> [Int] -> [Int]
> insert  n ss = $(observe "insert")  insert'  n ss
> insert' n ss = {-# SCC "insert" #-} insert'' n ss
> insert'' n []     = [n]
> insert'' n (s:ss)
>       | n <= s    = n : ss
>       | otherwise = s : (insert n ss)


> $(observedTypes "main" [])
> main = (runO . putStrLn . show) ($(observe "main") main')
> main' = {-# SCC "main" #-} isort [3,1,2]
