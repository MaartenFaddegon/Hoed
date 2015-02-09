Haskell version of the buggy insertion sort as shown in Lee Naish
A Declarative Debugging Scheme.

> {-# LANGUAGE TemplateHaskell, Rank2Types #-}
> import Debug.Hoed
> $(observedTypes "isort"  [[t|forall a . Observable a => [] a|]])
> $(observedTypes "insert" [[t|forall a . Observable a => [] a|]])
> $(observedTypes "result" [[t|forall a . Observable a => [] a|]])

Insertion sort.

> isort :: [Int] -> [Int]
> isort ns = $(observeTempl "isort") (\ns -> {-# SCC "isort" #-} isort' ns) ns
> isort' []     = []
> isort' (n:ns) = insert n (isort ns)

Insert number into sorted list.

> insert :: Int -> [Int] -> [Int]
> insert n ms = ($(observeTempl "insert") (\n ms -> {-# SCC "insert" #-} insert' n ms)) n ms
> insert' :: Int -> [Int] -> [Int]
> insert' n []      = [n]
> insert' n (m:ms)
>       | n <= m    = n : ms -- bug: `m' is missing in this case
>       | otherwise = m : (insert n ms)

> main = printO $
>          $(observeTempl "result") ({-# SCC "result" #-} isort [1,2])
