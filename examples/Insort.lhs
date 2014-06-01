Haskell version of the buggy insertion sort as shown in Lee Naish
A Declarative Debugging Scheme.

> {-# LANGUAGE TemplateHaskell, Rank2Types #-}
> import Debug.Hoed.Observe
> $(observedTypes "isort"  [[t|forall a . Observable a => [] a|]])
> $(observedTypes "insert" [[t|forall a . Observable a => [] a|]])
> $(observedTypes "result" [[t|forall a . Observable a => [] a|]])

Insertion sort.

> isort :: [Int] -> [Int]
> isort ns = $(observe "isort") (\ns -> {-# SCC "isort" #-} isort' ns) ns
> isort' []     = []
> isort' (n:ns) = insert n (isort ns)

Insert number into sorted list.

> insert :: Int -> [Int] -> [Int]
> insert n ss = ($(observe "insert") (\n ss -> {-# SCC "insert" #-} insert' n ss)) n ss
> insert' :: Int -> [Int] -> [Int]
> insert' n []     = [n]
> insert' n (s:ss)
>       | n <= s    = n : ss
>       | otherwise = s : (insert n ss)

> main = runO slices . print $
>          $(observe "result") ({-# SCC "result" #-} isort [1,2])

Slices, these should be generated automatically from the original code.

> slices
>   = [ ("result",  "isort [1,2]")
>     , ("isort" ,  "isort []     = []\n"
>                ++ "isort (n:ns) = insert n (isort ns)")
>     , ("insert",  " insert n []       = [n]\n"
>                ++ " insert n (s:ss)\n"
>                ++ "       | n <= s    = n : ss\n"
>                ++ "       | otherwise = s : (insert n ss)\n")
>     ]
