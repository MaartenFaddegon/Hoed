> {-# LANGUAGE TemplateHaskell, Rank2Types #-}
> import Debug.Hoed

> $(observedTypes "k" [])
> $(observedTypes "l" [])
> $(observedTypes "m" [])
> $(observedTypes "n" [])


> main = runO [] $ print (k 1)

> k :: Int -> Int
> k  x = $(observe "k") k' x
> k' x = {-# SCC "k" #-} k'' x
> k'' x = (l x) + (m $ x + 1)

> l :: Int -> Int
> l  x  = $(observe "l") l' x
> l' x  = {-# SCC "l" #-} m x

> m :: Int -> Int
> m  x = $(observe "m") m' x
> m' x = {-# SCC "m" #-} n x

> n :: Int -> Int
> n  x = $(observe "n") n' x
> n' x = {-# SCC "n" #-} x
