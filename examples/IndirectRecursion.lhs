> {-# LANGUAGE TemplateHaskell, Rank2Types #-}
> import Debug.Hoed.Observe

> $(observedTypes "f" [])
> $(observedTypes "g" [])
> $(observedTypes "h" [])

> f :: Int -> Int
> f   x = $(observe "f") f' x
> f'  x = {-# SCC "f" #-} f'' x
> f'' 1 = g 2
> f'' x = h (x + 1)

> g :: Int -> Int
> g   x = $(observe "g") g' x
> g'  x = {-# SCC "g" #-} g'' x
> g'' x = f (x + 1)

> h :: Int -> Int
> h   x = $(observe "h") h' x
> h'  x = {-# SCC "h" #-} h'' x
> h'' x = (x+1)

> main = runO $ print (f 1)
