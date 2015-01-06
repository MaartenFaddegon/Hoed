This is an example of how information is lost as a result of trunction of the
cost centre stack. The actual call graph is of this program is:

        main -> f 1 -> g 2 -> f 3 -> h 1

But with pushing "f" a second time the "g" label is also lost. Additionally
the h-statement is associated with the stack [f], which can either be from the
untruncated f statement or the truncated f statement. We therefore infer the
following call graph:

        main -> f 1 -> {f 3, g 2} -> h 1
                   \_________________^

> {-# LANGUAGE TemplateHaskell, Rank2Types #-}
> import Debug.Hoed

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

> main = logO "hoed-tests-IndirectRecursion.graph" $ print (f 1)
