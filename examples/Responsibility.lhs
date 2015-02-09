> import Debug.Hoed

sacc "outer" 1 + (sacc "inner" 2 * x)

> ex1 :: Int -> Int
> ex1 = (observe "outer") (\x -> {-# SCC "outer" #-} 
>          1 + (((observe "inner") (\x -> {-# SCC "inner" #-} 2 * x)) x)
>       )

(sacc "com" \f g x -> f (g x)) (sacc "add" (+1)) (sacc "mul" (*2))

> ex2 :: Int -> Int
> ex2 = ((observe "com1") (\f g x -> {-# SCC "com1" #-} f (g x)))
>       ((observe "add1") ({-# SCC "add1" #-} (+1)))
>       ((observe "mul1") ({-# SCC "add1" #-} (*2)))

> ex3 :: Int -> Int
> ex3 = let f = ((observe "add2") ({-# SCC "add2" #-} (+1)))
>           g = ((observe "mul2") ({-# SCC "add2" #-} (*2)))
>       in  ((observe "com2") (\x -> {-# SCC "com2" #-} f (g x)))


> ex4 :: Int -> Int
> ex4 = let f = ((observe "f") ({-# SCC "f" #-} (+1)))
>           g = ((observe "g") ({-# SCC "g" #-} (*2)))
>       in  ((observe "h") ({-# SCC "h" #-} f . g))


> main = runO $ do print (ex1 4)
>                  print (ex2 4)
>                  print (ex3 4)
>                  print (ex4 4)
