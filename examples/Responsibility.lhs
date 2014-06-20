> import Debug.Hoed.Observe

sacc "outer" 1 + (sacc "inner" 2 * x)

> ex1 :: Int -> Int
> ex1 = (gdmobserve "outer") (\x -> {-# SCC "outer" #-} 
>          1 + (((gdmobserve "inner") (\x -> {-# SCC "inner" #-} 2 * x)) x)
>       )

(sacc "com" \f g x -> f (g x)) (sacc "add" (+1)) (sacc "mul" (*2))

> ex2 :: Int -> Int
> ex2 = ((gdmobserve "com1") (\f g x -> {-# SCC "com1" #-} f (g x)))
>       ((gdmobserve "add1") ({-# SCC "add1" #-} (+1)))
>       ((gdmobserve "mul1") ({-# SCC "add1" #-} (*2)))

> ex3 :: Int -> Int
> ex3 = let f = ((gdmobserve "add2") ({-# SCC "add2" #-} (+1)))
>           g = ((gdmobserve "mul2") ({-# SCC "add2" #-} (*2)))
>       in  ((gdmobserve "com2") (\x -> {-# SCC "com2" #-} f (g x)))


> ex4 :: Int -> Int
> ex4 = let f = ((gdmobserve "f") ({-# SCC "f" #-} (+1)))
>           g = ((gdmobserve "g") ({-# SCC "g" #-} (*2)))
>       in  ((gdmobserve "h") ({-# SCC "h" #-} f . g))


> main = runO [] $ do print (ex1 4)
>                     print (ex2 4)
>                     print (ex3 4)
>                     print (ex4 4)
