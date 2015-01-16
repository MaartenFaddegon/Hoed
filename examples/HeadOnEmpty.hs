import Debug.Hoed

main = printO x

x :: Int
x = gdmobserve "f" ({-# SCC "f" #-} h head xs)

xs :: [Int]
xs = gdmobserve "xs" ({-# SCC "xs" #-} [])

h :: ([Int] -> Int) -> [Int] -> Int
h = gdmobserve "h" (\a' is' -> {-# SCC "h" #-} a' is')
