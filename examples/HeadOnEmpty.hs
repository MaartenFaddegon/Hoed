import Debug.Hoed

main = printO x

x :: Int
x = observe "f" ({-# SCC "f" #-} h head xs)

xs :: [Int]
xs = observe "xs" ({-# SCC "xs" #-} [])

h :: ([Int] -> Int) -> [Int] -> Int
h = observe "h" (\a' is' -> {-# SCC "h" #-} a' is')
