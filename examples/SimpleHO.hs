import Debug.Hoed.Pure

ap4 :: (Int -> Int) -> Int
ap4 = observe "ap4" (\f -> f 4)

mod2 :: Int -> Int
mod2 = observe "mod2" (\n -> div n 2)

main = printO (ap4 mod2)
