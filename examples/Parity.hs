-- A defective parity check.
import Debug.Hoed.Pure

isOdd n = isEven (plusOne n)

isEven = observe "isEven" isEven'
isEven' n = mod2 n == 0

plusOne = observe "plusOne" plusOne'
plusOne' n = n + 1

mod2 = observe "mod2" mod2'
mod2' n = div n 2

prop_isOdd :: Int -> Bool
prop_isOdd x = isOdd (2*x+1)

main :: IO ()
main = testO prop_isOdd  1
