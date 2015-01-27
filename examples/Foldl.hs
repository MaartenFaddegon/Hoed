-- Foldl is an example of a higher order function with state (sometimes called
-- the accumulator). 
--
-- In this example we demonstrate how a combination of wrapping 
-- the higher order function "foldl" and transforming the callee "g"
-- gives us a notion of order between the callee-observations.

import Prelude hiding (foldl)
import qualified Prelude
import Debug.Hoed(printO,gdmobserve,gdmobserve',Identifier(..))

-- Prelude.foldl :: (b -> a -> b) -> b [a] -> b

foldl :: ((b,Identifier) -> a -> (b,Int)) -> b -> [a] -> b
foldl fn z xs = fst $ Prelude.foldl fn' (z,UnknownId) xs
  where fn' a x = let (r,i) = fn a x in (r,InSequenceAfter i)

-- g :: Int -> Int -> Int
-- g x y = x + y

g :: (Int,Identifier) -> Int -> (Int,Int)
g (a1,id) a2 = let (fn,i) = gdmobserve' "g" id (\a1' a2' -> {-# SCC "g" #-} g_orig a1' a2')
               in (fn a1 a2,i)

  where g_orig x y = x + y

f :: [Int] -> Int
f xs = foldl g 10 xs

main :: IO ()
main = printO (f [1,2,3])
