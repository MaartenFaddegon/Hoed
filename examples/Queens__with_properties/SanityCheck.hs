-- The queens problem made famous by Wirth.
import Debug.Hoed.Pure hiding (maxSize)
import Data.List
import Test.QuickCheck
import Data.Maybe
import Test.QuickCheck
import Test.QuickCheck.Property
import Test.QuickCheck.Gen
import Test.QuickCheck.Random
import Control.Monad
import Properties
import Types

--------------------------------------------------------------------------------
-- Sanity checking of properties agains uncorrupted Queens implementation, all
-- should pass.

main = do
  putStrLn "Checking prop_queens_sound ..."
  mapM_ (showCase $ assertTrue . prop_queens_sound queens) [1..8]
  putStrLn "Checking prop_queens_complete ..."
  mapM_ (showCase $ \n -> quickerCheck $ prop_queens_complete queens n) [1,4]
  putStrLn "Checking prop_valid_sound ..."
  mapM_ (showCase $ \(x,y) -> assertTrue $ prop_valid_sound valid x y) $ for [1..8] [1..8]
  putStrLn "Checking prop_valid_complete ..."
  mapM (showCase $ \(m,n) -> quickerCheck $ prop_valid_complete valid m n) $ for [1,4] [1,4]
  putStrLn "Checking prop_extend_sound ..."
  mapM_ (showCase $ \n -> quickerCheck (prop_extend_sound extend n . nub)) [1..4]
  putStrLn "Checking prop_extend_complete ..."
  mapM_ (showCase $ \n -> quickerCheck (prop_extend_complete extend n . nub)) [1..4]
  putStrLn "Checking spec_safe ..."
  quickCheck (spec_safe safe)
  putStrLn "Checking spec_no_threat ..."
  mapM_ (showCase $ \a -> quickCheck (\b -> spec_no_threat no_threat a b 1)) [1,4]
  return ()

showCase :: Show a => (a -> IO ()) -> a -> IO ()
showCase f x = do
  putStr (show x ++ " ")
  f x
  
assertTrue :: Bool -> IO ()
assertTrue False = putStrLn "Failed!"
assertTrue True  = putStrLn "OK"

for :: [a] -> [b] -> [(a,b)]
for xs = foldr (\x z -> zip xs (cycle [x]) ++ z) []

quickerCheck :: Testable prop => prop -> IO ()
quickerCheck = quickCheckWith stdArgs{maxSuccess=10,maxDiscardRatio=1000,maxSize=4}

-------------------------------------------------------------------------------- 
-- Correct reference implementation of the Queens solver.

queens :: Int -> [Board]
queens n = valid n n

-- How can we place m queens on an n*n board?
valid :: Int -> Int -> [Board]
valid 0 _ = [B []]
valid m n = filter safe (extend n (valid (m-1) n))

extend :: Int -> [Board] -> [Board]
extend n bs = (consEach [1..n] bs)

consEach :: [Int] -> [Board] -> [Board]
consEach [] bs    = []
consEach (a:x) bs = map (\(B b) -> B (a:b)) bs ++ consEach x bs

safe :: Board -> Bool
safe (B (a:b)) = no_threat a (B b) 1

no_threat :: Int -> Board -> Int -> Bool
no_threat a (B []) m = True
no_threat a (B (b:y)) m
  = a /= b && a+m /= b && a-m /= b && no_threat a (B y) (m+1)
