module Queens where
-- The queens problem made famous by Wirth.
import Debug.Hoed.Pure
import Data.List
import Test.QuickCheck
import Data.Maybe
import Test.QuickCheck
import Test.QuickCheck.Property
import Test.QuickCheck.Gen
import Test.QuickCheck.Random
import Properties
import Types

doit :: IO ()
doit = runOwp properties $
  if null solutions then putStrLn "no solution!"
  else print (head solutions)
  where
  solutions = queens 4

queens :: Int -> [Board]
queens = observe "queens" (\n -> valid n n)

-- How can we place m queens on an n*n board?
valid :: Int -> Int -> [Board]
valid = observe "valid" valid'
valid' 0 _ = [[]]
valid' m n
 -- A correct definition:
 --  = filter safe (extend n (valid (m-1) n))
 -- A defective definition:
         = filter safe (extend n (valid (m-1) (n-1)))

extend :: Int -> [Board] -> [Board]
extend = observe "extend" (\n bs -> consEach [1..n] bs)

consEach :: Observable a => [a] -> [[a]] -> [[a]]
consEach = observe "consEach" (\xs y -> case xs of
 []    -> []
 (a:x) -> map (a:) y ++ consEach x y)

safe :: Board -> Bool
safe = observe "safe" (\(a:b) -> no_threat a b 1)

no_threat :: Int -> Board -> Int -> Bool
no_threat = observe "no_threat" no_threat'
no_threat' a [] m = True
no_threat' a (b:y) m
  = a /= b && a+m /= b && a-m /= b && no_threat a y (m+1)

showBoard :: Board -> String 
showBoard = observe "showBoard" (\b ->
  let rank r qcol =
        map line ["o o o", " \\|/ ", " === "]
        where
        line crown_slice =
          concat (zipWith square [1..] b)
          where
          square scol _ =
            if scol == qcol then crown_slice
            else if scol `rem` (2::Int) == r `rem` (2::Int) then "....."
            else "     "
  in unlines (concat (zipWith rank [1..] b)))
