module Queens where
-- The queens problem made famous by Wirth.
import Debug.Hoed
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
doit = testOwp properties (prop_queens_set queens) 8

queens :: Int -> [Board]
queens = observe "queens" (\n -> valid n n)

-- How can we place m queens on an n*n board?
valid :: Int -> Int -> [Board]
valid = observe "valid" valid'
valid' 0 _ = [B []]
valid' m n
 -- A correct definition:
 = filter safe (extend n (valid (m-1) n))
 -- A defective definition:
 --  = filter safe (extend n (valid (m-1) (n-1)))

extend :: Int -> [Board] -> [Board]
extend = observe "extend" (\n bs -> consEach [1..n] bs)

consEach :: [Int] -> [Board] -> [Board]
consEach = observe "consEach" consEach'
consEach' [] bs    = []
consEach' (a:x) bs = map (\(B b) -> B (a:b)) bs ++ consEach x bs

safe :: Board -> Bool
safe = observe "safe" safe'
safe' (B (a:b)) = no_threat a (B b) 1

no_threat :: Int -> Board -> Int -> Bool
no_threat = observe "no_threat" no_threat'
no_threat' a (B []) m = True
no_threat' a (B (b:y)) m
 -- A correct definition:
 -- = a /= b && a+m /= b && a-m /= b && no_threat a (B y) (m+1)
 -- A defective definition:
  = a+m /= b && a-m /= b && no_threat a (B y) (m+1)

--------------------------------------------------------------------------------
-- demonstrating the need for parallel equality

configurations :: Int -> [Configuration]
configurations n = zipWith (\i b -> Configuration i (B b)) [1..] 
                        (configurations' n [1..n])
configurations' :: Int -> [Int] -> [[Int]]
configurations' n xs 
  | n > 0  = [x:t | x <- xs, t <- configurations' (n-1) xs]
  | n <= 0 = [[]]

queensFilter :: [Configuration] -> [Configuration]
queensFilter = observe "queensFilter" queensFilter'
queensFilter' (Configuration i b:t)
  | safe b    = Configuration i b : t
  | otherwise = t

doit2 :: IO ()
doit2 = printOwp properties2 (b1,b2)
  where ((Configuration _ b1) : (Configuration _ b2) : _) = queensFilter cs
        cs = (Configuration 1 (B [1,1,1,1])) : (Configuration 2 (B [2,4,1,3])) : [(Configuration 3 (B [1,1,1,1]))]

--------------------------------------------------------------------------------

showBoard :: Board -> String 
showBoard = observe "showBoard" showBoard'
showBoard' (B b) =
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
  in unlines (concat (zipWith rank [1..] b))



