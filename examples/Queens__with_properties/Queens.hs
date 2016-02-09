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


-------------------------------------------------------------------------------- The properties

properties = [ Propositions 
                [ (BoolProposition,myModule,"prop_queens_sound",[SubjectFunction,Argument 0])
                , (QuickCheckProposition,myModule,"prop_queens_complete",[SubjectFunction,Argument 0,Random])
                ] Specify "queens" []
             , Propositions 
                [ (BoolProposition,myModule,"prop_valid_sound",[SubjectFunction,Argument 0,Argument 1])
                , (QuickCheckProposition,myModule,"prop_valid_complete"
                , [SubjectFunction,Argument 0,Argument 1,Random])
                ] Specify "valid" []
             , Propositions 
                [ (BoolProposition,myModule,"prop_extend_sound",[SubjectFunction,Argument 0,Argument 1])
                , (BoolProposition,myModule,"prop_extend_complete",[SubjectFunction,Argument 0,Argument 1])
                ] Specify "extend" []
             , Propositions 
                [ (QuickCheckProposition,myModule,"spec_safe",[SubjectFunction,Argument 0])
                ] Specify "safe" []
             , Propositions 
                [ (QuickCheckProposition,myModule,"spec_no_threat",[SubjectFunction,Argument 0,Argument 1,Argument 2])
                ] Specify "no_threat" []
             ]
  where myModule = Module "Queens" "."

prop_queens_sound q n      = all (validSolution n n) (q n)
prop_queens_complete q n b = validSolution n n b ==> b `elem` (q n)

prop_valid_sound :: (Int -> Int -> [Board]) -> Int -> Int -> Bool
prop_valid_sound v m n      = all (validSolution m n) (v m n)

prop_valid_complete v m n b = validSolution m n b ==> b `elem` (v m n)

prop_queens_full q n = all ((==n) . length) (q n)
prop_queens_safe q n = all (all (\t -> null t || safe t) . tails) (q n)
prop_queens_onboard q n = all (all (`elem` [1..n])) (q n)

nSolutions n b = length b == n

completeSolution n b
  = length b == n && all (`elem` [1..n]) b

validSolution :: Int -> Int -> Board -> Bool
validSolution 0 n b = b == []
validSolution m n b 
  = onBoard n b
  && isSet b 
  && isSet (diagonals n b) && isSet (diagonals n (reverse b))

onBoard :: Int -> Board -> Bool
onBoard n b = all (\i -> i > 0 && i <= n) b

-- Uses the function safe from the implementation to test against ...
dodgyValidSolution n b
  = all (\t -> null t || safe t) (tails b)

spec_safe :: (Board -> Bool) -> Board -> Property
spec_safe s b = all (>0) b ==> s b == validSolution (maximum b) (maximum b) b

spec_no_threat :: (Int -> Board -> Int -> Bool) -> Int -> Board -> Int -> Property
spec_no_threat n a b m 
  = False -- validSolution (maximum b) (length b) b
    ==> validSolution (maximum (a:b)) (length (a:b)) (a:b) == n a b m

-- In a set every element occurs exactly once
isSet xs = all (\x -> length (filter (==x) xs) == 1) xs

-- Give for each queen on the board (i.e. the list of column
-- positions) in which diagonal this queen is placed.
--
--  3   2   1   0
--  2   1   0  -1
--  1   0  -1  -2
--  0  -1  -2  -3
--
diagonals :: Int -> Board -> [Int]
diagonals n b = zipWith (-) b [1..n]

prop_extend_complete e n bs = length (e n bs) == n * (length bs)

prop_extend_sound :: (Int -> [Board] -> [Board]) -> Int -> [Board] -> Bool
prop_extend_sound e n bs = all extend_sound_board (zip3 bs' is ds)
  where ds = e n bs
        is = cycle [1..n]
        bs' = foldr (\b z -> replicate n b ++ z) [] bs

extend_sound_board (b, i, [])    = False
extend_sound_board (b, i, (j:e)) = i == j && b == e

-------------------------------------------------------------------------------- The implementation


-- A Board encodes for every row the number of the column in
-- which the queen is placed.
type Board = [Int]

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
 --    = filter safe (extend n (valid (m-1) n))
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
