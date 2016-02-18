module Properties where
import Debug.Hoed.Pure
import Data.List
import Test.QuickCheck
import Data.Maybe
import Test.QuickCheck
import Test.QuickCheck.Property
import Test.QuickCheck.Gen
import Test.QuickCheck.Random
import Control.Monad
import Types

properties = [ Propositions 
                [ (BoolProposition,m1,"prop_queens_sound",[SubjectFunction,Argument 0])
                , (QuickCheckProposition,m1,"prop_queens_complete",[SubjectFunction,Argument 0,Random])
                ] Specify "queens" [m2]
             , Propositions 
                [ (BoolProposition,m1,"prop_valid_sound",[SubjectFunction,Argument 0,Argument 1])
                , (QuickCheckProposition,m1,"prop_valid_complete"
                , [SubjectFunction,Argument 0,Argument 1,Random])
                ] Specify "valid" [m2]
             , Propositions 
                [ (BoolProposition,m1,"prop_extend_sound",[SubjectFunction,Argument 0,Argument 1])
                , (BoolProposition,m1,"prop_extend_complete",[SubjectFunction,Argument 0,Argument 1])
                ] Specify "extend" [m2]
             , Propositions 
                [ (QuickCheckProposition,m1,"spec_safe",[SubjectFunction,Argument 0])
                ] Specify "safe" [m2]
             , Propositions 
                [ (QuickCheckProposition,m1,"spec_no_threat",[SubjectFunction,Argument 0,Argument 1,Argument 2])
                ] Specify "no_threat" [m2]
             ]
  where m1 = Module "Properties" "../examples/Queens__with_properties/"
        m2 = Module "Queens"     "../examples/Queens__with_properties/"

prop_queens_sound q n      = all (\b -> completeSolution n b && validSolution n n b) (q n)
prop_queens_complete q n b = completeSolution n b && validSolution n n b ==> b `elem` (q n)

prop_valid_sound :: (Int -> Int -> [Board]) -> Int -> Int -> Bool
prop_valid_sound v m n = all (\b -> completeSolution m b && validSolution m n b) (v m n)

prop_valid_complete v m n b = completeSolution m b && validSolution m n b ==> b `elem` (v m n)

completeSolution m b = length b == m && all (`elem` [1..m]) b

validSolution :: Int -> Int -> Board -> Bool
validSolution 0 n b = b == []
validSolution m n b
  -- = m >= n &&
  = onBoard n b && isSet b 
  && isSet (diagonals n b) && isSet (diagonals n (reverse b))

onBoard :: Int -> Board -> Bool
onBoard n b = all (\i -> i > 0 && i <= n) b

spec_safe :: (Board -> Bool) -> Board -> Property
spec_safe s b = b /= [] && all (>0) b ==> s b == validSolution (maximum b) (maximum b) b

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
prop_extend_sound e n bs 
  | all (completeSolution n) bs = all extend_sound_board (zip3 bs' is ds)
  | otherwise                   = True
  where ds = e n bs
        is = cycle [1..n]
        bs' = foldr (\b z -> replicate n b ++ z) [] bs

extend_sound_board (b, i, [])    = False
extend_sound_board (b, i, (j:e)) = i == j && b == e

