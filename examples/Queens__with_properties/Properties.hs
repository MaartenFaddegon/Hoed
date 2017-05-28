module Properties where
import Debug.Hoed
import Data.List
import Data.Maybe
import Test.QuickCheck hiding ((===))
import Test.QuickCheck.Property hiding ((===))
import Test.QuickCheck.Gen
import Test.QuickCheck.Random
import Control.Monad
import Types

properties3 = [ Propositions 
                [ mkProposition m1 "prop_queens_sound"
                    `ofType` BoolProposition
                    `withSignature` [SubjectFunction,Argument 0]
                    `sizeHint` 4
                , mkProposition m1 "prop_queens_complete" 
                     `ofType` QuickCheckProposition
                     `withSignature` [SubjectFunction,Argument 0,Random]
                     `sizeHint` 4
                ] Specify "queens" [m2a,m3]
             , Propositions 
                [ mkProposition m1 "prop_valid_sound" 
                    `ofType` BoolProposition
                    `withSignature` [SubjectFunction,Argument 0,Argument 1]
                     `sizeHint` 4
                , mkProposition m1 "prop_valid_complete" 
                    `ofType` QuickCheckProposition
                    `withSignature` [SubjectFunction,Argument 0,Argument 1,Random]
                     `sizeHint` 4
                ] Specify "valid" [m2a,m3]
             , Propositions 
                [ mkProposition m1 "prop_extend_sound"
                    `ofType` BoolProposition `withSignature` [SubjectFunction,Argument 0,Argument 1]
                , mkProposition m1 "prop_extend_complete"
                    `ofType` BoolProposition `withSignature` [SubjectFunction,Argument 0,Argument 1]
                ] Specify "extend" [m2a,m3]
             , Propositions 
                [ mkProposition m1 "spec_safe"
                    `ofType` QuickCheckProposition `withSignature` [SubjectFunction,Argument 0]
                ] Specify "safe" [m2a,m3]
             , Propositions 
                [ mkProposition m1 "spec_no_threat"
                    `ofType` QuickCheckProposition `withSignature` [SubjectFunction,Argument 0,Argument 1,Argument 2]
                ] Specify "no_threat" [m2a,m3]
             ]
  where
  m2a = Module "Queens3"     "../examples/Queens__with_properties/"

properties2 = [ Propositions 
                [ mkProposition m1 "spec_queensFilter"
                    `ofType` BoolProposition
                    `withSignature` [SubjectFunction,Argument 0]
                , mkProposition m1 "spec_queensFilter_p"
                    `ofType` BoolProposition
                    `withSignature` [SubjectFunction,Argument 0]
                ] Specify "queensFilter" [m2,m3]
              ]

properties = [ Propositions 
                [ mkProposition m1 "prop_queens_sound"
                    `ofType` BoolProposition
                    `withSignature` [SubjectFunction,Argument 0]
                    `sizeHint` 4
                , mkProposition m1 "prop_queens_complete" 
                     `ofType` QuickCheckProposition
                     `withSignature` [SubjectFunction,Argument 0,Random]
                     `sizeHint` 4
                ] Specify "queens" [m2,m3]
             , Propositions 
                [ mkProposition m1 "prop_valid_sound" 
                    `ofType` BoolProposition
                    `withSignature` [SubjectFunction,Argument 0,Argument 1]
                     `sizeHint` 4
                , mkProposition m1 "prop_valid_complete" 
                    `ofType` QuickCheckProposition
                    `withSignature` [SubjectFunction,Argument 0,Argument 1,Random]
                     `sizeHint` 4
                ] Specify "valid" [m2,m3]
             , Propositions 
                [ mkProposition m1 "prop_extend_sound"
                    `ofType` BoolProposition `withSignature` [SubjectFunction,Argument 0,Argument 1]
                , mkProposition m1 "prop_extend_complete"
                    `ofType` BoolProposition `withSignature` [SubjectFunction,Argument 0,Argument 1]
                ] Specify "extend" [m2,m3]
             , Propositions 
                [ mkProposition m1 "spec_safe"
                    `ofType` QuickCheckProposition `withSignature` [SubjectFunction,Argument 0]
                ] Specify "safe" [m2,m3]
             , Propositions 
                [ mkProposition m1 "spec_no_threat"
                    `ofType` QuickCheckProposition `withSignature` [SubjectFunction,Argument 0,Argument 1,Argument 2]
                ] Specify "no_threat" [m2,m3]
             ]

m1 = Module "Properties" "../examples/Queens__with_properties/"
m2 = Module "Queens"     "../examples/Queens__with_properties/"
m3 = Module "Types"     "../examples/Queens__with_properties/"

prop_queens_set q n = all (\(B b) -> isSet b) (q n)

prop_safe_set s b = not (isSet b && s (B b))

prop_queens_sound q n      = all (\b -> completeSolution n b && validSolution n n b) (q n)
prop_queens_complete q n b = completeSolution n b && validSolution n n b ==> b `elem` (q n)

prop_valid_sound :: (Int -> Int -> [Board]) -> Int -> Int -> Bool
prop_valid_sound v m n = all (\b -> validSolution m n b) (v m n)

prop_valid_complete v m n b = completeSolution m b && validSolution m n b ==> b `elem` (v m n)

completeSolution :: Int -> Board -> Bool
completeSolution m (B b) = length b == m && all (`elem` [1..m]) b

norepeats (B b) = nub b == b

validSolution' :: Board -> Bool
validSolution' (B [])  = True
validSolution' (B [x]) = True
validSolution' (B b)   = validSolution (maximum b) (max (length b) (maximum b)) (B b)

validSolution :: Int -> Int -> Board -> Bool
validSolution 0 n (B b) = b == []
validSolution m n (B b)
  -- = m >= n &&
  = onBoard n b && isSet b
  && isSet (diagonals n b) && isSet (diagonals n (reverse b))

onBoard :: Int -> [Int] -> Bool
onBoard n b = all (\i -> i > 0 && i <= n) b

spec_safe :: (Board -> Bool) -> Board -> Property
spec_safe s b = nonEmptyBoard b && validSolution' (btail b) ==> s b == validSolution' b

nonEmptyBoard (B b) = b /= []

spec_no_threat :: (Int -> Board -> Int -> Bool) -> Int -> Board -> Int -> Property
spec_no_threat n a (B b) m = validSolution' (B b) ==> validSolution' (B (a:b)) == n a (B b) m

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
diagonals :: Int -> [Int] -> [Int]
diagonals n b = zipWith (-) b [1..n]

prop_extend_complete e n bs = length (e n bs) == n * (length bs)

prop_extend_sound :: (Int -> [Board] -> [Board]) -> Int -> [Board] -> Bool
prop_extend_sound e n bs 
  | all (completeSolution n) bs && all norepeats bs
      = all extend_sound_board (zip3 bs' is ds)
  | otherwise                   = True
  where ds = e n bs
        is = cycle [1..n]
        bs' = foldr (\b z -> replicate n b ++ z) [] bs

extend_sound_board (B b, i, B [])    = False
extend_sound_board (B b, i, B (j:e)) = i == j && b == e

btail (B b) = B (tail b)
bnull (B b)  = null b
bcon a (B b) = B (a:b)
ball f (B b) = all f b

spec_queensFilter_p, spec_queensFilter :: ([Configuration] -> [Configuration]) -> [Configuration] -> Bool
spec_queensFilter   q cs = q cs == filter validConfiguration cs
spec_queensFilter_p q cs = q cs === filter validConfiguration cs

validConfiguration (Configuration i b) = validSolution' b
