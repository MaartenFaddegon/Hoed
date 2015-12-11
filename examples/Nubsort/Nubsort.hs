-- An implementation of nub that sorts a list with a defective implementation of insertion sort and removes recurring elements from the resulting ordered list.
module Nubsort where
import Test.QuickCheck
import Debug.Hoed.Pure

-- Types and annatations

nub,nub_         :: (Observable a, Ord a) => [a] -> [a]
nubord,nubord_   :: (Observable a, Ord a) => [a] -> [a]
nubord',nubord'_ :: (Observable a, Ord a) => a -> [a] -> [a]
insert,insert_   :: (Observable a, Ord a) => a -> [a] -> [a]

nub     = observe "nub" nub_
nubord  = observe "nubord" nubord_
nubord' = observe "nubord'" nubord'_
insert  = observe "insert" insert_

-- The implementation

nub_ xs = nubord (foldr insert [] xs)

nubord_ xs = foldr nubord' [] xs
nubord'_ x []    = [x]
nubord'_ x (y:t) = if x == y then x:t else x:y:t

insert_ x []    = [x]
insert_ x (y:t) = if x < y then x : y : t else y : x : t -- DEFECT

-- A corrected version of the previous line could be
--    insert_ x (y:t) = if x < y then x : y : t else y : insert x t

-- Properties

prop_nub_idempotent :: [Int] -> Bool
prop_nub_idempotent xs = let xs' = nub xs in nub xs' == xs'

prop_nub_complete :: [Int] -> Bool
prop_nub_complete xs = complete xs (nub xs)

prop_nub_unique :: [Int] -> Bool
prop_nub_unique xs = unique (nub xs)

--

prop_insert_complete :: Int -> [Int] -> Property
prop_insert_complete x xs = ordered xs ==> complete (x:xs) (insert x xs)

prop_insert_ordered :: Int -> [Int] -> Property
prop_insert_ordered x xs = ordered xs ==> ordered (insert x xs)

--

prop_nubord'_idempotent :: Int -> [Int] -> Property
prop_nubord'_idempotent x xs = unique xs && ordered (x:xs) ==> xs' == nubord' x xs'
  where xs' = nubord' x xs

prop_nubord'_complete :: [Int] -> Int -> Property
prop_nubord'_complete xs x = ordered xs  && x `elem` xs ==> x `elem` (nub xs)

prop_nubord'_unique :: Int -> [Int] -> Property
prop_nubord'_unique x xs = unique xs && ordered (x:xs) ==> unique (nubord' x xs)

--

ordered []  = True
ordered [x] = True
ordered (x:y:t) = x <= y && ordered (y:t)

unique []     = True
unique (x:xs) = not (x `elem` xs) && unique xs

complete xs ys = all (\z -> z `elem` xs && z `elem` ys) (xs ++ ys)
