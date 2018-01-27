{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
import Debug.Hoed
import qualified Data.List

quicksort :: Observable a => (a -> a -> Bool) -> [a] -> [a]
quicksort = observe "quicksort" quicksort'
quicksort' op [] = []
quicksort' op (x:xs) = id_quicksort $ quicksort op lt ++ [x] ++ quicksort op gt
    where (observe "lt" . id_lt -> lt, observe "gt" -> gt) = partition (`op` x) xs

partition :: Observable a => (a -> Bool) -> [a] -> ([a],[a])
partition = observe "partition" partition'
{-# INLINE partition #-}
partition' p xs = foldr (select p) ([],[]) (id_partition xs)

select :: Observable a => (a -> Bool) -> a -> ([a], [a]) -> ([a], [a])
-- select = observe "select" select'
select p x ~(ts,fs) | p x       = (x:ts,fs)
                     | otherwise = (ts, x:fs)

main = printO $ quicksort (<=) "haskell"

id_quicksort, id_partition,id_lt :: Observable a => a -> a
id_quicksort = observe "id_quicksort" $ \x -> x
id_partition = observe "id_partition" $ \x -> x
id_lt = observe "id_lt" $ \x -> x
