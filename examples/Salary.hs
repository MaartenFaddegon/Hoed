{-# LANGUAGE DeriveGeneric #-}
import Debug.Hoed.Pure

avgSalary :: [Employee] -> Float
f         :: [Employee] -> Float
avg       :: Float -> Float -> Employee -> Float

------------------------------------------------------------
-- defective computation of avarage salary (always 0.0)

data Employee = Employee {getName :: String, getSalary :: Float} deriving Generic

avgSalary es = foldl (avg 1.0) 0.0 es
-- avgSalary es = foldl (avg (f es)) 0.0 es
f es = toFloat (1 `div` (length es))
avg' x acc (Employee _ s) = acc + (x * s)

instance Observable Employee
-- avgSalary = observe "avgSalary" avgSalary'
-- f = observe "f" f'
avg = observe "avg" avg'

------------------------------------------------------------
-- properties

newtype Positive a = Positive {getPositive :: a} deriving Show

prop_avgSalaryPositive :: [Positive Float] -> Bool
prop_avgSalaryPositive ss = avgSalary (map mkEmployee ss) > 0.0
  where mkEmployee (Positive s) = Employee "X" s

------------------------------------------------------------

main = testO prop_avgSalaryPositive ss
  where ss = [Positive 3000.0, Positive 1800.0]
        employees = [Employee "Aafje" 3000, Employee "Ben" 2000]

toFloat :: Int -> Float
toFloat = fromInteger . toInteger
