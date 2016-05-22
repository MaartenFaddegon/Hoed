module Properties where
import Debug.Hoed.Pure
import Test.QuickCheck hiding ((===))
import Test.QuickCheck.Property hiding ((===))
import Test.QuickCheck.Gen
import Test.QuickCheck.Random

prop_filter_t :: ((Int -> Bool) -> [Int] -> [Int]) -> (Int -> Bool) -> [Int] -> Int -> Property
prop_filter_t filter_ p xs x =
 x `elem` xs && p x ==> x `elem` (filter_ p xs)

prop_filter_f :: ((Int -> Bool) -> [Int] -> [Int]) -> (Int -> Bool) -> [Int] -> Int -> Property
prop_filter_f filter_ p xs x =
 x `elem` xs && not (p x) ==> not (x `elem` (filter_ p xs))

spec_odds odds_ xs x = 
 x `elem` xs ==> if odd_def x then p else not p
 where p = x `elem` (odds_ xs)

spec_isEven odds_ x = odds_ x == even_def x

even_def x = 2*n == x where n = x `div` 2

odd_def x = 2*n+1 == x where n = x `div` 2

properties = 
 [ Propositions 
    [ mkProposition m1 "prop_filter_t" 
         `ofType` QuickCheckProposition
         `withSignature` [SubjectFunction,Argument 0,Argument 1,Random]
         `sizeHint` 4
    , mkProposition m1 "prop_filter_f" 
         `ofType` QuickCheckProposition
         `withSignature` [SubjectFunction,Argument 0,Argument 1,Random]
         `sizeHint` 4
    ] PropertiesOf "filter" [m2,m3]
 , Propositions
    [ mkProposition m1 "spec_odds" 
         `ofType` QuickCheckProposition
         `withSignature` [SubjectFunction,Argument 0,Random]
         `sizeHint` 4
    ] Specify "odds" [m2]
 , Propositions
    [ mkProposition m1 "spec_isEven" 
         `ofType` BoolProposition
         `withSignature` [SubjectFunction,Argument 0]
         `sizeHint` 4
    ] Specify "isEven" [m2]
 ]

m1 = Module "Properties" path
m2 = Module "Even" path
m3 = Module "Prelude hiding (filter)" ""
path = "../examples/filter__with_properties/"
