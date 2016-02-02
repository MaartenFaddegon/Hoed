-- flip (3,0) = (_,3) ?
--       prop_flipflip2 (3,0) |-> False, give judgement Wrong
-- fst (3, 0) = 3 ?
--       spec_fst (3, 0) |-> True, give judgement Right
-- Incorrect conclusion: flip is defective

module Flip where
import Prelude hiding (flip,fst,snd)
import Debug.Hoed.Pure

flip :: (Int,Int) -> (Int,Int)
flip = observe "flip" (\x -> (snd x, fst x))

fst,snd :: (Int,Int) -> Int
fst = observe "fst" (\(x,y) -> x+y)
snd = observe "snd" (\(x,y) -> y)

-- flipping twice is the identity function
prop_flipflip :: (Int,Int) -> Bool
prop_flipflip x = prop_flipflip1 x && prop_flipflip2 x

-- Currently we ware using these
prop_flipflip1,prop_flipflip2 :: (Int,Int) -> Bool
prop_flipflip1 (x,y) = x == x' where (x',_) = (flip (flip (x,y)))
prop_flipflip2 (x,y) = y == y' where (_,y') = (flip (flip (x,y)))

-- To fix this we could use properties that do not recompute the result, but instead use the given result:
prop_flipflip1',prop_flipflip2' :: (Int,Int) -> (Int,Int) -> Bool
prop_flipflip1 (x,y) (x',y') = x == y'
prop_flipflip2 (x,y) (x',y') = y == x'

-- Or when using the prop_flipflip2; we should trace that application and use the new computation tree.

spec_fst (x,y) = x == fst (x,y)
spec_snd (x,y) = y == snd (x,y)

doit :: IO ()
doit = testOwp properties prop_flipflip (3,0)
  where
  properties = 
    [ Propositions 
        [ (BoolProposition, Module "Flip" ".", "prop_flipflip1",[0])
        , (BoolProposition, Module "Flip" ".", "prop_flipflip2",[0])
        ] PropertiesOf "flip" []
    , Propositions
        [ (BoolProposition, Module "Flip" ".", "spec_fst",[0])
        ] Specify "fst" []
    , Propositions
        [ (BoolProposition, Module "Flip" ".", "spec_snd",[0])
        ] Specify "snd" []
    ]
