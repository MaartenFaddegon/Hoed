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
snd = observe "snd" (\(x,y) -> x+y)

-- these are "local" properties of flip
prop_flip1,prop_flip2 :: ((Int,Int) -> (Int,Int)) -> (Int,Int) -> Bool
prop_flip1 f p = x == y
  where (_,y) = p
        (x,_) = f p
prop_flip2 f p = x == y
  where (x,_) = p
        (_,y) = f p

spec_fst fst (x,y) = x == fst (x,y)
spec_snd snd (x,y) = y == snd (x,y)


unconstrained :: IO ()
unconstrained = runOwp properties $ let (x,y) = flip (3,0) in print y
  where
  properties = 
    [ Propositions 
        [ (BoolProposition, Module "Flip" ".", "prop_flip1 flip",[0])
        , (BoolProposition, Module "Flip" ".", "prop_flip2 flip",[0])
        ] PropertiesOf "flip" [prel]
    , Propositions
        [ (BoolProposition, Module "Flip" ".", "spec_fst fst",[0])
        ] Specify "fst" [prel]
    , Propositions
        [ (BoolProposition, Module "Flip" ".", "spec_snd snd",[0])
        ] Specify "snd" [prel]
    ]
  prel = Module "Prelude hiding (flip,fst,snd)" ""

constrained :: IO ()
constrained = runOwp properties $ let (x,y) = flip (3,0) in print y
  where
  properties = 
    [ Propositions 
        [ (BoolProposition, Module "Flip" ".", "prop_flip1",[-1,0])
        , (BoolProposition, Module "Flip" ".", "prop_flip2",[-1,0])
        ] PropertiesOf "flip" [prel]
    , Propositions
        [ (BoolProposition, Module "Flip" ".", "spec_fst",[-1,0])
        ] Specify "fst" [prel]
    , Propositions
        [ (BoolProposition, Module "Flip" ".", "spec_snd",[-1,0])
        ] Specify "snd" [prel]
    ]
  prel = Module "Prelude hiding (flip,fst,snd)" ""
