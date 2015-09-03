{-# LANGUAGE DeriveGeneric #-}
module MyModule where
import Debug.Hoed.Pure

--------------------------------------------------------------------------------
-- Data types

data Expr = Mul Expr Expr | Div Expr Expr | Const Int
  deriving (Eq,Show,Generic)

instance Observable Expr

--------------------------------------------------------------------------------
-- The simplifier we want to test

simplify :: Expr -> Expr
simplify = observe "simplify" simplify'
simplify' (Mul e1 e2) = (one . zero) $ Mul (simplify e1) (simplify e2)
simplify' (Div e1 e2) = (one . zero) $ Div (simplify e1) (simplify e2)
simplify' e           = (one . zero) e

one = observe "one" one'
one' (Mul expr (Const 1)) = expr
one' (Mul e1 e2)          = Mul e2 e1
one' (Div expr (Const 1)) = expr
one' expr                 = expr

zero = observe "zero" zero'
zero' (Mul expr (Const 0)) = Const 0
zero' (Div expr (Const 0)) = Const 0
zero' expr                 = expr

--------------------------------------------------------------------------------
-- The propositions

idem :: Eq a => (a -> a) -> a -> Bool
idem f x = f x == (f . f) x

prop_idemSimplify :: Expr -> Bool
prop_idemSimplify = idem simplify

prop_idemOne :: Expr -> Bool
prop_idemOne = idem one

prop_idemZero :: Expr -> Bool
prop_idemZero = idem zero

prop_never :: Expr -> Bool
prop_never _ = False


--------------------------------------------------------------------------------
-- "generated" propositions
{-
 -
p1,p2,p3,p4,p5,p6,p7,p8,p9 :: Bool

p1 = prop_idemSimplify (Mul (Const 1) (Const 2))  -- False
p2 = prop_idemSimplify (Const 1)                  -- True
p3 = prop_idemSimplify (Const 2)                  -- True

p4 = prop_idemZero (Mul (Const 1) (Const 2))      -- True
p5 = prop_idemZero (Const 1)                      -- True
p6 = prop_idemZero (Const 2)                      -- True

p7 = prop_idemOne (Mul (Const 1) (Const 2))       -- False
p8 = prop_idemOne (Const 1)                       -- True
p9 = prop_idemOne (Const 2)                       -- True
-}
