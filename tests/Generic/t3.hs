{-# LANGUAGE DeriveGeneric #-}
import Debug.Hoed.Pure

data Expr = Mul Expr Expr | Const Int | Exc
  deriving (Show,Generic)

instance Observable Expr

one = observe "one" one'
one' (Mul expr (Const 1)) = expr
one' expr                 = expr

main = logO "g" $ print $ one (Mul (Const 1) (Const 1))
