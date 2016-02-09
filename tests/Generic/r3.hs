import Debug.Hoed.Pure

data Expr = Mul Expr Expr | Const Int | Exc
  deriving Show

instance Observable Expr where
  observer (Mul e1 e2) = send "Mul"   $ return Mul << e1 << e2
  observer (Const v)   = send "Const" $ return Const << v
  observer Exc         = send "Exc"   $ return Exc
  constrain = undefined

one = observe "one" one'
one' (Mul expr (Const 1)) = expr
one' expr                 = expr

main = logO "g" $ print $ one (Mul (Const 1) (Const 1))
