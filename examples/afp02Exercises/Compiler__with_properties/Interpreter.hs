module Interpreter(obey,prop_ifT) where

import Syntax
import Behaviour
import Value
import Debug.Hoed.Pure
import Test.QuickCheck

type Env = [(Name,Value)]

obey :: Command -> Trace Value
obey = observe "obey" obey_
obey_ p = if prop_traceLessThan100Deep trc then trc else error "Error: Stuck"
  where trc = fst (run p [])

look :: Name -> Env -> Value
look = observe "look" look'
look' x s = maybe Wrong id (lookup x s)

update :: Name -> Value -> Env -> Env
update = observe "update" update'
update' x a s = (x,a) : filter (\(y,_) -> y/=x) s


----

run :: Command -> Env -> (Trace Value, Env)
run = observe "run" run'
run' Skip        s = (End, s)
run' (x := e)    s = (End, update x (eval e s) s)
run' (p :-> q)   s = let (outp, sp) = run p s
                         (outq, sq) = run q sp
                     in (outp +++ outq, sq)
run' (If e p q)  s = case eval e s of
                     Log True  -> run q s
                     Log False -> run p s
                     _         -> (Crash, s)
run' (While e p) s = case eval e s of
                     Log True  -> let (outp,sp) = run p s
                                      (outw,sw) = run (While e p) sp
                                  in (outp +++ Step outw, sw)
                     Log False -> (End, s)
                     _         -> (Crash, s)
run' (Print e)   s = (eval e s :> End, s)

prop_ifT :: Command -> Env -> Bool
prop_ifT (If _ p q) s = run (If true p q) s == run p s
  where true = Val (Log True)
prop_ifT _ _ = True

c1 q = If (Duo Less (Var "x") (Var "y")) ("y" := (Duo Sub (Var "y") (Var "x"))) q
s1 = ("y", Num 58) : ("x", Num 148) : (error "unevaluated state")

p1 = quickCheck (\q -> prop_ifT (c1 q) s1)

----

eval :: Expr -> Env -> Value
eval = observe "eval" eval'
eval' (Var x)      s = look x s
eval' (Val v)      s = v
eval' (Uno op a)   s = uno op (eval a s)
eval' (Duo op a b) s = duo op (eval a s) (eval b s)
