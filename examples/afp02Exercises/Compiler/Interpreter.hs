module Interpreter(obey) where

import Syntax
import Behaviour
import Value
import Debug.Hoed.Pure

type Env = [(Name,Value)]

obey :: Command -> Trace Value
obey = observe "obey" obey_
obey_ p = if prop_traceLessThan100Deep trc then trc else error "Stuck (or command is too dificult)"
  where trc = fst (run p [])

look :: Name -> Env -> Value
look x s = maybe Wrong id (lookup x s)

update :: Name -> Value -> Env -> Env
update x a s = (x,a) : filter (\(y,_) -> y/=x) s

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

eval :: Expr -> Env -> Value
eval = observe "eval" eval'
eval' (Var x)      s = look x s
eval' (Val v)      s = v
eval' (Uno op a)   s = uno op (eval a s)
eval' (Duo op a b) s = duo op (eval a s) (eval b s)
