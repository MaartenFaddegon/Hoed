module Interpreter(obey) where

import Syntax
import Behaviour
import Value

type Env = [(Name,Value)]

obey :: Command -> Trace Value
obey p = fst (run p [])

look :: Name -> Env -> Value
look x s = maybe Wrong id (lookup x s)

update :: [Name] -> [Value] -> Env -> Env
update xs vs s = zip xs vs ++ filter (\(y,_)->y `notElem` xs) s

run :: Command -> Env -> (Trace Value, Env)
run Skip        s = (End, s)
run (xs := es)  s = (End, update xs (map (`eval` s) es) s)
run (p :-> q)   s = let (outp, sp) = run p s
                        (outq, sq) = run q sp
                    in (outp +++ outq, sq)
run (If e p q)  s = case eval e s of
                    Log True  -> run p s
                    Log False -> run q s
                    _         -> (Crash, s)
run (While e p) s = case eval e s of
                    Log True  -> let (outp,sp) = run p s
                                     (outw,sw) = run (While e p) sp
                                 in (outp +++ Step outw, sw)
                    Log False -> (End, s)
                    _         -> (Crash, s)
run (Print e)   s = (eval e s :> End, s)

eval :: Expr -> Env -> Value
eval (Var x)      s = look x s
eval (Val v)      s = v
eval (Uno op a)   s = uno op (eval a s)
eval (Duo op a b) s = duo op (eval a s) (eval b s)
