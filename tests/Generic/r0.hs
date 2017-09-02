import Debug.Hoed

data D = D Int 
  deriving Show

instance Observable D where
  observer (D x) = send "D" $ return D << x
  constrain = undefined

f = observe "f" f'
f' x = D x

main = logO "r0" $ print (f 3)
