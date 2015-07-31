import Debug.Hoed.Pure

data D = D D | C Int 
  deriving Show

instance Observable D where
  observer (D x) = send "D" $ return D << x

f = observe "f" f'
f' (C x) = x
f' (D d) = d

main = logO "r0" $ print (f (D (C 3)))
