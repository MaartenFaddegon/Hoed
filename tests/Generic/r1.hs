import Debug.Hoed.Pure

data D = D D | C Int 
  deriving Show

instance Observable D where
  observer (D x) = send "D" $ return D << x
  observer (C i) = send "C" $ return C << i
  constrain = undefined

f :: D -> Int
f = observe "f" f'
f' (C x) = x
f' (D d) = f d

main = logO "r0" $ print (f (D (C 3)))
