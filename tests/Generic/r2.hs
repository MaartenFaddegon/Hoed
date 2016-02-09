import Debug.Hoed.Pure

data D = D D | T
  deriving (Show)

instance Observable D where
  observer (D x) = send "D" $ return D << x
  observer T     = send "T" $ return T
  constrain = undefined

f :: D -> Int
f = observe "f" f'
f' T     = 1
f' (D _) = 0

main = logO "r0" $ print (f (D (D (D T))))
