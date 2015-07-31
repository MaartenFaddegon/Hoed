import Debug.Hoed.Pure

data D = D D | C Int 
  deriving (Show,Generic)

instance Observable D

f = observe "f" f'
f' (C x) = x
f' (D d) = d

main = logO "r0" $ print (f (D (C 3)))
