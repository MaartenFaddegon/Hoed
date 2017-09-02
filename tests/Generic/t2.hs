{-# LANGUAGE DeriveGeneric #-}
import Debug.Hoed

data D = D D | T
  deriving (Show,Generic)

instance Observable D

f :: D -> Int
f = observe "f" f'
f' T     = 1
f' (D _) = 0

main = logO "r0" $ print (f (D (D (D T))))
