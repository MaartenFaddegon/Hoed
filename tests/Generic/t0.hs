{-# LANGUAGE DeriveGeneric #-}
import Debug.Hoed

data D = D Int 
  deriving (Show,Generic)

instance Observable D

f = observe "f" f'
f' x = D x

main = logO "t0" $ print (f 3)
