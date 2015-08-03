{-# LANGUAGE DeriveGeneric #-}
import Debug.Hoed.Pure

data D = D D | C Int 
  deriving (Show,Generic)

instance Observable D

f :: D -> Int
f = observe "f" f'
f' (C x) = x
f' (D d) = f d

main = logO "r0" $ print (f (D (C 3)))
