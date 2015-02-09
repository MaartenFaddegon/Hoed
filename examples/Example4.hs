import Debug.Hoed

main = runO [] $ print (observe "main" $ 42 :: Int)
