import Debug.Hoed.Observe

main = runO [] $ print (gdmobserve "main" $ 42 :: Int)
