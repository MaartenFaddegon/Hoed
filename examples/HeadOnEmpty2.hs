import Debug.Hoed
import GHC.IO(failIO)
import Control.Exception(catch,SomeException)

main = runO [] $ do
  (print (h xs))     `catch` (handleExc "First one went wrong:")
  ((f xs) >>= print) `catch` (handleExc "Second one went wrong:")

-- Functions like 'readLn' use failIO. These exception are NOT traced.
f :: [Int] -> IO Int
f = gdmobserve "f" (\ys -> failIO "Oops from f")

-- Functions like 'head' use error. These exceptions are traced.
h :: [Int] -> Int
h = gdmobserve "h" (\ys -> error "Oops from h!")

xs :: [Int]
xs = gdmobserve "xs" ({-# SCC "xs" #-} [])

handleExc :: String -> SomeException -> IO ()
handleExc s e = putStrLn (s ++ show e)
