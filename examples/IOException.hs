import Debug.Hoed
-- import Debug.Hood.Observe
import GHC.IO(failIO)
import Control.Exception(catch, SomeException(..))

main = runO [] $ 
  do (x >>= print) `catchAll` \e -> print ("oops-x: " ++ show e)
     (y >>= print) `catchAll` \e -> print ("oops-y: " ++ show e)

x :: IO Int
x = gdmobserve "x" (error "Failed to get x!")

y :: IO Int
y = gdmobserve "y" (failIO "Failed to get e!")

catchAll :: IO a -> (SomeException -> IO a) -> IO a
catchAll = catch
