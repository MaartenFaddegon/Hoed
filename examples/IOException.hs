-- import Debug.Hoed
import Debug.Hood.Observe
import GHC.IO(failIO)

main = runO $ x >>= print

x :: IO Int
x = observe "x" (failIO "Failed to get x!")
