-- Based on the TrivialServer-example from Marlow, Parallel and Concurrent
-- Programming in Haskell

{-# LANGUAGE StandaloneDeriving #-}

import System.IO
import Network
import Text.Printf
import Control.Monad
import Control.Concurrent
import GHC.Stack(getCurrentCCS)
import Debug.Hoed(ccsToStrings,gdmobserve,Observable(..),runO,send)
import System.IO.Unsafe
import Data.List

twotimes :: Integer -> Integer
twotimes j = (gdmobserve "twotimes"
             ( \i -> {-# SCC "twotimes" #-} 
                2 + i -- bug: should be 2 * i
             )) j

double :: String -> String
double = gdmobserve "double" 
         $ \s -> {-# SCC "double" #-} show (twotimes (read s :: Integer))

loop h = do
  ccs <- getCurrentCCS ()
  ss <- ccsToStrings ccs
  hPutStrLn h $ "hello " ++ show h ++ " \"loop\" stack is " ++ show ss
  line <- hGetLine h
  if line == "end"
     then do hPutStrLn h ("Thank you for using the Haskell doubling service.")
             putStrLn $ "Terminated client " ++ show h
     else do hPutStrLn h (double line)
             loop h

talk :: Handle -> IO ()
talk h = do 
  hSetBuffering h LineBuffering
  i <- myThreadId
  hPutStrLn h $ "Welcome on thread " ++ show i
  loop h

port :: Int
port = 44444

mainloop :: Socket -> IO ()
mainloop = gdmobserve "mainloop" (\sock -> {-# SCC "mainloop" #-} mainloop' sock)
  where mainloop' sock = do
                            (handle, host, port) <- accept sock
                            printf "Accepted connection from %s: %s\n" host (show port)
                            forkFinally (talk handle) (\_ -> hClose handle)
                            mainloop sock

main :: IO ()
main = runO slices $ withSocketsDo $ do
  sock <- listenOn (PortNumber (fromIntegral port))
  printf "Listening on port %d\n" port
  mainloop sock

slices = []

instance Observable Handle where observer h = send (show h) (return h)
instance Observable Socket where observer s = send "socket" (return s)
