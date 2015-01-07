-- Based on the TrivialServer-example from Marlow, Parallel and Concurrent
-- Programming in Haskell

{-# LANGUAGE StandaloneDeriving #-}

import System.IO
import Network
import Text.Printf
import Control.Monad
import Control.Concurrent
import Debug.Hoed(gdmobserve,Observable(..),runO,send)
import System.IO.Unsafe
import Data.List

twotimes :: Integer -> Integer
twotimes j = (gdmobserve "twotimes"
             ( \i -> {-# SCC "twotimes" #-} 
                2 + i -- bug: should be 2 * i
             )) j

double :: String -> String
double s' = gdmobserve "double" 
            (\s -> {-# SCC "double" #-} show (twotimes (read s :: Integer))) s'

loop h = do
  line <- hGetLine h
  if line == "end"
     then do hPutStrLn h ("Thank you for using the Haskell doubling service.")
             putStrLn $ "server: Terminated client " ++ show h
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

server :: Int -> Socket -> IO ()
server = gdmobserve "server" (\x sock -> {-# SCC "server" #-} server' x sock)
  where server' 0 _ = putStrLn "server: Shutting down."
        server' x sock = do
          (handle, host, port) <- accept sock
          printf "server: Accepted connection from %s: %s\n" host (show port)
          forkFinally (talk handle) (\_ -> hClose handle)
          server (x-1) sock

client :: Int -> IO ()
client x = do
  h <- connectTo "localhost" (PortNumber (fromIntegral port))
  hSetBuffering h LineBuffering
  let pr s = putStrLn $ "client-" ++ show x ++ ": " ++ s
  
  s <- hGetLine h; pr s -- Get and print the welcome message
  hPutStrLn h (show x)  -- Send x for doubling to the server
  s <- hGetLine h; pr s -- Get and print response from server
  hPutStrLn h "end"     -- Send goodbye message
  s <- hGetLine h; pr s -- Get and print response from server

main :: IO ()
main = runO slices $ gdmobserve "main" $ {-# SCC "main" #-} withSocketsDo $ do
  sock <- listenOn (PortNumber (fromIntegral port))
  printf "server: Listening on port %d.\n" port
  forkIO (server 2 sock) -- Start server in own thread.
  client 2               -- Connect with two clients from this thread to the server.
  client 3
  threadDelay 1000       -- Give server-thread some time to terminate.

slices = []

instance Observable Handle where observer h = send (show h) (return h)
instance Observable Socket where observer s = send "socket" (return s)
