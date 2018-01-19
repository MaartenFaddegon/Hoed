{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Debug.Hoed.Util where

import System.Clock
import System.IO

data Verbosity = Verbose | Silent

-- | Conditional output to stderr
condPutStr :: Verbosity -> String -> IO ()
condPutStr Silent _    = return ()
condPutStr Verbose msg = hPutStr stderr msg

-- | Conditional output to stderr
condPutStrLn :: Verbosity -> String -> IO ()
condPutStrLn Silent _    = return ()
condPutStrLn Verbose msg = hPutStrLn stderr msg

--------------------------------------------
-- Measuring elapsed time

newtype Seconds = Seconds Double deriving (Eq, Ord, Num)

instance Show Seconds where
  show (Seconds s) = show s ++ " seconds"

stopWatch :: IO (IO Seconds)
stopWatch  = do
  t <- getTime Monotonic
  return $ do
    t' <- getTime Monotonic
    return (toSecs(diffTimeSpec t t'))
  where
       toSecs :: TimeSpec -> Seconds
       toSecs spec = Seconds $ fromIntegral(sec spec) + fromIntegral(nsec spec) * 1e-9
