{-|
Module      : Debug.Hoed.Pure
Description : Lighweight algorithmic debugging based on observing intermediate values.
Copyright   : (c) 2000 Andy Gill, (c) 2010 University of Kansas, (c) 2013-2015 Maarten Faddegon
License     : BSD3
Maintainer  : hoed@maartenfaddegon.nl
Stability   : experimental
Portability : POSIX

Hoed is a tracer and debugger for the programming language Haskell.

Hoed.Pure is recommended over Hoed.Stk: in contrast to Hoed.Stk you can optimize your program and do not need to enable profiling when using Hoed.Pure.

To locate a defect with Hoed.Pure you annotate suspected functions and compile as usual. Then you run your program, information about the annotated functions is collected. Finally you connect to a debugging session using a webbrowser.

Let us consider the following program, a defective implementation of a parity function with a test property.

> import Test.QuickCheck
>
> isOdd :: Int -> Bool
> isOdd n = isEven (plusOne n)
>
> isEven :: Int -> Bool
> isEven n = mod2 n == 0
>
> plusOne :: Int -> Int
> plusOne n = n + 1
>
> mod2 :: Int -> Int
> mod2 n = div n 2
>
> prop_isOdd :: Int -> Bool
> prop_isOdd x = isOdd (2*x+1)
>
> main :: IO ()
> main = printO (prop_isOdd 1)
>
> main :: IO ()
> main = quickcheck prop_isOdd

Using the property-based test tool QuickCheck we find the counter example `1` for our property.

> ./MyProgram
> *** Failed! Falsifiable (after 1 test): 1

Hoed can help us determine which function is defective. We annotate the functions `isOdd`, `isEven`, `plusOne` and `mod2` as follows:

> import Debug.Hoed.Pure
>
> isOdd :: Int -> Bool
> isOdd = observe "isOdd" isOdd'
> isOdd' n = isEven (plusOne n)
>
> isEven :: Int -> Bool
> isEven = observe "isEven" isEven'
> isEven' n = mod2 n == 0
>
> plusOne :: Int -> Int
> plusOne = observe "plusOne" plusOne'
> plusOne' n = n + 1
>
> mod2 :: Int -> Int
> mod2 = observe "mod2" mod2'
> mod2' n = div n 2
>
> prop_isOdd :: Int -> Bool
> prop_isOdd x = isOdd (2*x+1)
>
> main :: IO ()
> main = printO (prop_isOdd 1)

After running the program a computation tree is constructed and displayed in a web browser.

> ./MyProgram
> False
> Listening on http://127.0.0.1:10000/

After running the program a computation tree is constructed and displayed in a
web browser. You can freely browse this tree to get a better understanding of
your program. If your program misbehaves, you can judge the computation
statements in the tree as 'right' or 'wrong' according to your intention. When
enough statements are judged the debugger tells you the location of the fault
in your code.

<<https://raw.githubusercontent.com/MaartenFaddegon/Hoed/master/screenshots/AlgorithmicDebugging.png>>

Read more about Hoed on its project homepage <https://wiki.haskell.org/Hoed>.

Papers on the theory behind Hoed can be obtained via <http://maartenfaddegon.nl/#pub>.

I am keen to hear about your experience with Hoed: where did you find it useful and where would you like to see improvement? You can send me an e-mail at hoed@maartenfaddegon.nl, or use the github issue tracker <https://github.com/MaartenFaddegon/hoed/issues>.
-}

{-# LANGUAGE CPP #-}

module Debug.Hoed.Pure
  ( -- * Basic annotations
    observe
  , runO
  , testO
  , printO
  , logO

  -- * Property-based judging
  , runOwp
  , testOwp
  , logOwp
  , Propositions(..)
  , PropType(..)
  , Proposition(..)
  , PropositionType(..)
  , Module(..)
  , UnevalHandler(..)

    -- * Experimental annotations
  , traceOnly
  , observeTempl
  , observedTypes

   -- * For use by Hoed-generated programs that test some property ...
  , ParEq(..) -- MF TODO: this should become part of Observable
  , runOstore
  , conAp

   -- * The Observable class
  , Observer(..)
  , Observable(..)
  , (<<)
  , thunk
  , nothunk
  , send
  , observeBase
  , observeOpaque
  , debugO
  , CDS(..)
  , Generic
  ) where


import Debug.Hoed.Pure.Observe
import Debug.Hoed.Pure.Render
import Debug.Hoed.Pure.EventForest
import Debug.Hoed.Pure.CompTree
import Debug.Hoed.Pure.DemoGUI
import Debug.Hoed.Pure.Prop
import Debug.Hoed.Pure.Serialize
import Paths_Hoed(getDataDir)

import Prelude hiding (Right)
import qualified Prelude
import System.Process(system)
import System.IO
import Data.Maybe
import Control.Monad
import Data.List
import Data.Ord
import Data.Char
import System.Environment
import System.Directory(createDirectoryIfMissing)

import Language.Haskell.TH
import GHC.Generics

import Data.IORef
import System.IO.Unsafe
import Data.Graph.Libgraph
import Graphics.UI.Threepenny(startGUI,defaultConfig,Config(..))

import System.Directory(createDirectoryIfMissing)


-- %************************************************************************
-- %*                                                                   *
-- \subsection{External start functions}
-- %*                                                                   *
-- %************************************************************************

-- Run the observe ridden code.


runOnce :: IO ()
runOnce = do
  f <- readIORef firstRun
  case f of True  -> writeIORef firstRun False
            False -> error "It is best not to run Hoed more that once (maybe you want to restart GHCI?)"

firstRun :: IORef Bool
firstRun = unsafePerformIO $ newIORef True


-- | run some code and return the Trace
debugO :: IO a -> IO Trace
debugO program =
     do { runOnce
        ; initUniq
        ; startEventStream
        ; let errorMsg e = "[Escaping Exception in Code : " ++ show e ++ "]"
        ; ourCatchAllIO (do { program ; return () })
                        (hPutStrLn stderr . errorMsg)
        ; endEventStream
        }

-- | The main entry point; run some IO code, and debug inside it.
--   After the IO action is completed, an algorithmic debugging session is started at
--   @http://localhost:10000/@ to which you can connect with your webbrowser.
--
-- For example:
--
-- @
--   main = runO $ do print (triple 3)
--                    print (triple 2)
-- @

runO :: IO a -> IO ()
runO program = do
  (trace,traceInfo,compTree,frt) <- runO' Verbose program
  debugSession trace traceInfo compTree frt []
  return ()


-- | Hoed internal function that stores a serialized version of the tree on disk (assisted debugging spawns new instances of Hoed).
runOstore :: String -> IO a -> IO ()
runOstore tag program = do 
  (trace,traceInfo,compTree,frt) <- runO' Silent program
  storeTree (treeFilePath ++ tag) compTree
  storeTrace (traceFilePath ++ tag) trace

-- | Repeat and trace a failing testcase
testO :: Show a => (a->Bool) -> a -> IO ()
testO p x = runO $ putStrLn $ if (p x) then "Passed 1 test."
                                       else " *** Failed! Falsifiable: " ++ show x

-- | Use property based judging.

runOwp :: [Propositions] -> IO a -> IO ()
runOwp ps program = do
  (trace,traceInfo,compTree,frt) <- runO' Verbose program
  let compTree' = compTree
{-
  hPutStrLn stderr "\n=== Evaluating assigned properties ===\n"
  compTree' <- judge propVarError trace ps compTree

  let vs = filter (/= RootVertex) (vertices compTree')
      showLen p = show . length . (filter p) $ vs
  hPutStrLn stderr "\n=== Evaluated assigned properties for all computation statements ==="
  hPutStrLn stderr $ showLen isWrong    ++ " statements are now wrong because they fail a property."
  hPutStrLn stderr $ showLen isAssisted ++ " statements satisfy some properties and violate no properties."
  hPutStrLn stderr $ showLen isRight    ++ " statements are right because they satisfy their specification."
  case (findFaulty_dag getJudgement compTree') of
    []    -> do hPutStrLn stderr "\n=== Starting interactive debug session ===\n"
                debugSession trace traceInfo compTree' frt
    (v:_) -> hPutStrLn stderr $ "Fault detected in:\n\n" ++ show (vertexStmt v)
-}
  debugSession trace traceInfo compTree' frt ps
  return ()

-- | Repeat and trace a failing testcase
testOwp :: Show a => [Propositions] -> (a->Bool) -> a -> IO ()
testOwp ps p x = runOwp ps $ putStrLn $ 
  if (p x) then "Passed 1 test."
  else " *** Failed! Falsifiable: " ++ show x

-- | Short for @runO . print@.
printO :: (Show a) => a -> IO ()
printO expr = runO (print expr)

-- | Only produces a trace. Useful for performance measurements.
traceOnly :: IO a -> IO ()
traceOnly program = do
  debugO program
  return ()


data Verbosity = Verbose | Silent

condPutStrLn :: Verbosity -> String -> IO ()
condPutStrLn Silent _  = return ()
condPutStrLn Verbose msg = hPutStrLn stderr msg


runO' :: Verbosity -> IO a -> IO (Trace,TraceInfo,CompTree,EventForest)
runO' verbose program = do
  createDirectoryIfMissing True ".Hoed/"
  condPutStrLn verbose "=== program output ===\n"
  events <- debugO program
  condPutStrLn verbose"\n=== program terminated ==="
  condPutStrLn verbose"Please wait while the computation tree is constructed..."

  let cdss = eventsToCDS events
  let cdss1 = rmEntrySet cdss
  let cdss2 = simplifyCDSSet cdss1
  let eqs   = renderCompStmts cdss2

  let frt  = mkEventForest events
      ti   = traceInfo (reverse events)
      ds   = dependencies ti
      ct   = mkCompTree eqs ds

  writeFile ".Hoed/Events"     (unlines . map show . reverse $ events)
#if defined(TRANSCRIPT)
  writeFile ".Hoed/Transcript" (getTranscript events ti)
#endif
  
  condPutStrLn verbose "\n=== Statistics ===\n"
  let e  = length events
      n  = length eqs
      b  = fromIntegral (length . arcs $ ct ) / fromIntegral ((length . vertices $ ct) - (length . leafs $ ct))
  condPutStrLn verbose $ show e ++ " events"
  condPutStrLn verbose $ show n ++ " computation statements"
  condPutStrLn verbose $ show ((length . vertices $ ct) - 1) ++ " nodes + 1 virtual root node in the computation tree"
  condPutStrLn verbose $ show (length . arcs $ ct) ++ " edges in computation tree"
  condPutStrLn verbose $ "computation tree has a branch factor of " ++ show b ++ "(i.e the average number of children of non-leaf nodes)"

  condPutStrLn verbose "\n=== Debug Session ===\n"
  return (events, ti, ct, frt)

-- | Trace and write computation tree to file. Useful for regression testing.
logO :: FilePath -> IO a -> IO ()
logO filePath program = {- SCC "logO" -} do
  (_,_,compTree,_) <- runO' Verbose program
  writeFile filePath (showGraph compTree)
  return ()

  where showGraph g        = showWith g showVertex showArc
        showVertex RootVertex = ("\".\"","shape=none")
        showVertex v       = ("\"" ++ (escape . showCompStmt) v ++ "\"", "")
        showArc _          = ""
        showCompStmt       = show . vertexStmt

-- | As logO, but with property-based judging.
logOwp :: UnevalHandler -> FilePath -> [Propositions] -> IO a -> IO ()
logOwp handler filePath properties program = do
  (trace,traceInfo,compTree,frt) <- runO' Verbose program
  hPutStrLn stderr "\n=== Evaluating assigned properties ===\n"
  compTree' <- judgeAll handler trace properties compTree
  writeFile filePath (showGraph compTree')
  return ()

  where showGraph g        = showWith g showVertex showArc
        showVertex RootVertex = ("root","")
        showVertex v       = ("\"" ++ (escape . showCompStmt) v ++ "\"", "")
        showArc _          = ""
        showCompStmt s     = (show . vertexJmt) s ++ ": " ++ (show . vertexStmt) s

------------------------------------------------------------------------
-- Algorithmic Debugging

debugSession :: Trace -> TraceInfo -> CompTree -> EventForest -> [Propositions] -> IO ()
debugSession trace traceInfo tree frt ps
  = do createDirectoryIfMissing True ".Hoed/wwwroot/css"
       dataDir <- getDataDir
       system $ "cp " ++ dataDir ++ "/img/*png .Hoed/wwwroot/"
       system $ "cp " ++ dataDir ++ "/img/*gif .Hoed/wwwroot/"
       startGUI defaultConfig
           { jsPort       = Just 10000
           , jsStatic     = Just "./.Hoed/wwwroot"
           } (guiMain trace tree frt ps)
