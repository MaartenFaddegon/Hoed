{-|
Module      : Debug.Hoed.Notrace
Description : Lighweight algorithmic debugging based on observing intermediate values.
Copyright   : (c) 2016 Maarten Faddegon
License     : BSD3
Maintainer  : hoed@maartenfaddegon.nl
Stability   : experimental
Portability : POSIX

Hoed is a tracer and debugger for the programming language Haskell.

This is a drop-in replacement of the Debug.Hoed.Pure or Debug.Hoed.Stk modules and disables tracing (all functions are variations of id).

Read more about Hoed on its project homepage <https://wiki.haskell.org/Hoed>.

Papers on the theory behind Hoed can be obtained via <http://maartenfaddegon.nl/#pub>.

I am keen to hear about your experience with Hoed: where did you find it useful and where would you like to see improvement? You can send me an e-mail at hoed@maartenfaddegon.nl, or use the github issue tracker <https://github.com/MaartenFaddegon/hoed/issues>.
-}

{-# LANGUAGE DefaultSignatures, CPP #-}

module Debug.NoHoed
( observe
, runO
, printO
, testO
, observeBase
, Observable(..)
, Parent(..)
, Generic(..)
, send
, ObserverM(..)
, (<<)
) where
import GHC.Generics
import System.IO.Unsafe
import Control.Monad

observe :: String -> a -> a
observe _ = id

runO :: IO a -> IO ()
runO program = do
  program
  return ()

printO :: (Show a) => a -> IO ()
printO expr = print expr

testO :: Show a => (a->Bool) -> a -> IO ()
testO p x = putStrLn $ if (p x) then "Passed 1 test."
                                else " *** Failed! Falsifiable: " ++ show x

data Parent = Parent
class Observable a where
        observer  :: a -> Parent -> a 
        default observer :: (Generic a) => a -> Parent -> a
        observer x _ = x

        constrain :: a -> a -> a
        default constrain :: (Generic a) => a -> a -> a
        constrain x _ = x

observeBase :: a -> Parent -> a 
observeBase x _ = x

constrainBase :: a -> a -> a
constrainBase x _ = x

newtype ObserverM a = ObserverM { runMO :: Int -> Int -> (a,Int) }

instance Functor ObserverM where
    fmap  = liftM

#if __GLASGOW_HASKELL__ >= 710
instance Applicative ObserverM where
    pure  = return
    (<*>) = ap
#endif

instance Monad ObserverM where
        return a = ObserverM (\ c i -> (a,i))
        fn >>= k = ObserverM (\ c i ->
                case runMO fn c i of
                  (r,i2) -> runMO (k r) c i2
                )

(<<) :: (Observable a) => ObserverM (a -> b) -> a -> ObserverM b
fn << a = do {fn' <- fn; return (fn' a)}

send :: String -> ObserverM a -> Parent -> a
send _ fn context =
  unsafePerformIO $ do { let (r,portCount) = runMO fn 0 0
                       ; return r
                       }

instance Observable Int where
  observer  = observeBase
  constrain = constrainBase
                                 
instance Observable Bool where
  observer  = observeBase
  constrain = constrainBase
                                 
instance Observable Integer where
  observer  = observeBase
  constrain = constrainBase
                                 
instance Observable Float where
  observer  = observeBase
  constrain = constrainBase
                                 
instance Observable Double where
  observer  = observeBase
  constrain = constrainBase
                                 
instance Observable Char where
  observer  = observeBase
  constrain = constrainBase
                                 
instance Observable () where
  observer  = observeBase
  constrain = constrainBase

instance (Observable a) => Observable [a] where
  observer  = observeBase
  constrain = constrainBase
