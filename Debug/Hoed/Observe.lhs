\begin{code}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE CPP #-}

\end{code}

The file is part of the Haskell Object Observation Debugger,
(HOOD) March 2010 release.

HOOD is a small post-mortem debugger for the lazy functional
language Haskell. It is based on the concept of observation of
intermediate data structures, rather than the more traditional
stepping and variable examination paradigm used by imperative
language debuggers.

Copyright (c) Andy Gill, 1992-2000
Copyright (c) The University of Kansas 2010
Copyright (c) Maarten Faddegon, 2013-2015

All rights reserved. HOOD is distributed as free software under
the license in the file "License", which available from the HOOD
web page, http://www.haskell.org/hood

This module produces CDS's, based on the observation made on Haskell
objects, including base types, constructors and functions.

WARNING: unrestricted use of unsafePerformIO below.

This was ported for the version found on www.haskell.org/hood.


%************************************************************************
%*                                                                      *
\subsection{Exports}
%*                                                                      *
%************************************************************************

\begin{code}
module Debug.Hoed.Observe
{-
  (
   -- * The main Hood API
  
  , observe
  , Observer(..)   -- contains a 'forall' typed observe (if supported).
  , Observable(..) -- Class

   -- * For advanced users, that want to render their own datatypes.
  , (<<)           -- (Observable a) => ObserverM (a -> b) -> a -> ObserverM b
  , thunk          -- (Observable a) => a -> ObserverM a        
  , nothunk
  , send
  , observeBase
  , observeOpaque
  , observedTypes
  , Generic
  , Trace
  , Event(..)
  , Change(..)
  , Parent(..)
  , UID
  , ParentPosition
  , ThreadId(..)
  , isRootEvent
  , initUniq
  , startEventStream
  , endEventStream
  , ourCatchAllIO
  , peepUniq
  ) -} where
\end{code}


%************************************************************************
%*                                                                      *
\subsection{Imports and infixing}
%*                                                                      *
%************************************************************************

\begin{code}
import Prelude hiding (Right)
import qualified Prelude
import Control.Concurrent.MVar
import Control.Monad
import Data.Array as Array
import qualified Data.HashTable.IO as H
import Data.List (sortOn)
import Data.Maybe
import Data.Monoid ((<>))
import Data.Proxy
import Data.Rope.Mutable (Rope, new', write, reset)
import Data.Strict.Tuple (Pair(..))
import Data.Text (Text, pack)
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import Data.Vector.Unboxed (Vector)
import Data.Vector.Unboxed.Deriving
import Data.Vector.Unboxed.Mutable (MVector)
import Data.Word
import Debug.Hoed.Fields
import Debug.Trace

import GHC.Generics

import Data.IORef
import System.IO.Unsafe

\end{code}

\begin{code}
import qualified Control.Exception as Exception
import Control.Exception (throw, SomeException(..))
{-
 ( catch
                , Exception(..)
                , throw
                ) as Exception
-}
import Data.Dynamic ( Dynamic )

\end{code}

%************************************************************************
%*                                                                      *
\subsection{Event stream}
%*                                                                      *
%************************************************************************

Trival output functions

\begin{code}

type UID = Int

data Event = Event
                { eventUID     :: !UID      -- my UID
                , eventParent  :: !Parent
                , change       :: !Change
                }
        deriving (Eq,Generic)

data Change
        = Observe          !Text
        | Cons     !Word8  !Text
        | ConsChar         !Char
        | Enter
        | Fun
        deriving (Eq, Show,Generic)

type ParentPosition = Word8

data Parent = Parent
        { parentUID      :: !UID            -- my parents UID
        , parentPosition :: !ParentPosition -- my branch number (e.g. the field of a data constructor)
        } deriving (Eq,Generic)

instance Show Event where
  show e = (show . eventUID $ e) ++ ": " ++ (show . change $ e) ++ " (" ++ (show . eventParent $ e) ++ ")"

instance Show Parent where
  show p = "P " ++ (show . parentUID $ p) ++ " " ++ (show . parentPosition $ p)

root = Parent 0 0

isRootEvent :: Event -> Bool
isRootEvent e = case change e of Observe{} -> True; _ -> False

data ChangeFlat  = ChangeFlat {changeType :: !Word8, consPort :: !Word8, string :: !Int}
data EventSansId = EventSansId {-# UNPACK #-} !Parent  !ChangeFlat

derivingUnbox "ChangeFlat"
    [t| ChangeFlat -> (Word8, Word8, Int) |]
    [| \ (ChangeFlat a b c) -> (a,b,c) |]
    [| \ (a,b,c) -> ChangeFlat a b c |]

derivingUnbox "Parent"
    [t| Parent -> (UID, ParentPosition) |]
    [| \ (Parent a b) -> (a,b) |]
    [| \ (a,b) -> Parent a b |]

derivingUnbox "EventSansId"
    [t| EventSansId -> (Parent, ChangeFlat) |]
    [| \ (EventSansId a b) -> (a,b) |]
    [| \ (a,b) -> EventSansId a b |]

\end{code}

\begin{code}
type Trace = V.Vector Event

endEventStream :: IO Trace
endEventStream = do
  (stringsCount :!: stringsHashTable) <- takeMVar strings
  unsortedStrings <- H.toList stringsHashTable
  putMVar strings . (0 :!:) =<< H.new
  let stringsTable = V.unsafeAccum (\_ -> id) (V.replicate stringsCount (error "uninitialized")) [(i,s) | (s,i) <- unsortedStrings]
  VG.imap (\ix (EventSansId parent change) ->
                  Event (ix+1) parent (unflatten stringsTable change)) . VG.convert <$>
    reset (Proxy :: Proxy Vector) events

sendEvent :: Int -> Parent -> Change -> IO ()
sendEvent nodeId !parent !change = do
  changeFlat <- flatten change
  write events (nodeId-1) (EventSansId parent changeFlat)

unflatten stringsTable (ChangeFlat 0 _ s) = Observe (V.unsafeIndex stringsTable s)
unflatten stringsTable (ChangeFlat 1 c s) = Cons c  (V.unsafeIndex stringsTable s)
unflatten stringsTable (ChangeFlat 2 _ s) = ConsChar (toEnum s)
unflatten stringsTable (ChangeFlat 3 _ _) = Enter
unflatten stringsTable (ChangeFlat 4 _ _) = Fun

flatten   (Observe s)  = ChangeFlat 0 0 <$> lookupOrAddString s
flatten   (Cons c s)   = ChangeFlat 1 c <$> lookupOrAddString s
flatten   (ConsChar s) = return $ ChangeFlat 2 0 (fromEnum s)
flatten   Enter        = return $ ChangeFlat 3 0 0
flatten   Fun          = return $ ChangeFlat 4 0 0

lookupOrAddString s = do
  (stringsCount :!: stringsTable) <- takeMVar strings
  value <- H.lookup stringsTable s
  (count',res) <- case value of
            Just x -> return (stringsCount, x)
            Nothing -> H.insert stringsTable s stringsCount >> return (stringsCount+1, stringsCount)
  putMVar strings (count' :!: stringsTable)
  return res

-- Global store of unboxed events.
-- Since we cannot unbox Strings, these are represented as references to the
--  strings table
{-# NOINLINE events #-}
events :: Rope IO MVector EventSansId
events = unsafePerformIO $ do
  rope <- new' 10000  -- size of the lazy vectors internal to the rope structure
  return rope


{-# NOINLINE strings #-}
strings :: MVar(Pair Int (H.CuckooHashTable Text Int))
strings = unsafePerformIO $ do
  h <- H.newSized 100000  -- suggested capacity for the hash table
  newMVar (0 :!: h)
\end{code}


%************************************************************************
%*                                                                      *
\subsection{unique name supply code}
%*                                                                      *
%************************************************************************

Use the single threaded version

\begin{code}

initUniq :: IO ()
initUniq = writeIORef uniq 1

getUniq :: IO UID
getUniq = atomicModifyIORef' uniq (\n -> (n+1,n))

peepUniq :: IO UID
peepUniq = readIORef uniq

-- locals
{-# NOINLINE uniq #-}
uniq :: IORef UID
uniq = unsafePerformIO $ newIORef 1

\end{code}


%************************************************************************
%*                                                                      *
\subsection{GDM Generics}
%*                                                                      *
%************************************************************************

The generic implementation of the observer function.

\begin{code}
class Observable a where
        observer  :: a -> Parent -> a 
        default observer :: (Generic a, GObservable (Rep a)) => a -> Parent -> a
        observer x c = to (gdmobserver (from x) c)

        constrain :: a -> a -> a
        default constrain :: (Generic a, GConstrain (Rep a)) => a -> a -> a
        constrain x c = to (gconstrain (from x) (from c))

class GObservable f where
        gdmobserver :: f a -> Parent -> f a
        gdmObserveArgs :: f a -> ObserverM (f a)
        gdmShallowShow :: f a -> Text

constrainBase :: (Show a, Eq a) => a -> a -> a
constrainBase x c | x == c = x
                  | otherwise = error $ show x ++ " constrained by " ++ show c
\end{code}


A type generic definition of constrain

\begin{code}
class GConstrain f where gconstrain :: f a -> f a -> f a
instance (GConstrain a, GConstrain b) => GConstrain (a :+: b) where
  gconstrain (L1 x) (L1 c) = L1 (gconstrain x c)
  gconstrain (R1 x) (R1 c) = R1 (gconstrain x c)
instance (GConstrain a, GConstrain b) => GConstrain (a :*: b) where
  gconstrain (x :*: y) (c :*: d) = (gconstrain x c) :*: (gconstrain y d)
instance GConstrain U1 where
  gconstrain x c = x
instance (Observable a) => GConstrain (K1 i a) where
  gconstrain (K1 x) (K1 c) = K1 (constrain x c)
instance (GConstrain a) => GConstrain (M1 D d a) where
  gconstrain (M1 x) (M1 c) = M1 (gconstrain x c)
instance (GConstrain a, Selector s) => GConstrain (M1 S s a) where
  gconstrain m@(M1 x) n@(M1 c) | selName m ==  selName n = M1 (gconstrain x c)
instance (GConstrain a, Constructor c) => GConstrain (M1 C c a) where
  gconstrain m@(M1 x) n@(M1 c) | conName m == conName n = M1 (gconstrain x c)
\end{code}

Observing the children of Data types of kind *.

\begin{code}

-- Meta: data types
-- FieldLimit requires undecidable instances
instance (FieldLimit ('S ('S ('S ('S ('S ('S 'Z)))))) a, GObservable a) => GObservable (M1 D d a) where
 gdmobserver m@(M1 x) cxt = M1 (gdmobserver x cxt)
 gdmObserveArgs = gthunk
 gdmShallowShow = error "gdmShallowShow not defined on the <<data meta type>>"

-- Meta: Selectors
instance (GObservable a, Selector s) => GObservable (M1 S s a) where
 gdmobserver (M1 x) cxt = M1 (gdmobserver x cxt)
 gdmObserveArgs = gthunk
 gdmShallowShow = error "gdmShallowShow not defined on the <<selector meta type>>"

-- Meta: Constructors
instance (GObservable a, Constructor c) => GObservable (M1 C c a) where
 gdmobserver m1 = send (gdmShallowShow m1) (gdmObserveArgs m1)
 gdmObserveArgs (M1 x) = do {x' <- gdmObserveArgs x; return (M1 x')}
 gdmShallowShow = pack . conName

-- Unit: used for constructors without arguments
instance GObservable U1 where
 gdmobserver x _ = x
 gdmObserveArgs = return
 gdmShallowShow = error "gdmShallowShow not defined on <<the unit type>>"

-- Sums: encode choice between constructors
instance (GObservable a, GObservable b) => GObservable (a :+: b) where
 gdmobserver (L1 x) = send (gdmShallowShow x) (gdmObserveArgs $ L1 x)
 gdmobserver (R1 x) = send (gdmShallowShow x) (gdmObserveArgs $ R1 x)
 gdmShallowShow (L1 x) = gdmShallowShow x
 gdmShallowShow (R1 x) = gdmShallowShow x
 gdmObserveArgs (L1 x) = do {x' <- gdmObserveArgs x; return (L1 x')}
 gdmObserveArgs (R1 x) = do {x' <- gdmObserveArgs x; return (R1 x')}

-- Products: encode multiple arguments to constructors
instance (GObservable a, GObservable b) => GObservable (a :*: b) where
 gdmobserver (a :*: b) cxt = (gdmobserver a cxt) :*: (gdmobserver b cxt)
 gdmObserveArgs (a :*: b) = do 
   a'  <- gdmObserveArgs a
   b'  <- gdmObserveArgs b
   return (a' :*: b')
 gdmShallowShow = error "gdmShallowShow not defined on <<the product type>>"

-- Constants: additional parameters and recursion of kind *
instance (Observable a) => GObservable (K1 i a) where
 gdmobserver (K1 x) cxt = K1 $ observer x cxt
 gdmObserveArgs = gthunk
 gdmShallowShow = error "gdmShallowShow not defined on <<the constant type>>"

\end{code}

Observing functions is done via the ad-hoc mechanism, because
we provide an instance definition the default is ignored for
this type.

\begin{code}
instance (Observable a,Observable b) => Observable (a -> b) where
  observer fn cxt arg = gdmFunObserver cxt fn arg
  constrain = error "how to constrain the function type?"
\end{code}

Observing the children of Data types of kind *->*.

\begin{code}
gdmFunObserver :: (Observable a,Observable b) => Parent -> (a->b) -> (a->b)
gdmFunObserver cxt fn arg
        = sendObserveFnPacket
            (do arg' <- thunk observer arg
                thunk observer (fn arg')
            ) cxt
\end{code}

%************************************************************************
%*                                                                      *
\subsection{Instances}
%*                                                                      *
%************************************************************************

 The Haskell Base types

\begin{code}
instance Observable Int     where observer  = observeBase
                                  constrain = constrainBase
instance Observable Bool    where observer  = observeBase
                                  constrain = constrainBase
instance Observable Integer where observer  = observeBase
                                  constrain = constrainBase
instance Observable Float   where observer  = observeBase
                                  constrain = constrainBase
instance Observable Double  where observer  = observeBase
                                  constrain = constrainBase
instance Observable Char    where
  observer lit cxt = seq lit $ unsafeWithUniq $ \node -> do
    sendEvent node cxt (ConsChar lit)
    return lit
  constrain = constrainBase
instance Observable ()      where observer  = observeOpaque "()"
                                  constrain = constrainBase

-- utilities for base types.
-- The strictness (by using seq) is the same 
-- as the pattern matching done on other constructors.
-- we evalute to WHNF, and not further.

observeBase :: (Show a) => a -> Parent -> a
observeBase lit cxt = seq lit $ send (pack $ show lit) (return lit) cxt

observeOpaque :: Text -> a -> Parent -> a
observeOpaque str val cxt = seq val $ send str (return val) cxt
\end{code}

The Constructors.

\begin{code}
instance (Observable a,Observable b) => Observable (a,b) where
  observer (a,b) = send "," (return (,) << a << b)

instance (Observable a,Observable b,Observable c) => Observable (a,b,c) where
  observer (a,b,c) = send "," (return (,,) << a << b << c)

instance (Observable a,Observable b,Observable c,Observable d) 
          => Observable (a,b,c,d) where
  observer (a,b,c,d) = send "," (return (,,,) << a << b << c << d)

instance (Observable a,Observable b,Observable c,Observable d,Observable e) 
         => Observable (a,b,c,d,e) where
  observer (a,b,c,d,e) = send "," (return (,,,,) << a << b << c << d << e)

instance (Observable a) => Observable [a] where
  observer (a:as) = send ":"  (return (:) << a << as)
  observer []     = send "[]" (return [])

instance (Observable a) => Observable (Maybe a) where
  observer (Just a) = send "Just"    (return Just << a)
  observer Nothing  = send "Nothing" (return Nothing)

instance (Observable a,Observable b) => Observable (Either a b) where
  observer (Left a)  = send "Left"  (return Left  << a)
  observer (Prelude.Right a) = send "Right" (return Prelude.Right << a)
\end{code}

Arrays.

\begin{code}
instance (Ix a,Observable a,Observable b) => Observable (Array.Array a b) where
  observer arr = send "array" (return Array.array << Array.bounds arr 
                                                  << Array.assocs arr
                              )
  constrain = undefined
\end{code}

IO monad.

\begin{code}
instance (Observable a) => Observable (IO a) where
  observer fn cxt = 
        do res <- fn
           send "<IO>" (return return << res) cxt
  constrain = undefined
\end{code}



The Exception *datatype* (not exceptions themselves!).

\begin{code}
instance Observable SomeException where
  observer e = send ("<Exception> " <> pack(show e)) (return e)
  constrain = undefined

-- instance Observable ErrorCall where
--   observer (ErrorCall a)        = send "ErrorCall"   (return ErrorCall << a)


instance Observable Dynamic where
  observer = observeOpaque "<Dynamic>"
  constrain = undefined
\end{code}


%************************************************************************
%*                                                                      *
\subsection{Classes and Data Definitions}
%*                                                                      *
%************************************************************************


%************************************************************************
%*                                                                      *
\subsection{The ObserveM Monad}
%*                                                                      *
%************************************************************************

The Observer monad, a simple state monad, 
for placing numbers on sub-observations.

\begin{code}
newtype ObserverM a = ObserverM { runMO :: Int -> Word8 -> (a,Word8) }

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

thunk :: (a -> Parent -> a) -> a -> ObserverM a
thunk f a = ObserverM $ \ parent port ->
                ( observer_ f a (Parent
                                { parentUID = parent
                                , parentPosition   = port
                                }) 
                , port+1 )

gthunk :: (GObservable f) => f a -> ObserverM (f a)
gthunk a = ObserverM $ \ parent port ->
                ( gdmobserver_ a (Parent
                                { parentUID = parent
                                , parentPosition   = port
                                }) 
                , port+1 )

(<<) :: (Observable a) => ObserverM (a -> b) -> a -> ObserverM b
-- fn << a = do { fn' <- fn ; a' <- thunk a ; return (fn' a') }
fn << a = gdMapM (thunk observer) fn a
infixl 9 <<

gdMapM :: (Monad m)
        => (a -> m a)  -- f
        -> m (a -> b)  -- data constructor
        -> a           -- argument
        -> m b         -- data
gdMapM f c a = do { c' <- c ; a' <- f a ; return (c' a') }

\end{code}


%************************************************************************
%*                                                                      *
\subsection{observe and friends}
%*                                                                      *
%************************************************************************

Our principle function and class

\begin{code}
-- | 'observe' observes data structures in flight.
--  
-- An example of use is 
--  @
--    map (+1) . observe \"intermeduate\" . map (+2)
--  @
--
-- In this example, we observe the value that flows from the producer
-- @map (+2)@ to the consumer @map (+1)@.
-- 
-- 'observe' can also observe functions as well a structural values.
-- 
{-# NOINLINE gobserve #-}
gobserve :: (a->Parent->a) -> Text -> a -> (a,Int)
gobserve f name a = generateContext f name a

{- | 
Functions which you suspect of misbehaving are annotated with observe and
should have a cost centre set. The name of the function, the label of the cost
centre and the label given to observe need to be the same.

Consider the following function:

@triple x = x + x@

This function is annotated as follows:

> triple y = (observe "triple" (\x -> {# SCC "triple" #}  x + x)) y

To produce computation statements like:

@triple 3 = 6@

To observe a value its type needs to be of class Observable.
We provided instances for many types already.
If you have defined your own type, and want to observe a function
that takes a value of this type as argument or returns a value of this type,
an Observable instance can be derived as follows:

@  
  data MyType = MyNumber Int | MyName String deriving Generic

  instance Observable MyType
@
-}
{-# NOINLINE observe #-}
observe ::  (Observable a) => Text -> a -> a
observe lbl = fst . (gobserve observer lbl)

{- This gets called before observer, allowing us to mark
 - we are entering a, before we do case analysis on
 - our object.
 -}

{-# NOINLINE observer_ #-}
observer_ :: (a -> Parent -> a) -> a -> Parent -> a 
observer_ f a context = sendEnterPacket f a context

gdmobserver_ :: (GObservable f) => f a -> Parent -> f a
gdmobserver_ a context = gsendEnterPacket a context

\end{code}

The functions that output the data. All are dirty.

\begin{code}
unsafeWithUniq :: (Int -> IO a) -> a
unsafeWithUniq fn 
  = unsafePerformIO $ do { node <- getUniq
                         ; fn node
                         }
\end{code}

\begin{code}
generateContext :: (a->Parent->a) -> Text -> a -> (a,Int)
generateContext f {- tti -} label orig = unsafeWithUniq $ \node ->
     do sendEvent node (Parent 0 0) (Observe label)
        return (observer_ f orig (Parent
                      { parentUID      = node
                      , parentPosition = 0
                      })
               , node)

send :: Text -> ObserverM a -> Parent -> a
send consLabel fn context = unsafeWithUniq $ \ node ->
     do { let (r,portCount) = runMO fn node 0
        ; sendEvent node context (Cons portCount consLabel)
        ; return r
        }

sendEnterPacket :: (a -> Parent -> a) -> a -> Parent -> a
sendEnterPacket f r context = unsafeWithUniq $ \ node ->
     do { sendEvent node context Enter
        ; ourCatchAllIO (evaluate (f r context))
                        (handleExc context)
        }

gsendEnterPacket :: (GObservable f) => f a -> Parent -> f a
gsendEnterPacket r context = unsafeWithUniq $ \ node ->
     do { sendEvent node context Enter
        ; ourCatchAllIO (evaluate (gdmobserver r context))
                        (handleExc context)
        }

evaluate :: a -> IO a
evaluate a = a `seq` return a

sendObserveFnPacket :: ObserverM a -> Parent -> a
sendObserveFnPacket fn context
  = unsafeWithUniq $ \ node ->
     do { let (r,_) = runMO fn node 0
        ; sendEvent node context Fun
        ; return r
        }
\end{code}




%************************************************************************
%*                                                                      *
\subsection{Global, initualizers, etc}
%*                                                                      *
%************************************************************************

-- \begin{code}
-- openObserveGlobal :: IO ()
-- openObserveGlobal =
--      do { initUniq
--      ; startEventStream
--      }
-- 
-- closeObserveGlobal :: IO Trace
-- closeObserveGlobal =
--      do { evs <- endEventStream
--         ; putStrLn ""
--      ; return evs
--      }
-- \end{code}

%************************************************************************
%*                                                                      *
\subsection{Simulations}
%*                                                                      *
%************************************************************************

Here we provide stubs for the functionally that is not supported
by some compilers, and provide some combinators of various flavors.

\begin{code}
ourCatchAllIO :: IO a -> (SomeException -> IO a) -> IO a
ourCatchAllIO = Exception.catch

handleExc :: Parent -> SomeException -> IO a
-- handleExc context exc = return (send "throw" (return throw << exc) context)
handleExc context exc = return (send (pack $ show exc) (return (throw exc)) context)
\end{code}

%************************************************************************
