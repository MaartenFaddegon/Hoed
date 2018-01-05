{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiWayIf            #-}
-- This file is part of the Haskell debugger Hoed.
--
-- Copyright (c) Maarten Faddegon, 2015

{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists       #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Debug.Hoed.CompTree
( CompTree
, Vertex(..)
, mkCompTree
, isRootVertex
, vertexUID
, vertexRes
, replaceVertex
, getJudgement
, setJudgement
, isRight
, isWrong
, isUnassessed
, isAssisted
, isInconclusive
, isPassing
, leafs
, ConstantValue(..)
, unjudgedCharacterCount
#if defined(TRANSCRIPT)
, getTranscript
#endif
, TraceInfo(..)
, traceInfo
, Graph(..) -- re-export from LibGraph
)where
import           Control.Exception
import           Control.DeepSeq
import           Control.Monad
import           Control.Monad.ST
import           Debug.Hoed.EventForest
import           Debug.Hoed.Observe
import           Debug.Hoed.Render

import           Data.Bits
import qualified Data.Foldable          as F
import           Data.Graph.Libgraph
import           Data.IntMap.Strict     (IntMap)
import qualified Data.IntMap.Strict     as IntMap
import           Data.IntSet            (IntSet)
import qualified Data.IntSet            as IntSet
import           Data.List              (foldl', group, sort)
import           Data.List.NonEmpty     (NonEmpty, nonEmpty)
import           Data.Maybe
import           Data.Monoid            hiding ((<>))
import           Data.Semigroup
import           Data.Sequence          (Seq, ViewL (..), ViewR (..), viewl,
                                         viewr, (<|), (|>))
import qualified Data.Sequence          as S
import           Data.Set               (Set)
import qualified Data.Set               as Set
import           Data.Vector.Mutable as VM (STVector)
import qualified Data.Vector.Generic.Mutable as VM
import qualified Data.Vector.Unboxed    as U
import           GHC.Exts               (IsList (..))
import           GHC.Generics
import           Prelude                hiding (Right)
import           Text.Show.Functions

data Vertex = RootVertex | Vertex {vertexStmt :: CompStmt, vertexJmt :: Judgement}
  deriving (Eq,Show,Ord,Generic)

getJudgement :: Vertex -> Judgement
getJudgement RootVertex = Right
getJudgement v          = vertexJmt v

setJudgement :: Vertex -> Judgement -> Vertex
setJudgement RootVertex _ = RootVertex
setJudgement v          j = v{vertexJmt=j}

isRight :: Vertex -> Bool
isRight v      = getJudgement v == Right
isWrong :: Vertex -> Bool
isWrong v      = getJudgement v == Wrong
isUnassessed :: Vertex -> Bool
isUnassessed v = getJudgement v == Unassessed
isAssisted :: Vertex -> Bool
isAssisted v   = case getJudgement v of (Assisted _) -> True; _ -> False

isInconclusive :: Vertex -> Bool
isInconclusive v = case getJudgement v of
  (Assisted ms) -> any isInconclusive' ms
  _             -> False
isInconclusive' :: AssistedMessage -> Bool
isInconclusive' (InconclusiveProperty _) = True
isInconclusive' _                        = False

isPassing :: Vertex -> Bool
isPassing v = case getJudgement v of
  (Assisted ms) -> any isPassing' ms
  _             -> False
isPassing' :: AssistedMessage -> Bool
isPassing' (PassingProperty _) = True
isPassing' _                   = False

vertexUID :: Vertex -> UID
vertexUID RootVertex   = -1
vertexUID (Vertex s _) = stmtIdentifier s

vertexRes :: Vertex -> String
vertexRes RootVertex = "RootVertex"
vertexRes v          = stmtRes . vertexStmt $ v

-- | The forest of computation trees. Also see the Libgraph library.
type CompTree = Graph Vertex ()

isRootVertex :: Vertex -> Bool
isRootVertex RootVertex = True
isRootVertex _          = False

leafs :: CompTree -> [Vertex]
leafs g = filter (null . succs g) (vertices g)

-- | Approximates the complexity of a computation tree by summing the length
-- of the unjudged computation statements (i.e not Right or Wrong) in the tree.
unjudgedCharacterCount :: CompTree -> Int
unjudgedCharacterCount = sum . map characterCount . filter unjudged . vertices
  where characterCount = length . stmtLabel . vertexStmt

unjudged :: Vertex -> Bool
unjudged = not . judged
judged :: Vertex -> Bool
judged v = isRight v || isWrong v

replaceVertex :: CompTree -> Vertex -> CompTree
replaceVertex g v = mapGraph f g
  where f RootVertex = RootVertex
        f v' | vertexUID v' == vertexUID v = v
             | otherwise                       = v'

--------------------------------------------------------------------------------
-- Computation Tree

-- One computation statement can have multiple result-events. Sometime these add
-- new information, often the information is just a duplicate of what we already
-- know. With nub we remove the duplicates.

mkCompTree :: [CompStmt] -> [Dependency] -> CompTree
mkCompTree cs ds = Graph RootVertex vs as

  where vs = RootVertex : map (`Vertex` Unassessed) cs
        as = map (\(D i j) -> Arc (findVertex i) (findVertex j) ()) (snub ds)

        snub = map head . group . sort

        -- A mapping from stmtUID to Vertex of all CompStmts in cs
        vMap :: IntMap Vertex
        -- vMap = foldl (\m c -> let v = Vertex c Unassessed in foldl (\m' i -> IntMap.insert i v m') m (stmtUIDs c)) IntMap.empty cs
        vMap = foldl' (\m c -> IntMap.insert (stmtIdentifier c) (Vertex c Unassessed) m) IntMap.empty cs

        -- Given an UID, get corresponding CompStmt (wrapped in a Vertex)
        findVertex :: UID -> Vertex
        findVertex (-1) = RootVertex
        findVertex a = case IntMap.lookup a vMap of
          Nothing  -> error $ "mkCompTree: Error, cannot find a statement with UID " ++ show a ++ "!\n"
                              ++ "We recorded statements with the following UIDs: " ++ (show . IntMap.keys) vMap ++ "\n"
                              ++ unlines (map (\c -> (show . stmtIdentifier) c ++ ": " ++ show c) cs)
          (Just v) -> v

------------------------------------------------------------------------------------------------------------------------

data ConstantValue = ConstantValue { valStmt :: !UID, valLoc :: !Location
                                   , valMin  :: !UID, valMax :: !UID }
                   | CVRoot
                  deriving Eq

instance Show ConstantValue where
  show CVRoot = "Root"
  show v = "Stmt-" ++ (show . valStmt $ v)
         ++ "-"    ++ (show . valLoc  $ v)
         ++ ": "   ++ (show . valMin  $ v)
         ++ "-"    ++ (show . valMax  $ v)

------------------------------------------------------------------------------------------------------------------------

data EventDetails = EventDetails
  { topLvlFun   :: !(Maybe UID)
              -- ^ references from the UID of an event to the UID of the corresponding top-level Fun event
  , locations   :: ParentPosition -> Bool
              -- ^ reference from parent UID and position to location
  }
  deriving Show

type EventDetailsStore s = VM.STVector s EventDetails

getEventDetails :: EventDetailsStore s -> UID -> ST s EventDetails
getEventDetails = VM.unsafeRead

setEventDetails :: EventDetailsStore s -> UID -> EventDetails -> ST s ()
setEventDetails = VM.unsafeWrite

------------------------------------------------------------------------------------------------------------------------
data Dependency = D !UID !UID deriving (Eq, Generic, Ord, Show)

instance NFData Dependency

data TraceInfo = TraceInfo
  { computations :: !SpanZipper
                   -- UIDs of active and paused computations of arguments/results of Fun events
  , dependencies :: ![Dependency] -- the result
#if defined(TRANSCRIPT)
  , messages     :: !(IntMap String)
              -- ^ stored depth of the stack for every event
#endif
  }
  deriving Show

------------------------------------------------------------------------------------------------------------------------
addMessage :: Event -> String -> TraceInfo -> TraceInfo
#if defined(TRANSCRIPT)
addMessage e msg s = s{ messages = (flip $ IntMap.insert i) (messages s) $ case IntMap.lookup i (messages s) of
  Nothing     -> msg
  (Just msg') -> msg' ++ ", " ++ msg }

  where i = eventUID e


getMessage :: Event -> TraceInfo -> String
getMessage e s = case IntMap.lookup i (messages s) of
  Nothing    -> ""
  (Just msg) -> msg

  where i = eventUID e

getTranscript :: [Event] -> TraceInfo -> String
getTranscript es t = foldl (\acc e -> (show e ++ m e) ++ "\n" ++ acc) "" es

  where m e = case IntMap.lookup (eventUID e) ms of
          Nothing    -> ""
          (Just msg) -> "\n  " ++ msg

        ms = messages t
#else
addMessage _ _ t = t
#endif
------------------------------------------------------------------------------------------------------------------------

getLocation :: EventDetailsStore s -> Event -> ST s Bool
getLocation s e = do
          ed <- getEventDetails s (parentUID p)
          return $ locations ed (parentPosition p)
  where
    p = eventParent e

collectEventDetails :: EventDetailsStore s -> Event -> ST s (Bool,UID)
collectEventDetails v e = do
            let !p = eventParent e
            parentDetails <- getEventDetails v (parentUID p)
            let !loc = locations parentDetails (parentPosition p)
                !top = fromMaybe (parentUID p) $ topLvlFun parentDetails
            return (loc, top)

-- When we see a Fun event whose parent is not a Fun event it is a top level Fun event,
-- otherwise just copy the reference to the top level Fun event from the parent.
-- A top level Fun event references itself.
mkFunDetails :: EventDetailsStore s -> Event -> ST s EventDetails
mkFunDetails s e = do
    let p = eventParent e
    ed  <- getEventDetails s (parentUID p)
    let !loc = locations ed (parentPosition p)
        !top = fromMaybe (eventUID e) $ topLvlFun ed
        locFun 0 = not loc
        locFun 1 = loc
    return $ EventDetails (Just top) locFun
  where
    j = parentUID . eventParent $ e

------------------------------------------------------------------------------------------------------------------------

data Span = Computing !UID | Paused !UID deriving (Eq, Ord)

instance NFData Span where rnf = rwhnf

instance Show Span where
  show (Computing i) = show i
  show (Paused i)    = "(" ++ show i ++ ")"

getSpanUID :: Span -> UID
getSpanUID (Computing j) = j
getSpanUID (Paused j)    = j

data SpanList = SpanCons !UID !SpanList | SpanNil

instance IsList SpanList where
  type Item SpanList = Span
  toList SpanNil = []
  toList (SpanCons uid rest)
    | uid > 0 = Computing uid : toList rest
    | otherwise = Paused (negate uid) : toList rest
  fromList [] = SpanNil
  fromList (Paused uid : rest) = SpanCons (negate uid) $ fromList rest
  fromList (Computing uid : rest) = SpanCons uid $ fromList rest

instance Show SpanList where show = show . toList

data SpanZipper
  = SZ { left :: !SpanList
      ,  cursorUID :: !UID
      ,  right :: !SpanList}
  | SZNil

instance IsList SpanZipper where
  type Item SpanZipper = Span
  toList SZNil = []
  toList (SZ l uid r) = reverse(toList l) ++ toList (SpanCons uid r)

  fromList [] = SZNil
  fromList (Paused x : xx) = SZ [] (negate x) (fromList xx)
  fromList (Computing x : xx) = SZ [] x (fromList xx)

newtype Verbatim = Verbatim String
instance Show Verbatim where show (Verbatim s) = s

instance Show SpanZipper where
  show SZNil = "[]"
  show SZ {..} =
    show $
    map (Verbatim . show) (toList left) ++
    Verbatim ('\ESC':"[4m" ++ show cursorUID ++ '\ESC':"[24m") :
    map (Verbatim . show) (toList right)

emptySpanZipper = SZNil

startSpan :: UID -> SpanZipper -> SpanZipper
startSpan uid SZNil = SZ [] uid []
startSpan uid SZ{..}  = SZ [] uid (left <> SpanCons cursorUID right)
  where
    SpanNil <> x = x
    x <> SpanNil = x
    SpanCons uid rest <> x = rest <> SpanCons uid x

moveLeft, moveRight :: SpanZipper -> Maybe SpanZipper
moveLeft SZNil = Nothing
moveLeft SZ{left = SpanNil} = Nothing
moveLeft SZ{left = SpanCons uid l, ..} = Just $ SZ l uid (SpanCons cursorUID right)

moveRight SZNil = Nothing
moveRight SZ{right = SpanNil} = Nothing
moveRight SZ{right = SpanCons uid r, ..} = Just $ SZ (SpanCons cursorUID left) uid r

-- pauseSpan always moves to the right
pauseSpan uid sz
  | x  == uid = sz{cursorUID = negate uid}
  | otherwise = pauseSpan uid $ fromMaybe err $ moveRight sz{cursorUID = if x>0 then negate x else x}
  where
    x = cursorUID sz
    err = error $ unwords ["pauseSpan", show uid, show sz]

-- resumeSpan moves to the left, except when at the Top of the stack in which case it goes right
resumeSpan (negate -> uid) sz
  | cursorUID sz == uid = sz{cursorUID = negate uid}
  | SpanNil <- left sz, Just sz' <- moveRight sz = go moveRight sz'
  | Just sz' <- moveLeft sz = go moveLeft sz'
  | otherwise = err
  where
    err = error $ unwords ["resumeSpan", show (negate uid), show sz]
    go move sz
      | cursorUID sz == uid = sz{cursorUID = negate uid}
      | Nothing <- move sz = err
      | Just sz' <- move sz = go move sz'

-- stopSpan moves left
stopSpan :: UID -> SpanZipper -> SpanZipper
stopSpan uid sz@SZ{..}
  | uid == abs cursorUID = if
      | Just sz' <- moveRight sz -> sz'{left = left}
      | Just sz' <- moveLeft  sz -> sz'{right = right}
      | otherwise -> SZNil
  | Just sz' <- moveLeft sz = stopSpan uid sz'
  | otherwise = error $ unwords ["stopSpan", show uid, show sz]

----------------------------------------------------------------------------

start, stop,pause,resume :: Event -> EventDetails -> TraceInfo -> TraceInfo
start e ed s = m s{computations = cs}
  where Just i  = topLvlFun ed
        cs = startSpan i $ computations s
        m  = addMessage e $ "Start computation " ++ show i ++ ": " ++ show cs

stop e ed s = m s {computations = cs'}
  where
    Just i = topLvlFun ed
    cs' = stopSpan i (computations s)
    m = addMessage e $ "Stop computation " ++ show i ++ ": " ++ show cs'

pause e ed s = m s {computations = cs'}
  where
    Just i = topLvlFun ed
    cs' = pauseSpan i (computations s)
    m = addMessage e $ "Pause up to " ++ show i ++ ": " ++ show cs'

resume e ed s = m s {computations = cs'}
  where
    Just i = topLvlFun ed
    cs' = resumeSpan i (computations s)
    m = addMessage e $ "Resume computation " ++ show i ++ ": " ++ show cs'

activeComputations :: TraceInfo -> [UID]
activeComputations s = map getSpanUID . filter isActive . toList $ computations s
  where isActive (Computing _) = True
        isActive _             = False

------------------------------------------------------------------------------------------------------------------------

addDependency :: Event -> TraceInfo -> TraceInfo
addDependency _e s = m s{dependencies = case d of (Just d') -> d':dependencies s; Nothing -> dependencies s}

  where d = case activeComputations s of
              []      -> Nothing
              [n]     -> Just $ D (-1) n  -- top-level function detected (may later add dependency from Root)
              (n:m:_) -> Just $ D m n

        m = case d of
             Nothing   -> addMessage _e ("does not add dependency")
             (Just (D a b)) -> addMessage _e ("adds dependency " ++ show a ++ " -> " ++ show b)

------------------------------------------------------------------------------------------------------------------------

type ConsMap = U.Vector Word

--- Iff an event is a constant then the UID of its parent and its ParentPosition
--- are elements of the ConsMap.
mkConsMap :: Int -> Trace -> ConsMap
mkConsMap l t =
  U.create $ do
    v <- VM.replicate l 0
    forM_ t $ \e ->
      case change e of
        Cons {} -> do
          let p = eventParent e
          assert (parentPosition p < 64) $
            VM.unsafeModify v (`setBit` parentPosition p) (parentUID p - 1)
        _ -> return ()
    return v

corToCons :: ConsMap -> Event -> Bool
corToCons cm e = case U.unsafeIndex cm (parentUID p - 1) of
                   0 -> False
                   other -> testBit other (parentPosition p)
  where p = eventParent e

------------------------------------------------------------------------------------------------------------------------

traceInfo :: Int -> Trace -> TraceInfo
traceInfo l trc = runST $ do
  -- Practically speaking, event UIDs start in 1
  v <- VM.replicate (l+1) $ EventDetails Nothing (const False)
  let loop s e =
        case change e of
          Observe {} -> do
            setEventDetails v (eventUID e) (EventDetails Nothing (const True))
            return s
          Fun {} -> do
            setEventDetails v (eventUID e) =<< mkFunDetails v e
            return s
            -- Span start
          Enter {}
            | corToCons cs e -> do
              (loc, top) <- collectEventDetails v e
              let !details = EventDetails (Just top) (const loc)
              setEventDetails v (eventUID e) details
              return $ if loc
                  then addDependency e . start e details $ s
                  else pause e details s
            | otherwise -> return s
            -- Span end
          Cons {} -> do
            (loc, top) <- collectEventDetails v e
            let !details = EventDetails (Just top) (const loc)
            setEventDetails v (eventUID e) details
            return $ if loc
              then stop e details s
              else resume e details s
  foldM loop s0 trc
  where
    s0 :: TraceInfo
    s0 = TraceInfo [] []
#if defined(TRANSCRIPT)
           IntMap.empty
#endif
    cs :: ConsMap
    cs = mkConsMap l trc

