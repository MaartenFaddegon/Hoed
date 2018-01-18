{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists       #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

-- This file is part of the Haskell debugger Hoed.
--
-- Copyright (c) Maarten Faddegon, 2015


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
import           Control.DeepSeq
import           Control.Exception as E
import           Control.Monad
import           Debug.Hoed.EventForest
import           Debug.Hoed.Observe
import           Debug.Hoed.Span
import           Debug.Hoed.Render

import           Data.Bits
import qualified Data.Foldable          as F
import           Data.Graph.Libgraph
import qualified Data.Set               as Set
import           Data.IntMap.Strict     (IntMap)
import qualified Data.IntMap.Strict     as IntMap
import           Data.IntSet            (IntSet)
import           Data.List              (foldl', unfoldr)
import           Data.Maybe
import           Data.Semigroup
import           Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VM
import           Data.Vector.Mutable as VM (IOVector)
import qualified Data.Vector.Unboxed    as U
import           Data.Word
import           GHC.Exts               (IsList (..))
import           GHC.Generics
import           Prelude                hiding (Right)

data Vertex = RootVertex | Vertex {vertexStmt :: CompStmt, vertexJmt :: Judgement}
  deriving (Show,Ord,Generic)

instance Eq Vertex where
  RootVertex == RootVertex   = True
  v1@Vertex{} == v2@Vertex{} = vertexStmt v1 == vertexStmt v2
  _ == _ = False

instance NFData AssistedMessage
instance NFData Judgement
instance NFData Vertex
instance (NFData a, NFData b) => NFData (Arc a b)

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
vertexRes v          = unpack . stmtRes . vertexStmt $ v

-- | The forest of computation trees. Also see the Libgraph library.
type CompTree = Graph Vertex ()

isRootVertex :: Vertex -> Bool
isRootVertex RootVertex = True
isRootVertex _          = False

leafs :: CompTree -> [Vertex]
leafs g = filter (not . (`Set.member` nonLeafs)) (vertices g)
  where
    nonLeafs = Set.fromList [s | Arc s t _ <- arcs g]

-- | Approximates the complexity of a computation tree by summing the length
-- of the unjudged computation statements (i.e not Right or Wrong) in the tree.
unjudgedCharacterCount :: CompTree -> Int
unjudgedCharacterCount = sum . map characterCount . filter unjudged . vertices
  where characterCount = fromIntegral . T.length . stmtLabel . vertexStmt

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

mkCompTree :: [CompStmt] -> Dependencies -> CompTree
mkCompTree cs ds = Graph RootVertex vs as

  where vs = RootVertex : map (`Vertex` Unassessed) cs
        as = [Arc (findVertex i) (findVertex j) () | (i,jj) <- toList ds, j <- toList jj]

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

newtype TopLvlFun = TopLvlFun UID deriving Eq

noTopLvlFun :: TopLvlFun
noTopLvlFun = TopLvlFun (-1)

data EventDetails = EventDetails
  { topLvlFun_   :: !TopLvlFun
              -- ^ references from the UID of an event to the UID of the corresponding top-level Fun event
  , locations   :: ParentPosition -> Bool
              -- ^ reference from parent UID and position to location
  }

topLvlFun :: EventDetails -> UID
topLvlFun EventDetails{topLvlFun_ = TopLvlFun x} = x

type EventDetailsStore s = VM.IOVector EventDetails

getEventDetails :: EventDetailsStore s -> UID -> IO EventDetails
getEventDetails = VM.unsafeRead

setEventDetails :: EventDetailsStore s -> UID -> EventDetails -> IO ()
setEventDetails = VM.unsafeWrite


getTopLvlFunOr :: UID -> EventDetails -> UID
getTopLvlFunOr def EventDetails{topLvlFun_}
  | topLvlFun_ == noTopLvlFun = def
  | TopLvlFun x <- topLvlFun_ = x

------------------------------------------------------------------------------------------------------------------------

type Dependencies = IntMap IntSet

data TraceInfo = TraceInfo
  { computations :: !SpanZipper
                   -- UIDs of active and paused computations of arguments/results of Fun events
  , dependencies :: !Dependencies
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

collectEventDetails :: EventDetailsStore s -> Event -> IO (Bool,UID)
collectEventDetails v e = do
            let !p = eventParent e
            parentDetails <- getEventDetails v (parentUID p)
            let !loc = locations parentDetails (parentPosition p)
                !top = getTopLvlFunOr (parentUID p) parentDetails
            return (loc, top)

-- When we see a Fun event whose parent is not a Fun event it is a top level Fun event,
-- otherwise just copy the reference to the top level Fun event from the parent.
-- A top level Fun event references itself.
mkFunDetails :: EventDetailsStore s -> UID -> Event -> IO EventDetails
mkFunDetails s uid e = do
    let p = eventParent e
    ed  <- getEventDetails s (parentUID p)
    let !loc = locations ed (parentPosition p)
        !top = getTopLvlFunOr uid ed
        locFun 0 = not loc
        locFun 1 = loc
    return $ EventDetails (TopLvlFun top) locFun

----------------------------------------------------------------------------

start, stop,pause,resume :: Event -> EventDetails -> TraceInfo -> TraceInfo
start e ed s = m s{computations = cs}
  where i  = topLvlFun ed
        cs = startSpan i $ computations s
        m  = addMessage e $ "Start computation " ++ show i ++ ": " ++ show cs

stop e ed s = m s {computations = cs'}
  where
    i = topLvlFun ed
    cs' = stopSpan i (computations s)
    m = addMessage e $ "Stop computation " ++ show i ++ ": " ++ show cs'

pause e ed s = m s {computations = cs'}
  where
    i = topLvlFun ed
    cs' = pauseSpan i (computations s)
    m = addMessage e $ "Pause up to " ++ show i ++ ": " ++ show cs'

resume e ed s = m s {computations = cs'}
  where
    i = topLvlFun ed
    cs' = resumeSpan i (computations s)
    m = addMessage e $ "Resume computation " ++ show i ++ ": " ++ show cs'

activeComputations :: TraceInfo -> [UID]
activeComputations s = map getSpanUID . filter isActive . toList $ computations s
  where isActive (Computing _) = True
        isActive _             = False

------------------------------------------------------------------------------------------------------------------------

addDependency :: Event -> TraceInfo -> TraceInfo
addDependency _e s =
  m s{dependencies = case d of
         Just (from,to) -> IntMap.insertWith (<>) from [to] (dependencies s)
         Nothing -> dependencies s}

  where d = case activeComputations s of
              []      -> Nothing
              [n]     -> Just (-1, n)  -- top-level function detected (may later add dependency from Root)
              (n:m:_) -> Just (m, n)

        m = case d of
             Nothing       -> addMessage _e ("does not add dependency")
             (Just (a, b)) -> addMessage _e ("adds dependency " ++ show a ++ " -> " ++ show b)

------------------------------------------------------------------------------------------------------------------------

type ConsMap = U.Vector Word

--- Iff an event is a constant then the UID of its parent and its ParentPosition
--- are elements of the ConsMap.
mkConsMap :: Int -> Trace -> ConsMap
mkConsMap l t =
  U.create $ do
    v <- VM.replicate l 0
    VG.forM_ t $ \e ->
      when (isCons (change e)) $ do
          let p = eventParent e
#if __GLASGOW_HASKELL__ >= 800
          VM.unsafeModify v (`setBit` fromIntegral(parentPosition p)) (parentUID p)
#else
          let ix = parentUID p
          x <- VM.unsafeRead v ix
          VM.unsafeWrite v ix (x `setBit` parentPosition p)
#endif
    return v
  where
    isCons Cons{} = True
    isCons ConsChar{} = True
    isCons _ = False

corToCons :: ConsMap -> Event -> Bool
corToCons cm e = case U.unsafeIndex cm (parentUID p) of
                   0 -> False
                   other -> testBit other (fromIntegral $ parentPosition p)
  where p = eventParent e

------------------------------------------------------------------------------------------------------------------------

traceInfo :: Int -> Trace -> IO TraceInfo
traceInfo l trc = do
  -- Practically speaking, event UIDs start in 1
  v <- VM.replicate l $ EventDetails noTopLvlFun (const False)
  let loop !s uid e = do
        when (uid `mod` l100 == 0) $ putStr "."
        case (change e) of
          Observe {} -> do
            setEventDetails v uid (EventDetails noTopLvlFun (const True))
            return s
          Fun {} -> do
            setEventDetails v uid =<< mkFunDetails v uid e
            return s
            -- Span start
          Enter {}
            | corToCons cs e -> do
              (loc, top) <- collectEventDetails v e
              let !details = EventDetails (TopLvlFun top) (const loc)
              setEventDetails v uid details
              return $ if loc
                  then addDependency e . start e details $ s
                  else pause e details s
            | otherwise -> return s
            -- Span end (Cons or Char)
          other -> do
            (loc, top) <- collectEventDetails v e
            let !details = EventDetails (TopLvlFun top) (const loc)
            setEventDetails v uid details
            return $ if loc
              then stop e details s
              else resume e details s
  VG.ifoldM' loop s0 trc
  where
    l100 = l `div` 100
    s0 :: TraceInfo
    s0 = TraceInfo [] []
#if defined(TRANSCRIPT)
           IntMap.empty
#endif
    cs :: ConsMap
    cs = mkConsMap l trc

