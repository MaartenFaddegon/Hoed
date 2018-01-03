{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
-- This file is part of the Haskell debugger Hoed.
--
-- Copyright (c) Maarten Faddegon, 2015

{-# LANGUAGE CPP           #-}
{-# LANGUAGE DeriveGeneric #-}

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
import           Control.Monad
import           Control.Monad.ST
import           Debug.Hoed.EventForest
import           Debug.Hoed.Observe
import           Debug.Hoed.Render

import           Data.Graph.Libgraph
import           Data.IntMap.Strict     (IntMap)
import qualified Data.IntMap.Strict     as IntMap
import           Data.List              (foldl', group, sort)
import           Data.Maybe
import qualified Data.Vector.Mutable as VM
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

-- Add element x to the head of the list at key k.
insertCon :: Int -> a -> IntMap [a] -> IntMap [a]
insertCon k x = IntMap.insertWith (\[x'] xs->x':xs) k [x] -- where x == x'

------------------------------------------------------------------------------------------------------------------------

data EventDetails = EventDetails
  { topLvlFun :: !(Maybe UID)
              -- ^ references from the UID of an event to the UID of the corresponding top-level Fun event
  , locations :: ParentPosition -> Bool
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
  { computations :: !Nesting
                   -- UIDs of active and paused computations of arguments/results of Fun events
  , dependencies :: ![Dependency] -- the result
#if defined(TRANSCRIPT)
  , messages  :: !(IntMap String)
              -- ^ stored depth of the stack for every event
#endif
  }
  deriving Show

------------------------------------------------------------------------------------------------------------------------
#if defined(TRANSCRIPT)
addMessage :: Event -> String -> TraceInfo -> TraceInfo
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

data Span = Computing !UID | Paused !UID
type Nesting = [Span]

instance Show Span where
  show (Computing i) = show i
  show (Paused i)    = "(" ++ show i ++ ")"

toPaused (Computing i) = Paused i
toPaused it@Paused{} = it

getSpanUID :: Span -> UID
getSpanUID (Computing j) = j
getSpanUID (Paused j)    = j

isSpan :: UID -> Span -> Bool
isSpan i s = i == getSpanUID s

start :: Event -> EventDetails -> TraceInfo -> TraceInfo
start e ed s = m s{computations = cs}

  where Just i  = topLvlFun ed
        cs = Computing i : computations s
#if defined(TRANSCRIPT)
        m  = addMessage e $ "Start computation " ++ show i ++ ": " ++ show cs
#else
        m = id
#endif


stop :: Event -> EventDetails -> TraceInfo -> TraceInfo
stop e ed s = m s{computations = cs}

  where Just i = topLvlFun ed
        cs = deleteFirst (computations s)
        deleteFirst [] = []
        deleteFirst (s:ss) | isSpan i s = ss
                           | otherwise  = s : deleteFirst ss
#if defined(TRANSCRIPT)
        m  = addMessage e $ "Stop computation " ++ show i ++ ": " ++ show cs
#else
        m = id
#endif


pause :: Event -> EventDetails -> TraceInfo -> TraceInfo
pause e ed s = m s{computations=cs}

  where Just i = topLvlFun ed
        cs = case cs_post of
               []      -> cs_pre
               (_:cs') -> map toPaused cs_pre ++ Paused i : cs'
        (cs_pre,cs_post)           = break isComputingI (computations s)
        isComputingI (Computing j) = i == j
        isComputingI _             = False
#if defined(TRANSCRIPT)
        m  = addMessage e $ "Pause computations up to " ++ show i ++ ": " ++ show cs
#else
        m = id
#endif

resume :: Event -> EventDetails -> TraceInfo -> TraceInfo
resume e ed s = m s{computations=cs}

  where Just i = topLvlFun ed
        cs = case cs_post of
               []      -> cs_pre
               (_:cs') -> cs_pre ++ Computing i : cs'
        (cs_pre,cs_post)     = break isPausedI (computations s)
        isPausedI (Paused j) = i == j
        isPausedI _          = False
#if defined(TRANSCRIPT)
        m = addMessage e $ "Resume computation " ++ show i ++ ": " ++ show cs
#else
        m = id
#endif

activeComputations :: TraceInfo -> [UID]
activeComputations s = map getSpanUID . filter isActive $ computations s
  where isActive (Computing _) = True
        isActive _             = False


------------------------------------------------------------------------------------------------------------------------

addDependency :: Event -> TraceInfo -> TraceInfo
addDependency _e s = m s{dependencies = case d of (Just d') -> d':dependencies s; Nothing -> dependencies s}

  where d = case activeComputations s of
              []      -> Nothing
              [n]     -> Just $ D (-1) n  -- top-level function detected (may later add dependency from Root)
              (n:m:_) -> Just $ D m n

#if defined(TRANSCRIPT)
        m = case d of
             Nothing   -> addMessage _e ("does not add dependency")
             (Just d') -> addMessage _e ("adds dependency " ++ show (fst d') ++ " -> " ++ show (snd d'))
#else
        m = id
#endif

------------------------------------------------------------------------------------------------------------------------

type ConsMap = IntMap [ParentPosition]

-- Iff an event is a constant then the UID of its parent and its ParentPosition
-- are elements of the ConsMap.
mkConsMap :: Trace -> ConsMap
mkConsMap = foldl' loop IntMap.empty
  where loop :: IntMap [ParentPosition] -> Event -> IntMap [ParentPosition]
        loop m e = case change e of
          Cons{} -> insertCon (parentUID . eventParent $ e) (parentPosition . eventParent $ e) m
          _      -> m

-- Return True for an enter event corresponding to a constant event and for any constant event, return False otherwise.
corToCons :: ConsMap -> Event -> Bool
corToCons cs e = case IntMap.lookup j cs of
                    Nothing   -> False
                    (Just ps) -> p `elem` ps
  where j = parentUID . eventParent $ e
        p = parentPosition . eventParent $ e

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
    cs = mkConsMap trc

