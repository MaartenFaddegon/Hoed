{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE ViewPatterns #-}
module Debug.Hoed.Span
  ( Span(..)
  , getSpanUID
  , SpanZipper
  , startSpan
  , stopSpan
  , pauseSpan
  , resumeSpan
  , runSpanTests
  ) where

import           Control.Exception as E
import           Debug.Hoed.Observe

import           Data.List              (foldl', unfoldr)
import           Data.Maybe
import           GHC.Exts               (IsList (..))

import Debug.Trace
import Test.QuickCheck
import Test.QuickCheck.All

data Span = Computing !UID | Paused !UID deriving (Eq, Ord)

instance Show Span where
  show (Computing i) = show i
  show (Paused i)    = "(" ++ show i ++ ")"

getSpanUID :: Span -> UID
getSpanUID (Computing j) = j
getSpanUID (Paused j)    = j

data SpanList = SpanCons !UID !SpanList | SpanNil
  deriving Eq

instance IsList SpanList where
  type Item SpanList = Span
  toList = unfoldr f where
    f SpanNil = Nothing
    f (SpanCons uid rest)
      | uid > 0   = Just (Computing uid, rest)
      | otherwise = Just (Paused (negate uid), rest)
  fromList = foldr f SpanNil where
    f (Paused uid) = SpanCons (negate uid)
    f (Computing uid) = SpanCons uid

instance Show SpanList where show = show . toList

data SpanZipper
  = SZ { left :: !SpanList
      ,  cursorUID :: !UID
      ,  right :: !SpanList}
  | SZNil
  deriving Eq

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

-- pauseSpan always moves to the right, except when at the bottom of the stack in which case it goes left
pauseSpan :: UID -> SpanZipper -> SpanZipper
pauseSpan uid sz
  | SZNil <- sz = sz
  | x == uid = sz {cursorUID = negate uid}
  | SpanNil <- right sz
  , SpanCons a aa <- fromListWithReverse $ toList (SpanCons x (left sz)) -- this should fuse!
   = go (SZ [] a aa)
  | Just sz' <- moveRight sz {cursorUID = negative x} = pauseSpan uid sz'
  | otherwise = assert notLeft sz
  where
    x = cursorUID sz
    negative x =
      if x < 0
        then x
        else negate x
    fromListWithReverse = foldl f SpanNil
      where
        f rest (Paused uid) = SpanCons (negate uid) rest
        f rest (Computing uid) = SpanCons uid rest
    notLeft =
      (Computing uid `notElem` toList (left sz)) ||
      error (unwords ["pauseSpan", show uid, show sz])
    go sz
      | cursorUID sz == uid = sz {cursorUID = negate uid}
      | Just sz' <- moveRight sz {cursorUID = negative (cursorUID sz)} = go sz'
      | otherwise = sz

-- resumeSpan moves to the left, except when at the Top of the stack in which case it goes right
resumeSpan :: UID -> SpanZipper -> SpanZipper
resumeSpan (negate -> uid) sz
  | SZNil <- sz = sz
  | cursorUID sz == uid = sz{cursorUID = negate uid}
  | SpanNil <- left sz, Just sz' <- moveRight sz = go moveRight sz'
  | Just sz' <- moveLeft sz = go moveLeft sz'
  | otherwise = assert (Computing uid `notElem` toList (right sz)) sz
  where
    go move sz
      | cursorUID sz == uid = sz{cursorUID = negate uid}
      | Just sz' <- move sz = go move sz'
      | otherwise = sz

-- stopSpan moves left
stopSpan :: UID -> SpanZipper -> SpanZipper
stopSpan uid sz@SZ{..}
  | uid == abs cursorUID = if
      | Just sz' <- moveRight sz -> sz'{left = left}
      | Just sz' <- moveLeft  sz -> sz'{right = right}
      | otherwise -> SZNil
  | Just sz' <- moveLeft sz = stopSpan uid sz'
stopSpan uid sz = error $ unwords ["stopSpan", show uid, show sz]


---------------------------------------------------------
-- Properties
instance Arbitrary Span where
  arbitrary = do
    computing <- arbitrary
    uid <- arbitrary
    return $ if computing then Computing (abs uid + 1) else Paused (abs uid + 1)

instance Arbitrary SpanList where
  arbitrary = fromList <$> arbitrary
  shrink [] = []
  shrink (SpanCons a rest) = [SpanNil, rest] ++ [SpanCons a l' | l' <- shrink rest]

instance Arbitrary SpanZipper where
  arbitrary = oneof [pure SZNil, SZ <$> arbitrary <*> arbitrary <*> arbitrary]
  shrink SZNil = []
  shrink (SZ l x r) = [SZ l' x r' | (l',r') <- shrink (l,r)]

prop_SpanList1 :: [Span] -> Bool
prop_SpanList1 xx = toList(fromList xx :: SpanList) == xx
prop_SpanList2 :: SpanList -> Bool
prop_SpanList2 xx = fromList(toList xx) == xx

prop_SpanZipper1 :: [Span] -> Bool
prop_SpanZipper1 xx = toList(fromList xx :: SpanZipper) == xx
prop_SpanZipper2 :: SpanZipper -> Bool
prop_SpanZipper2 xx = toList(fromList @SpanZipper (toList xx)) == toList xx

prop_LR, prop_RL :: SpanZipper -> Property
prop_LR x = isJust(moveRight x) ==> (moveLeft  =<< moveRight x) == Just x
prop_RL x = isJust(moveLeft  x) ==> (moveRight =<< moveLeft  x) == Just x

return []
runSpanTests = $quickCheckAll
