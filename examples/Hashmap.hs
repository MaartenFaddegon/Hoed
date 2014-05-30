{-# LANGUAGE TemplateHaskell, Rank2Types #-}
-- module Hashmap (emptyMap, add, lookup, Hashmap) where
import Prelude hiding (lookup,insert)
import Data.Maybe (fromJust)
import Debug.Hoed.Observe

data Hashmap a = Hashmap Int [(Maybe a)]
class Hashable a where hash :: a -> Int

emptyMap :: Int -> Hashmap a
emptyMap size = Hashmap size $ take size (repeat Nothing)

size :: Hashmap a -> Int
size (Hashmap s _) = s

(%) :: Int -> Hashmap a -> Int
idx % hashmap = idx `mod` (size hashmap)

(!!!) :: Hashmap a -> Int -> Maybe a
(!!!) (Hashmap _ elems) = (!!) elems

(///) :: Hashmap a -> (Int,Maybe a) -> Hashmap a
(Hashmap size elems) /// e = Hashmap size (elems // e)

(//) :: [a] -> (Int,a) -> [a]
xs // (idx,x) = take idx xs ++ x : (drop (idx + 1) xs)

add :: Hashable k => (k,v) -> Hashmap (k,v) -> Hashmap (k,v)
add (key,elem) hashmap = insert hashmap idx (key,elem)
      where idx = (hash key) `mod` (size hashmap)

insert :: Hashmap a -> Int -> a -> Hashmap a
insert hashmap idx x = case hashmap !!! idx of
      Nothing -> hashmap /// (idx,Just x)
      _       -> insert hashmap ((idx+1) % hashmap) x

lookup :: (Eq k, Hashable k) => k -> Hashmap (k,v) -> Maybe (k,v)
lookup key hashmap = fmap (\i -> fromJust $ hashmap !!! i) idx
      where idx = find hashmap ((hash key) % hashmap) key

find :: Eq k => Hashmap (k,v) -> Int -> k -> Maybe Int
find hashmap idx key = case hashmap !!! idx of
      Nothing            -> Nothing
      (Just (key',elem)) -> if key == key' 
                               then Just idx
                               else find hashmap ((idx+1) % hashmap) key

$(observedTypes "remove" [ [t| forall a . Hashmap a |]
                         , [t| forall a . Observable a => [] a |]
                         ]
 )

remove :: (Eq k, Hashable k) => k -> Hashmap (k,v) -> Hashmap (k,v)
remove key hashmap 
  = ( $(observe "remove") 
      (\hashmap -> 
        case find hashmap ((hash key) % hashmap) key of
          Nothing  -> hashmap
          Just idx -> hashmap /// (idx,Nothing)
      )  
    ) hashmap

--------------------------------------------------------------------------------

data Testkey = One | Two | Three deriving Eq
data Testval = Wennemars | Kramer | Verheijen deriving Eq

instance Hashable Testkey where
        hash One   = 10
        hash Two   = 20
        hash Three = 30

testmap :: Hashmap (Testkey,Testval)
testmap = emptyMap 5

test1 = ( (lookup Two) 
        . (add (One, Kramer)) 
        . (add (Two, Verheijen))
        $ testmap
        ) == Just (Two, Verheijen)

test2 = ( (lookup Two) 
        . (remove One)
        . (add (Two, Verheijen))
        . (add (One, Kramer))
        $ testmap
        ) == Just (Two, Verheijen)

main = runO $ print test2
