{-# LANGUAGE DeriveGeneric #-}
import Prelude hiding (lookup,insert)
import Data.Maybe (fromJust)
import Debug.Hoed.Observe

--------------------------------------------------------------------------------

data Hashmap a = Hashmap Int [(Maybe a)] deriving Generic
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

add :: (Observable k, Observable v, Hashable k)
    => (k,v) -> Hashmap (k,v) -> Hashmap (k,v)
add (key,elem) hashmap
  = ( -- gdmobserve "add"
      (\(key,elem) hashmap -> {-# SCC "add" #-}
        let idx = (hash key) `mod` (size hashmap)
        in insert hashmap idx (key,elem)
      )
    ) (key,elem) hashmap

insert :: Hashmap a -> Int -> a -> Hashmap a
insert hashmap idx x = case hashmap !!! idx of
      Nothing -> hashmap /// (idx,Just x)
      _       -> insert hashmap ((idx+1) % hashmap) x

lookup :: (Observable k, Observable v, Eq k, Hashable k) 
       => k -> Hashmap (k,v) -> Maybe (k,v)
lookup key hashmap
  = (gdmobserve "lookup"
      (\key hashmap -> {-# SCC "lookup" #-} 
        let idx = find hashmap ((hash key) % hashmap) key
        in fmap (\i -> fromJust $ hashmap !!! i) idx
      )
    ) key hashmap

find :: (Observable k, Observable v, Eq k) 
     => Hashmap (k,v) -> Int -> k -> Maybe Int
find hashmap idx key 
  = ( gdmobserve "find"
      (\hashmap idx key -> {-# SCC "find" #-} 
        case hashmap !!! idx of
          Nothing            -> Nothing
          (Just (key',elem)) -> if key == key' 
                                   then Just idx
                                   else find hashmap ((idx+1) % hashmap) key
      )
    ) hashmap idx key

remove :: (Eq k, Hashable k, Observable k, Observable v)
       => k -> Hashmap (k,v) -> Hashmap (k,v)
remove key hashmap 
  = ( gdmobserve "remove"
      (\key hashmap -> {-# SCC "remove" #-} 
        case find hashmap ((hash key) % hashmap) key of
          Nothing  -> hashmap
          Just idx -> hashmap /// (idx,Nothing)
      )  
    ) key hashmap

instance Observable a => Observable (Hashmap a)
instance Observable Testval
instance Observable Testkey

--------------------------------------------------------------------------------

data Testkey = One | Two | Three deriving (Eq, Generic)
data Testval = Wennemars | Kramer | Verheijen deriving (Eq, Generic)

main = runO $ print test

instance Hashable Testkey where
        hash One   = 10
        hash Two   = 20
        hash Three = 30

map1 :: Hashmap (Testkey,Testval)
map1 = ( (add (Two, Verheijen))
       . (add (One, Kramer))
       ) (emptyMap 5)


-- test = lookup Two map1 == lookup Two (remove One map1)
test = (==) (gdmobserve "testLeft" $ {-# SCC "testLeft" #-} lookup Two map1 )
            (gdmobserve "testRight" $ {-# SCC "testRight" #-} lookup Two (remove One map1))
