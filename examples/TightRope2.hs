-- This is an example from the "Learn You A Haskell" tuturial.
-- The story is that there is a guy walking with a Pole where birds
-- can land on the left and right. If the difference between birds
-- on the left and right gets too big he falls off the rope. This
-- is indicated by Nothing.

import Debug.Hoed(runO,observe,observe',Identifier(..))

type Birds = Int
type Pole  = (Birds,Birds)

landLeft :: Birds -> Identifier -> Pole -> (Maybe Pole, Int)
landLeft n d p = let (f,i) = observe' "landLeft" d 
                                (\n' p' -> {-# SCC "landLeft" #-} landLeft' n' p')
                  in (f n p, i)
landLeft' n (left,right)
  | abs ((left + n) - right) < 4 = Just (left + n, right)
  | otherwise                    = Nothing

landRight :: Birds -> Identifier -> Pole -> (Maybe Pole, Int)
landRight n d p = let (f,i) = observe' "landRight" d 
                                 (\n' p' -> {-# SCC "landRight" #-} landRight' n' p')
                   in (f n p, i)
landRight' n (left,right)
  | abs (left - (right + n)) < 4 = Just (left, right + n)
  | otherwise                    = Nothing
        where x + y = x Prelude.+ (abs y)

walk :: Maybe Pole
walk = observe "walk" $ {-# SCC "walk" #-}
        return (0,0) *>>= landRight 1   >>== landLeft 1
          >>== landRight 2 >>== landRight (-1) >>=* landRight 1

main = runO [] $ print walk

--------------------------------------------------------------------------------
-- This used to be part of Observe, but implementing instance of TracedMonad
-- for IO and State are challenging.

class (Monad m) => TracedMonad m where
  (*>>=) :: Monad m => m a               -> (Identifier -> a -> (m b, Int)) -> (m b, Identifier)
  (>>==) :: Monad m => (m a, Identifier) -> (Identifier -> a -> (m b, Int)) -> (m b, Identifier)
  (>>=*) :: Monad m => (m a, Identifier) -> (Identifier -> a -> (m b, Int)) -> m b

instance TracedMonad Maybe where
  (Just x)    *>>= f = let (y,i) = f UnknownId x in (y,InSequenceAfter i)
  Nothing     *>>= f = (Nothing, UnknownId)

  (Just x, d) >>== f = let (y,i) = f d x in (y,InSequenceAfter i)
  (Nothing,_) >>== _ = (Nothing, UnknownId)

  (Just x,d)  >>=* f = fst (f d x)
  (Nothing,_) >>=* f = Nothing
