-- This is an example from the "Learn You A Haskell" tuturial.
-- The story is that there is a guy walking with a Pole where birds
-- can land on the left and right. If the difference between birds
-- on the left and right gets too big he falls off the rope. This
-- is indicated by Nothing.

import Debug.Hoed(runO,gdmobserve,gdmobserve',Identifier(..))

-- instance Monad Maybe where
--   return        = Just
--   Nothing >>= f = Nothing
--   Just x  >>= f = f x
--   fail _        = Nothing

type Birds = Int
type Pole  = (Birds,Birds)

----------------------------------------------------------------------

landLeft :: Birds -> Pole -> Maybe Pole
landLeft n p = gdmobserve "landLeft" (\n' p' -> {-# SCC "landLeft" #-} landLeft' n' p') n p
landLeft' n (left,right)
  | abs ((left + n) - right) < 4 = Just (left + n, right)
  | otherwise                    = Nothing

landRight :: Birds -> Pole -> Maybe Pole
landRight n p = gdmobserve "landRight" (\n' p' -> {-# SCC "landRight" #-} landRight' n' p') n p
landRight' n (left,right)
  | abs (left - (right + n)) < 4 = Just (left, right + n)
  | otherwise                    = Nothing

----------------------------------------------------------------------

landLeft3 :: Identifier -> Birds -> Pole -> (Maybe Pole, Int)
landLeft3 d n p = let (f,i) = gdmobserve' "landLeft3" d 
                                (\n' p' -> {-# SCC "landLeft3" #-} landLeft3' n' p')
                  in (f n p, i)
landLeft3' n (left,right)
  | abs ((left + n) - right) < 4 = Just (left + n, right)
  | otherwise                    = Nothing

landRight3 :: Identifier -> Birds -> Pole -> (Maybe Pole, Int)
landRight3 d n p = let (f,i) = gdmobserve' "landRight3" d 
                                 (\n' p' -> {-# SCC "landRight3" #-} landRight3' n' p')
                   in (f n p, i)
landRight3' n (left,right)
  | abs (left - (right + n)) < 4 = Just (left, right + n)
  | otherwise                    = Nothing
        where x + y = x Prelude.+ (abs y)


----------------------------------------------------------------------

walk1 :: Maybe Pole
walk1 = gdmobserve "walk1" $ {-# SCC "walk1" #-}
       return (0,0) >>= landLeft 1 >>= landRight 4 >>= landLeft (-1)

-- -- A tracing bind does not help because landLeft and landRight are not
-- -- applied in the lexical scope of bind.
-- walk2 :: Maybe Pole
-- walk2 = gdmobserve "walk2" $ {-# SCC "walk2" #-}
--         let x0      = return (0,0)
--             (x1,d1) = tracedBind UnknownId x0 $ landLeft  1
--             (x2,d2) = tracedBind d1        x1 $ landRight 4
--         in  fst     $ tracedBind d2        x2 $ landLeft  (-1)

walk3 :: Maybe Pole
walk3 = gdmobserve "walk3" $ {-# SCC "walk3" #-}
        return (0,0) *>>= flip landRight3 1   >>== flip landLeft3 1
          >>== flip landRight3 2 >>== flip landRight3 (-1) >>=* flip landRight3 1

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

main = runO [] $ print walk3
