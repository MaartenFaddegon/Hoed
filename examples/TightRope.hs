-- This is an example from the "Learn You A Haskell" tuturial.
-- The story is that there is a guy walking with a Pole where birds
-- can land on the left and right. If the difference between birds
-- on the left and right gets too big he falls off the rope. This
-- is indicated by Nothing.

import Debug.Hoed(runO,gdmobserve)

type Birds = Int
type Pole  = (Birds,Birds)

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
        where x + y = x Prelude.+ (abs y)

walk :: Maybe Pole
walk = gdmobserve "walk" $ {-# SCC "walk" #-}
        return (0,0) >>= landRight 1  >>= landLeft 1
          >>= landRight 2 >>= landRight (-1) >>= landRight 1

main = runO [] $ print walk
