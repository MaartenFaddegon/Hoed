{-# LANGUAGE DeriveGeneric #-}
module Behaviour(Trace(..),(+++),approx,prop_traceLessThan100Deep) where
import Debug.Hoed.Pure

data Trace a
  = Step (Trace a)
  | a :> Trace a
  | End
  | Crash
  deriving (Eq, Show,Generic)

instance Observable a => Observable (Trace a)

(+++) :: Trace a -> Trace a -> Trace a
Step s   +++ t = Step (s +++ t)
(x :> s) +++ t = x :> (s +++ t)
End      +++ t = t
Crash    +++ t = Crash

approx :: Eq a => Int -> Trace a -> Trace a -> Bool
approx 0 _        _        = True
approx n (a :> s) (b :> t) = a == b && approx (n-1) s t
approx n (Step s) (Step t) = approx (n-1) s t
approx n End    End        = True
approx n Crash  Crash      = True
approx n _        _        = False

prop_traceLessThan100Deep :: (Trace a) -> Bool
prop_traceLessThan100Deep = lessThanXDeep 100
  where lessThanXDeep 0 _          = False
        lessThanXDeep x (Step trc) = lessThanXDeep (x-1) trc
        lessThanXDeep x (_ :> trc) = lessThanXDeep (x-1) trc
        lessThanXDeep _ End        = True
        lessThanXDeep _ Crash      = True
