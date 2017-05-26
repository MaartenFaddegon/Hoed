{-# LANGUAGE DefaultSignatures, TypeOperators, FlexibleContexts, FlexibleInstances, StandaloneDeriving, CPP, DeriveGeneric #-}

import Debug.Hoed.Prop
import GHC.Generics hiding (moduleName)

data A = A                 deriving (Eq, Generic)
instance ParEq A
data B = C B | D | E A A A | F Int Int | G B B deriving (Eq, Generic)
instance ParEq B
data H = I B B B | J Int B deriving (Eq, Generic)
instance ParEq H

main = print $ all (==True) [t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,
                             s1,s2,s3,s4,s5,s6]

t1 = pareq_equiv A A
t2 = pareq_equiv D D
t3 = pareq_equiv (C D) D
t4 = pareq_equiv D     (C D)
t5 = pareq_equiv (C (C D)) D
t6 = pareq_equiv (E A A A) D
t7 = pareq_equiv e e where e = E A A A
t8 = pareq_equiv (C (C D)) (C (C (E A A A)))
t9 = pareq_equiv (C (C D)) (C (F 4 2))
t10 = pareq_equiv c c where c = (C (F 4 2))
t11 = pareq_equiv c c where c = (C (F 4 2))

s1 = ((G (error "oeps") D) === (G D (F 4 2))) == False
s2 = ((G D (F 4 2) === (G (error "oeps") D))) == False
s3 = ((G (error "oeps") D) `parEq` (G D D)) == Nothing
s4 = (I D (E A A A) D) === (I (error "oeps") D D) == False
s5 = (I D D (E A A A)) === (I D (error "oeps") D) == False
s6 = (I (E A A A) D D) === (I D D (error "oeps")) == False

-- pareq should be equivalent to normal equality when the
-- latter is conclusive
pareq_equiv x y = (x == y) == b
  where (Just b) = x `parEq` y
