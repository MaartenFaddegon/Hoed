{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS -fdefer-type-errors #-}
import Debug.Hoed

-- Should build
data T64 = T64 { t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,t16,t17,t18,t19,t20,t21,t22,t23,t24,t25,t26,t27,t28,t29,t30
               , t31,t32,t33,t34,t35,t36,t37,t38,t39,t40,t41,t42,t43,t44,t45,t46,t47,t48,t49,t50,t51,t52,t53,t54,t55,t56,t57,t58
               , t59,t60,t61,t62,t63,t64 :: ()}
           deriving Generic

instance Observable T64

data F65 = F65 { f1,f2,f3,f4,f5,f6,f7,f8,f9,f10,f11,f12,f13,f14,f15,f16,f17,f18,f19,f20,f21,f22,f23,f24,f25,f26,f27,f28,f29,f30
               , f31,f32,f33,f34,f35,f36,f37,f38,f39,f40,f41,f42,f43,f44,f45,f46,f47,f48,f49,f50,f51,f52,f53,f54,f55,f56,f57,f58
               , f59,f60,f61,f62,f63,f64,f65 :: ()}
           deriving (Generic, Show)

-- Should type erro with "Hoed handles constructors with 64 fields or less"
instance Observable F65
