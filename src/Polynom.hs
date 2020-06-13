module Polynom where

type Polynom = [Double]

cmult :: Polynom -> Double -> Polynom
cmult poly c = map (*c) poly

eval :: Polynom -> Double -> Double
eval poly x = foldr ((+).(x*)) 0 poly
-- these are functionally the same. The latter one shows the horner scheme more clearly while the former is more concise
evalLambda :: Polynom -> Double -> Double
evalLambda poly x = foldr (\aN acc -> aN + (x * acc)) 0 poly