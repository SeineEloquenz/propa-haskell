module Ropes where

data Rope
  = Leaf String
  | Inner Rope Int Rope
  
ropeLength :: Rope -> Int
ropeLength (Leaf s) = length s
ropeLength (Inner _ l r) = l + ropeLength r

ropeConcat :: Rope -> Rope -> Rope
ropeConcat l r = Inner l (ropeLength l) r