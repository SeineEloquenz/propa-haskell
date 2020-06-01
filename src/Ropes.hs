module Ropes where

data Rope
  = Leaf String
  | Inner Rope Int Rope
  
ropeLength :: Rope -> Int
ropeLength (Leaf s) = length s
ropeLength (Inner _ l r) = l + ropeLength r

ropeConcat :: Rope -> Rope -> Rope
ropeConcat (Inner right inner left) rope = ropeConcat left rope
ropeConcat (Leaf s) rope = Inner (Leaf s) (ropeLength (Leaf s)) rope