module Ropes where

data Rope
  = Leaf String
  | Inner Rope Int Rope
  
ropeLength :: Rope -> Int
ropeLength (Leaf s) = length s
ropeLength (Inner _ l r) = l + ropeLength r

ropeConcat :: Rope -> Rope -> Rope
ropeConcat l r = Inner l (ropeLength l) r

ropeSplitAt :: Int -> Rope -> (Rope, Rope)
ropeSplitAt i (Leaf s) = (Leaf (take i s), Leaf (drop i s))
ropeSplitAt i (Inner left len right)
  | i < len = let (leftleft, leftright) = ropeSplitAt i left in (leftleft, ropeConcat leftright right)
  | i > len = let (rightleft, rightright) = ropeSplitAt (i - len) right in (ropeConcat left rightleft, rightright)
  | otherwise = (left, right)
  
ropeInsert :: Int -> Rope -> Rope -> Rope
ropeInsert i a b = let (leftOfI, rightOfI) = ropeSplitAt i b in leftOfI `ropeConcat` a `ropeConcat` rightOfI 