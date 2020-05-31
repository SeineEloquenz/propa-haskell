module Collatz where

collatz :: Int -> [Int]
collatz x
  | x < 0 = error "x < 0"
  | x `mod` 2 == 0 = x : collatz (x `div` 2)
  | otherwise = x : collatz (3 * x + 1)

collatzIter :: Int -> [Int]
collatzIter x = iterate (\x -> if x `mod` 2 == 0 then x `div` 2 else 3*x+1) x

num :: Int -> Int
num m = length (takeWhile (\x -> x /= 1) (collatz m))

maxNum :: Int -> Int -> (Int, Int)
maxNum a b =  (swap . maximum . (map (\x -> (num x, x)))) [a..b]
  where swap (a,b) = (b,a)