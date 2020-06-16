module SS2019 where

import Sort

splay :: [a] -> [[a]]
splay [] = []
splay (x:xs) = iterate id x : splay xs

-- example solution. repeat contructs an infinite list out of a value
splay' :: [a] -> [[a]]
splay' = map repeat

-- same as example solution. note the eta conversion in the function definition
zziipp :: Num a => [[a]] -> [[a]] -> [[a]]
zziipp = zipWith (zipWith (+))

distrib :: Num a => [a] -> [a] -> [[a]]
distrib x y = zziipp (splay x) (repeat y)

-- Correct solution - Note the list constructor in pattern matching has to be in braces
mergeAll :: Ord a => [[a]] -> [a]
mergeAll ((a:as):bs:rest) = a : mergeAll (merge as bs : rest)

-- Only works on a finite number of lists
mergeAll' :: Ord a => [[a]] -> [a]
mergeAll' = foldl (Sort.merge) []

-- Trivial with the other functions. distrib builds all possible element sums, mergeAll sorts and flattens
xadd :: (Ord a, Num a) => [a] -> [a] -> [a]
xadd a b = mergeAll $ distrib a b