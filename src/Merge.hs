module Merge where
import Sort

-- merge in Sort.hs module

primepowers :: Integer -> [Integer]
primepowers 1 = primes
primepowers i = merge (pp i) (primepowers (i-1))
  where pp i = map (\x -> x ^ i) primes

primes :: [Integer]
primes = 2 : oddPrimes (tail odds)
  where oddPrimes (p : ps) = p : (oddPrimes [p_ | p_ <- ps, p_ `mod` p /= 0])
        odds = iterate (+2) 1