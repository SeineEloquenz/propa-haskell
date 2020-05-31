module Arithmetik where

    pow1 :: Int -> Int -> Int
    pow1 b 0 = 1
    pow1 b e = b * pow1 b (e-1)
    
    pow2 :: Int -> Int -> Int
    pow2 b 0 = 1
    pow2 b e = if e `mod` 2 == 0 then pow2 (b*b) (e `div` 2) else b * pow2 (b*b) (e `div` 2)
    
    pow3 :: Int -> Int -> Int
    pow3 _ e | e < 0 = error("Negative exponent!")
    pow3 _ 0 = 1
    pow3 b e = powAcc b e 1 where
        powAcc :: Int -> Int -> Int -> Int
        powAcc _ 0 acc = acc
        powAcc b e acc = if e `mod` 2 == 0 then powAcc (b*b) (e `div` 2) acc else powAcc (b*b) (e `div` 2) b * acc
        
    isPrime :: Int -> Bool
    isPrime x
        | x < 1 = False
        | x == 2 = True
        | x >= 1 = sieve x 2 where
            sieve :: Int -> Int -> Bool
            sieve x d
                | x `mod` d == 0 = False
                | d > isqrt x = True
                | otherwise = sieve x (d+1)
                
    isqrt :: Int -> Int
    isqrt n = try n where
        try i   | i*i > n   = try (i - 1) 
                | i*i <= n  = i