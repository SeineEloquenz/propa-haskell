module ChurchNumbers where

type Church t = (t -> t) -> t -> t

int2church :: Integer -> Church t
int2church 0 = \s z -> z
int2church n = \s z -> iterate s z !! fromIntegral n

church2int :: Church Integer -> Integer
-- n = \s z -> s(s(...(z)))
church2int n = n (+1) 0

succ :: Church t -> Church t
succ n = \s -> s . n s