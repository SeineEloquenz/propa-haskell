module Sort where
    
    insert :: (Ord a) => a -> [a] -> [a]
    insert a [] = [a]
    insert a (x:xs) = if a >= x then x:insert a xs else a:x:xs
    
    insertSort :: (Ord a) => [a] -> [a]
    insertSort [] = []
    insertSort (x:xs) = insert x (insertSort xs)
    
    merge :: (Ord a) => [a] -> [a] -> [a]
    merge [] a = a
    merge a [] = a
    merge (x:xs) (y:ys) = if x < y then x:merge xs (y:ys) else y:merge (x:xs) ys
    
    mergeSort :: (Ord a) => [a] -> [a]
    mergeSort [] = []
    mergeSort [x] = [x]
    mergeSort l = merge (mergeSort first) (mergeSort last)
        where
            first = take ((length l) `div` 2) l
            last = drop ((length l) `div` 2) l