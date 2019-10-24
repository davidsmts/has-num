lu3 ::  [[Int]] -> ([[Int]], [[Int]])
lu3 (x:xs) = 
    | xs == [] = (1,x)
    | otherwise =
        let ak = head x
            bk = tail x
            lk = each bk ak
            Akp1 = 
        in ([[]], [[]])


each :: [Int] -> Int -> [Int]
each (x:xs) p = 
    | xs == [] = x/p : []
    | otherwise = x/p : each xs 
 