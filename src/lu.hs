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


matrixMult :: [[Int]] -> [[Int]] -> [[Int]]
matrixMult (x:xs) (y:ys) = 
    | xs == [] && ys != [] = 
    | xs == [] && ys == [] = 
    | otherwise = 

rowByColumn :: [Int] -> [[Int]] -> Int
rowByColumn (x:xs) (y:ys) =
    | xs != [] and ys == [] = 
    | xs == [] and ys != [] = x*y : []
    | xs == [] && ys == [] = 
    | otherwise = 
