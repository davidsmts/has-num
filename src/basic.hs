module Basics (
    transpose,
    matrixMult
) where

transpose :: [[Int]] -> [[Int]]
transpose (x:xs)
    | xs == []  = arrPlode x
    | otherwise = arrConn (arrPlode x) (transpose xs)


arrPlode :: [Int] -> [[Int]]
arrPlode (x:xs)
    | xs == []  = [[x]]
    | otherwise = [x] : (arrPlode xs)

arrConn :: [[Int]] -> [[Int]] -> [[Int]]
arrConn (x:xs) (y:ys)
    | xs == []  = [x ++ y]
    | otherwise = (x ++ y) : (arrConn xs ys)


matrixMult :: [[Int]] -> [[Int]] -> [[Int]]
matrixMult a b = matrixMultTransposed a (transpose b)


matrixMultTransposed  :: [[Int]] -> [[Int]] -> [[Int]]
matrixMultTransposed (x:xs) b
    | xs == [] = rowByColumns x b : []
    | otherwise = (rowByColumns x b) : (matrixMultTransposed xs b)


rowByColumns :: [Int] -> [[Int]] -> [Int]
rowByColumns a (x:xs)
    | xs == [] = (rowByColumn a x) : []
    | otherwise = (rowByColumn a x) : (rowByColumns a xs)

rowByColumn :: [Int] -> [Int] -> Int
rowByColumn [] [] = 0
rowByColumn (x:xs) (y:ys) = x*y + (rowByColumn xs ys)