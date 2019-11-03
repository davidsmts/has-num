module LU
(

) where

import Vector
import Matrix

-- lu :: [[Int]] -> ([[Int]], [[Int]])
-- lu (x:xs)
--     | xs == [] = ()

r :: Matrix -> Int -> Matrix
r (x:xs) i 
    | xs == []  = (oand1till x 0 i) : []
    | otherwise = (oand1till x 0 i) : (r xs (i+1))

oand1till :: Vector -> Int -> Int -> Vector
oand1till (x:xs) i till
    | xs == []  = if i <= till then (1 : []) else (x : [])
    | i < till = 0 : (oand1till xs (i+1) till)
    | i == till = x : (oand1till xs (i+1) till)
    | otherwise = x : (oand1till xs i till)

-- l :: Matrix -> 
-- l (x:xs) i 
--     | xs == []  = (oand1till x 0 i) : []
--     | otherwise = (oand1till x 0 i) : (l xs (i+1))

column :: Matrix -> Int -> Vector
column (x:xs) i
    | xs == []  = x !! i : []
    | otherwise = x !! i : (column xs i)


-- lu ::  [[Int]] -> ([[Int]], [[Int]])
-- lu (x:xs) = 
--     | xs == [] = (1,x)
--     | otherwise =
--         let ak = head x
--             bk = tail x
--             lk = each bk ak
--             Akp1 = 
--         in ([[]], [[]])


-- each :: [Int] -> Int -> [Int]
-- each (x:xs) p = 
--     | xs == [] = x/p : []
--     | otherwise = x/p : each xs 
 