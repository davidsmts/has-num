module LU
(

) where

import Vector
import Matrix

-- lu :: [[Int]] -> ([[Int]], [[Int]])
-- lu (x:xs)
--     | xs == [] = ()

-- l :: [[Int]] -> Int -> Int -> [[Int]]
-- l (x:xs) i
--     | xs == []  = []
--     | otherwise = 

oand1till :: [Int] -> Int -> Int -> [Int]
oand1till (x:xs) i till
    | xs == []  = if i <= till then (1 : []) else (x : [])
    | i < till = 0 : (oand1till xs (i+1) till)
    | i == till = 1 : (oand1till xs (i+1) till)
    | otherwise = x : (oand1till xs i till) 
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
 