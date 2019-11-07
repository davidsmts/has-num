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

l :: Matrix -> Int -> Matrix
l a i
    | (length a)-1 == i = vecPlode (oand1till (head a) 0 i)
    | otherwise         = arrConn (vecPlode (oand1till (vectorDiv (column a i) ak) 0 i)) (l a (i+1))
    where ak = (a !! i) !! i -- diagonal element

