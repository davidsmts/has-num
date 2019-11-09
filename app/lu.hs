module LU
(

) where

import Vector
import Matrix

lu :: Matrix -> (Matrix, Matrix)
lu a = luz a 0

luz :: Matrix -> Int -> (Matrix, Matrix)
luz a i
    | i == length a -1 = (lk, rk : [])
    | otherwise     = (arrConn lk (fst (luz next (i+1))), rk : snd (luz next (i+1)))
    where 
        rk = r a i
        lk = l a i
        next = matrixSub a (matrixMult lk [rk])

r :: Matrix -> Int -> Vector
r (x:xs) i 
    | xs == []  = (oand1till x 0 i)
    | otherwise = (oand1till x 0 i)

oand1till :: Vector -> Int -> Int -> Vector
oand1till (x:xs) i till
    | xs == []  = if i <= till then (1 : []) else (x : [])
    | i < till = 0 : (oand1till xs (i+1) till)
    | i == till = x : (oand1till xs (i+1) till)
    | otherwise = x : (oand1till xs i till)

l :: Matrix -> Int -> Matrix
l a i
    | (length a)-1 == i = vecPlode (oand1till (head a) 0 i)
    | otherwise         = (vecPlode (oand1till (vectorDiv (column a i) ak) 0 i))
    where ak = (a !! i) !! i -- diagonal element




-- r :: Matrix -> Int -> Matrix
-- r (x:xs) i 
--     | xs == []  = (oand1till x 0 i) : []
--     | otherwise = (oand1till x 0 i) : (r xs (i+1))

-- oand1till :: Vector -> Int -> Int -> Vector
-- oand1till (x:xs) i till
--     | xs == []  = if i <= till then (1 : []) else (x : [])
--     | i < till = 0 : (oand1till xs (i+1) till)
--     | i == till = x : (oand1till xs (i+1) till)
--     | otherwise = x : (oand1till xs i till)

-- l :: Matrix -> Int -> Matrix
-- l a i
--     | (length a)-1 == i = vecPlode (oand1till (head a) 0 i)
--     | otherwise         = arrConn (vecPlode (oand1till (vectorDiv (column a i) ak) 0 i)) (l a (i+1))
--     where ak = (a !! i) !! i -- diagonal element

