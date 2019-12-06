module Cholesky (
    cholesky
) where

import Matrix

cholesky :: Num a => Matrix a -> Matrix a
cholesky a = cholesky_iterator a (length values a)

cholesky_iterator :: Num a => Matrix a -> Int -> Matrix a
cholesky_iterator (Matrix x) i
    | length x - 2 == i = cholesky_col (Matrix x) i : []
    | otherwise         = cholesky_col (Matrix x) i : cholesky_iterator (Matrix x) (i-1)

cholesky_col :: Floating a => Matrix a -> Int -> Matrix a
cholesky_col (Matrix x) i
    | i == 0    = column_ops 
    | i == lenx = 
    | otherwise = 
    where
        lenx = length x
        diag = (x !! i) !! i
        
-- todo umschreiben, sodass von vorne nach hinten abgearbeitet wird
column_ops :: Floating a => [a] -> [a] -> [a]
column_ops arr [] = []
column_ops arr (x:xs)
    | length xs < (length arr)-1    = (1/g11)*x : (column_ops arr xs)
    | otherwise                     = g11 : (column_ops arr xs)
    where
        g11 = sqrt (arr !! 0)