module Cholesky (
    -- cholesky
) where

import Matrix

-- cholesky :: Num a => Matrix a -> Matrix a
-- cholesky a = cholesky_iterator a (length values a)

-- cholesky_iterator :: Num a => Matrix a -> Int -> Matrix a
-- cholesky_iterator (Matrix x) i
--     | length x - 2 == i = cholesky_col (Matrix x) i : []
--     | otherwise         = cholesky_col (Matrix x) i : cholesky_iterator (Matrix x) (i-1)

-- cholesky_col :: Floating a => Matrix a -> Int -> Matrix a
-- cholesky_col (Matrix x) i
--     | i == 0    = column_ops 
--     | i == lenx = 
--     | i < lenx  = 
--     | otherwise = 
--     where
--         lenx = length x
--         diag = (x !! i) !! i

-- not iterating over rows (x:xs). But instead over columns (see "where")
cholesky_sum :: Floating a => Matrix a -> Int -> Int -> a
cholesky_sum (Matrix a) j i
    | length a-j-1 <= 0 = 0
    | otherwise         = (x_col !! i) * (x_col !! j) + (cholesky_sum xs_col i j)
    where 
        x_col = column (Matrix a) 0              -- to iterate over columns
        xs_col = withoutColumn (Matrix a) 0 0    -- also to iterate over columns

-- todo umschreiben, sodass von vorne nach hinten abgearbeitet wird
column_ops :: Floating a => [a] -> [a] -> [a]
column_ops arr [] = []
column_ops arr (x:xs)
    | length xs < (length arr)-1    = (1/g11)*x : (column_ops arr xs)
    | otherwise                     = g11 : (column_ops arr xs)
    where
        g11 = sqrt (arr !! 0)