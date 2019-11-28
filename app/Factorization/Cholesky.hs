module Cholesky (

) where

import Matrix

-- cholesky :: Num a => Matrix a -> Matrix a
-- cholesky a
--     |
--     | otherwise = (column a 0) : tail (values a)

column_ops :: Floating a => [a] -> [a] -> [a]
column_ops arr [] = []
column_ops arr (x:xs)
    | length xs < (length arr)-1    = (1/g11)*x : (column_ops arr xs)
    | otherwise                     = g11 : (column_ops arr xs)
    where
        g11 = sqrt (arr !! 0)