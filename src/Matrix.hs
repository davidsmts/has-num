module Matrix (
    transpose,
    matrixMult,
    unit,
    zero,
    xMatrix,
    Matrix
) where

-- imports
import Vector

-- Matrix type
type Matrix = [[Int]]


-- Matrix creation

xMatrix :: Int -> Int -> Int -> Matrix
xMatrix rowDim colDim val
    | rowDim == 0   = []
    | otherwise     = (xVector colDim val) : (xMatrix (rowDim-1) colDim val)

zero :: Int -> Int -> Matrix
zero rowDim colDim = xMatrix rowDim colDim 0

unit :: Int -> Matrix
unit dim = unitCnt dim dim

unitCnt :: Int -> Int -> Matrix
unitCnt dim dimCnt
    | dimCnt-1 == 0   = fillOneV 1 dimCnt dim : []
    | otherwise     = (fillOneV 1 dimCnt dim) : (unitCnt dim (dimCnt-1))

--todo
-- upperTri :: Int -> [[Int]]
-- upperTri dim
--     | 
--     | otherwise = 


-- mathematical operations

transpose :: Matrix -> Matrix
transpose (x:xs)
    | xs == []  = vecPlode x
    | otherwise = arrConn (vecPlode x) (transpose xs)


vecPlode :: Vector -> Matrix
vecPlode (x:xs)
    | xs == []  = [[x]]
    | otherwise = [x] : (vecPlode xs)

arrConn :: Matrix -> Matrix -> Matrix
arrConn (x:xs) (y:ys)
    | xs == []  = [x ++ y]
    | otherwise = (x ++ y) : (arrConn xs ys)


matrixMult :: Matrix -> Matrix -> Matrix
matrixMult a b = matrixMultTransposed a (transpose b)


matrixMultTransposed  :: Matrix -> Matrix -> Matrix
matrixMultTransposed (x:xs) b
    | xs == [] = rowByColumns x b : []
    | otherwise = (rowByColumns x b) : (matrixMultTransposed xs b)


rowByColumns :: Vector -> Matrix -> Vector
rowByColumns a (x:xs)
    | xs == [] = (rowByColumn a x) : []
    | otherwise = (rowByColumn a x) : (rowByColumns a xs)

rowByColumn :: Vector -> Vector -> Int
rowByColumn [] [] = 0
rowByColumn (x:xs) (y:ys) = x*y + (rowByColumn xs ys)