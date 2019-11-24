module Matrix (
    -- transpose,
    -- matrixMult,
    -- unit,
    -- zero,
    -- xMatrix,
    -- Matrix,
    -- column,
    -- arrConn,
    -- vecPlode
) where

-- imports
import Vector

-- Matrix type
-- Row ordered
--
data Matrix a = Matrix {
    values :: [[a]]
    } deriving (Eq, Show, Ord)

instance (Ord a, Num a) => Num (Matrix a)
    where
        a + b = matrixAdd a b
        a - b = matrixSub a b
        -- (Matrix a) * (Matrix b) = Matrix $ matrixMult a b
        -- negate (Vector a)       = Vector $ zipWith (-) (zero (length a)) a
        -- abs (Vector a)          = Vector $ vecAbs a
        signum                  = undefined
        fromInteger             = undefined


-- matrixAdd :: (Ord a, Num a) => Matrix a -> Matrix a -> Matrix a
-- matrixAdd (Matrix (x:xs)) (Matrix(y:ys))
--     | length xs == 0  = Matrix $ values ((Vector x) + (Vector y)) : []
--     | otherwise = Matrix (values ((Vector x) + (Vector y)) : values (matrixAdd (Matrix xs) (Matrix ys)))

matrixAdd :: Num a => Matrix a -> Matrix a -> Matrix a
matrixAdd (Matrix (x:xs)) (Matrix(y:ys))
    | length xs == 0  = Matrix $ zipWith (+) x y : []
    | otherwise = Matrix $ zipWith (+) x y : values (matrixAdd (Matrix xs) (Matrix ys))
    
matrixSub :: Num a => Matrix a -> Matrix a -> Matrix a
matrixSub (Matrix (x:xs)) (Matrix(y:ys))
    | length xs == 0  = Matrix $ zipWith (-) x y : []
    | otherwise = Matrix $ zipWith (-) x y : values (matrixSub (Matrix xs) (Matrix ys))
        
        
    


-- Matrix creation

xMatrix :: Num a => Int -> Int -> a -> Matrix a
xMatrix rowDim colDim val
    | rowDim == 0   = Matrix $ []
    | otherwise     = Matrix $ (xVector colDim val) : values (xMatrix (rowDim-1) colDim val)

zero :: Num a => Int -> Int -> Matrix a
zero rowDim colDim = xMatrix rowDim colDim 0

unit :: (Eq a, Num a) => Int -> Matrix a
unit dim = unitCnt dim dim

unitCnt :: (Eq a, Num a) => Int -> Int -> Matrix a
unitCnt dim dimCnt
    | dimCnt-1 == 0   = Matrix (fillOneV 1 dimCnt dim : [])
    | otherwise     = Matrix $ (fillOneV 1 dimCnt dim) : values (unitCnt dim (dimCnt-1))


-- other
column :: Num a => Matrix a -> Int -> [a]
column (Matrix []) i = []
column (Matrix (x:xs)) i = x !! i : (column (Matrix xs) i)

--todo
-- upperTri :: Int -> [[Int]]
-- upperTri dim
--     | 
--     | otherwise = 


-- mathematical operations

-- transpose :: Matrix -> Matrix
-- transpose (x:xs)
--     | xs == []  = vecPlode x
--     | otherwise = arrConn (vecPlode x) (transpose xs)


-- vecPlode :: Vector -> Matrix
-- vecPlode (x:xs)
--     | xs == []  = [[x]]
--     | otherwise = [x] : (vecPlode xs)

-- arrConn :: Matrix -> Matrix -> Matrix
-- arrConn (x:xs) (y:ys)
--     | xs == []  = [x ++ y]
--     | otherwise = (x ++ y) : (arrConn xs ys)

-- row-ordered
-- matrixMult :: Matrix -> Matrix -> Matrix
-- matrixMult a b = matrixMultTransposed a (transpose b)


-- matrixSub :: Matrix -> Matrix -> Matrix
-- matrixSub (x:xs) (y:ys)
--     | xs == []  = vectorAdd x y : []
--     | otherwise = vectorAdd x y : matrixAdd xs ys
        

-- matrixMultTransposed  :: Matrix -> Matrix -> Matrix
-- matrixMultTransposed (x:xs) b
--     | xs == [] = rowByColumns x b : []
--     | otherwise = (rowByColumns x b) : (matrixMultTransposed xs b)


-- rowByColumns :: Vector -> Matrix -> Vector
-- rowByColumns a (x:xs)
--     | xs == [] = (rowByColumn a x) : []
--     | otherwise = (rowByColumn a x) : (rowByColumns a xs)

-- rowByColumn :: Vector -> Vector -> Int
-- rowByColumn [] [] = 0
-- rowByColumn (x:xs) (y:ys) = x*y + (rowByColumn xs ys)