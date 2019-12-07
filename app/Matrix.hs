module Matrix (
    transpose,
    matrixMult,
    unit,
    zero,
    xMatrix,
    Matrix(..),
    column,
    vecPlode,
    withoutColumn
) where

-- imports
import Vector

-- Matrix type
-- Row ordered
--
data Matrix a = Matrix {
    values :: [[a]]
    } deriving (Eq, Ord)

instance (Ord a, Num a) => Num (Matrix a)
    where
        a + b = matrixAdd a b
        a - b = matrixSub a b
        a * b = matrixMult a b
        -- negate (Vector a)       = Vector $ zipWith (-) (zero (length a)) a
        -- abs (Vector a)          = Vector $ vecAbs a
        signum                  = undefined
        fromInteger             = undefined


instance (Num a, Show a) => Show (Matrix a) where
    show (Matrix (x:xs))
        | length xs == 0    = show x
        | otherwise         = show x ++ "\n" ++ show (Matrix xs)

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


-- returns column i
column :: Num a => Matrix a -> Int -> [a]
column (Matrix []) i = []
column (Matrix (x:xs)) i = x !! i : (column (Matrix xs) i)

-- returns all Matrix without column i
-- iteration counter needed
withoutColumn :: Num a => Matrix a -> Int -> Int -> Matrix a
withoutColumn a i cnt
    | length (values a) == cnt = (Matrix [])
    | cnt == i  = withoutColumn a i (cnt+1)
    | otherwise = Matrix ((column a cnt) : values (withoutColumn a i (cnt+1)))

--todo
-- upperTri :: Int -> [[Int]]
-- upperTri dim
--     | 
--     | otherwise = 


-- mathematical operations

transpose :: Num a => Matrix a -> Matrix a
transpose (Matrix (x:xs))
    | length xs == 0  = vecPlode x
    | otherwise = matrixConn (vecPlode x) (transpose (Matrix xs))


vecPlode :: Num a => [a] -> Matrix a
vecPlode (x:xs)
    | length xs == 0  = Matrix $ [[x]]
    | otherwise = Matrix $ [x] : (values (vecPlode xs))

matrixConn :: Num a => Matrix a -> Matrix a -> Matrix a
matrixConn (Matrix (x:xs)) (Matrix (y:ys))
    | length xs == 0  = Matrix $ [x ++ y]
    | otherwise = Matrix $ (x ++ y) : values (matrixConn (Matrix xs) (Matrix ys))

-- row-ordered Matrix Multiplication
matrixMult :: Num a => Matrix a -> Matrix a -> Matrix a
matrixMult a b = matrixMultTransposed a (transpose b)

matrixMultTransposed  :: Num a => Matrix a -> Matrix a -> Matrix a
matrixMultTransposed (Matrix (x:xs)) b
    | length xs == 0 = Matrix $ (rowByColumns x b) : []
    | otherwise = Matrix $ (rowByColumns x b) : values (matrixMultTransposed (Matrix xs) b)
     

rowByColumns :: Num a => [a] -> Matrix a -> [a]
rowByColumns a (Matrix (x:xs))
    | length xs == 0 = (rowByColumn a x) : []
    | otherwise = (rowByColumn a x) : (rowByColumns a (Matrix xs))

rowByColumn :: Num a => [a] -> [a] -> a
rowByColumn (x:xs) (y:ys)
    | length xs == 0 = x * y
    | otherwise = x*y + (rowByColumn xs ys)