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


-- vandermonde :: Num a => [a] -> Matrix a
-- vandermonde coeffs
--     | coeffs != null =
--     | otherwise     = Matrix $ values (vecPlode coeff) : vandermonde

vandermonize :: Num a => [a] -> [a]
vandermonize coeffs
    | length coeffs == 1 = [1]
    | otherwise = (vandermonize coeffs_s) ++ [coefficient ** ((length coeffs)-1)]
    where coefficient = coeffs !! 0
          coeffs_s = take ((length coeffs)-1) coeffs

-- returns column i
column :: Num a => Matrix a -> Int -> [a]
column (Matrix []) i = []
column (Matrix (x:xs)) i = x !! i : (column (Matrix xs) i)

withoutColumn :: Num a => Matrix a -> Int -> Matrix a
withoutColumn a i = transpose (withoutColumnCalc a i 0)

-- only a wrapper for without column
-- returns transposed !!!
-- returns all Matrix without column i
-- iteration counter needed (second number passed in)
withoutColumnCalc :: Num a => Matrix a -> Int -> Int -> Matrix a
withoutColumnCalc a i cnt
    | length ((values a) !! 0) == cnt = (Matrix [])
    | cnt == i  = withoutColumnCalc a i (cnt+1)
    | otherwise = Matrix ((column a cnt) : values (withoutColumnCalc a i (cnt+1)))

--todo
-- upperTri :: Int -> [[Int]]
-- upperTri dim
--     |
--     | otherwise =

--todo find good way to iterate two dimensionally
-- cuts a matrix into a Upper triangle matrix
-- cut0s_U :: Num a => Matrix a -> Matrix a
-- cut0s_U (Matrix a) = do
--     (x, row) <- enumerate a
--     (y, cell) <- enumerate row
--     if length row <= (length cell) / 2 then

--todo column form of matrix
--todo row form of (column-form) matrix
--todo put matrix into one dimensional list and an integer telling the size

-- Sums up all the rows and returns a list, in which each number represents a sum
rowSums :: Num a => Matrix a -> [a]
rowSums (Matrix []) = []
rowSums (Matrix (x:xs)) = sum x : rowSums (Matrix xs)

--todo column sums
-- Sums up all the columns and returns a list, in which each number represents a sum
columnSums :: Num a => Matrix a -> [a]
columnSums (Matrix []) = []
columnSums matrix = sum (column matrix 0) : columnSums (withoutColumn matrix 0)


-- mathematical operations

transpose :: Num a => Matrix a -> Matrix a
transpose (Matrix []) = (Matrix [])
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
