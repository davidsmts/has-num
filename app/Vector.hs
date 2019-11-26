module Vector (
    changeXtoY,
    changeItoY,
    xVector,
    zeroV,
    fillOneV,
    vectorDiv
) where


--
-- create vector type
--
-- data Vector a = Vector {
--     values :: [a]
--     } deriving (Eq, Show, Ord)

-- instance (Ord a, Num a) => Num (Vector a)
--     where
--         (Vector a) + (Vector b) = Vector $ zipWith (+) a b
--         (Vector a) - (Vector b) = Vector $ zipWith (-) a b
--         (Vector a) * (Vector b) = Vector $ zipWith (*) a b
--         -- negate (Vector a)       = Vector $ zipWith (-) (zero (length a)) a
--         abs (Vector a)          = Vector $ vecAbs a
--         signum                  = undefined
--         fromInteger             = undefined

--

--
-- vector creation
--

-- Create Vector of dimension: (dim) that only holds values: (val).
xVector :: Num a => Int -> a -> [a]
xVector dim val
    | dim == 0  = []
    | otherwise = val : xVector (dim - 1) val

-- Vector with zeros of dimension: dim
zeroV :: Num a => Int -> [a]
zeroV dim = xVector dim 0

-- todo
-- onesTill :: Int -> Int -> [Int]
-- onesTill dim a
--     | a == 0    =
--     | otherwise =

-- todo
-- onesFrom :: Int -> [Int]


--
-- Vector operations
--

-- returns a list with the positive version of the vector
vecAbs :: (Ord a,Num a) => [a] -> [a]
vecAbs [] = []
vecAbs (x:xs)
    | x < 0     = -x : vecAbs xs
    | otherwise = x : vecAbs xs

-- divides all values of an vector by a quotient
vectorDiv :: (Eq a, Num a, Fractional a) => [a] -> a -> [a]
vectorDiv (x:xs) quotient
    | xs == []  = [(x / quotient)]
    | otherwise = (x / quotient) : (vectorDiv xs quotient)
    

-- Changes all appearances of a certain number, to another number
changeXtoY :: (Eq a,Num a) => [a] -> a -> a -> [a]
changeXtoY (x:xs) ix y
    | xs == []  = if (x == ix)
                    then [y] else [x]
    | ix == x   = y : changeXtoY xs ix y
    | otherwise = x : changeXtoY xs ix y

-- todo put changeItoY and changeItoYunder into one function
-- Changes number at:i to value:y
-- parent function to changeItoYunder
changeItoY :: (Eq a, Num a) => [a] -> Int -> a -> [a]
changeItoY list i y = changeItoYunder list ((length list)-i-1) y

-- Changes number at:i to value:y with index
-- child function to changeItoY
changeItoYunder :: (Eq a, Num a) => [a] -> Int -> a -> [a]
changeItoYunder (x:xs) i y
    | xs == []          = if ((length xs) == i )
                            then [y] else [x]
    | (length xs) == i  =  y : changeItoYunder xs i y
    | otherwise         = x : changeItoYunder xs i y

-- todo fix reverse indexiation
-- todo decide on wether we want the vector type or not
-- Create vector where entry at index:y has the value of:x and dimension: dim
fillOneV :: (Eq a, Num a) => a -> Int -> Int -> [a]
fillOneV x y dim
    | dim == 0  = []
    | y == dim  = x : (fillOneV x y (dim-1))
    | otherwise = 0 : (fillOneV x y (dim-1))
