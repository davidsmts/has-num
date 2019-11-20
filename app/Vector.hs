module Vector (
    changeXtoY,
    changeItoY,
    xVector,
    zeroV,
    fillOneV,
    Vector,
    vectorDiv
) where


--
-- create vector type
--
data Vector a = Vector {
    values :: [a]
    } deriving (Eq, Show, Ord)

instance (Ord a, Num a) => Num (Vector a)
    where
        (Vector a) + (Vector b) = Vector $ zipWith (+) a b
        (Vector a) - (Vector b) = Vector $ zipWith (-) a b
        (Vector a) * (Vector b) = Vector $ zipWith (*) a b
        -- negate (Vector a)       = Vector $ zipWith (-) (zero (length a)) a
        abs (Vector a)          = Vector $ vecAbs a
        signum                  = undefined
        fromInteger             = undefined

--

--
-- vector creation
--

-- Create Vector of dimension: (dim) that only holds values: (val).
xVector :: Num a => Int -> a -> Vector a
xVector dim val
    | dim == 0  = (Vector [])
    | otherwise = Vector $ (val : values (xVector (dim - 1) val))

-- Vector with zeros of dimension: dim
zeroV :: Num a => Int -> Vector a
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
vectorDiv :: (Eq a, Num a, Fractional a) => Vector a -> a -> Vector a
vectorDiv (Vector (x:xs)) quotient
    | xs == []  = Vector $ [(x / quotient)]
    | otherwise = Vector $ (x / quotient) : values (vectorDiv (Vector xs) quotient)


-- Changes all appearances of a certain number, to another number
changeXtoY :: (Eq a,Num a) => Vector a -> a -> a -> Vector a
changeXtoY (Vector (x:xs)) ix y
    | xs == []  = if (x == ix)
                    then Vector [y] else Vector [x]
    | ix == x   = Vector (y : values (changeXtoY (Vector xs) ix y))
    | otherwise = Vector (x : values (changeXtoY (Vector xs) ix y))

-- todo put changeItoY and changeItoYunder into one function
-- Changes number at:i to value:y
-- parent function to changeItoYunder
changeItoY :: (Eq a, Num a) => Vector a -> Int -> a -> Vector a
changeItoY list i y = changeItoYunder list ((length $ values list)-i-1) y

-- Changes number at:i to value:y with index
-- child function to changeItoY
changeItoYunder :: (Eq a, Num a) => Vector a -> Int -> a -> Vector a
changeItoYunder (Vector (x:xs)) i y
    | xs == []          = if ((length xs) == i )
                            then Vector [y] else Vector [x]
    | (length $ xs) == i  = Vector $ y : values (changeItoYunder (Vector xs) i y)
    | otherwise         = Vector $ x : values (changeItoYunder (Vector xs) i y)

-- todo fix reverse indexiation
-- Create vector where entry at index:y has the value of:x and dimension: dim
fillOneV :: (Eq a, Num a) => a -> Int -> Int -> Vector a
fillOneV x y dim
    | dim == 0  = Vector []
    | y == dim  = Vector $ x : values (fillOneV x y (dim-1))
    | otherwise = Vector $ 0 : values (fillOneV x y (dim-1))
