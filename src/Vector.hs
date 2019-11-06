module Vector (
    changeXtoY,
    changeItoY,
    xVector,
    zeroV,
    fillOneV,
    Vector,
    vectorDiv,
    vectorSub
) where

    
--
-- create vector type
--
data Vector a = Vector [a]
--

--
-- vector creation
--

xVector :: Int -> Int -> Vector
xVector dim val
    | dim == 0  = []
    | otherwise = val : xVector (dim - 1) val

zeroV :: Int -> Vector
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

vectorSub :: Vector -> Vector -> Vector
vectorSub (x:xs) (y:ys)
    | xs == []  = x-y : []
    | otherwise = x-y : vectorSub xs ys

vectorDiv :: Vector -> Int -> Vector
vectorDiv (x:xs) quotient
    | xs == []  = (x `div` quotient) : []
    | otherwise = (x `div` quotient) : (vectorDiv xs quotient)

changeXtoY :: Vector -> Int -> Int -> Vector
changeXtoY (x:xs) ix y
    | xs == []  = if (x == ix)
                    then y : [] else x: []
    | ix == x   = y : changeXtoY xs ix y
    | otherwise = x : changeXtoY xs ix y

changeItoY :: Vector -> Int -> Int -> Vector
changeItoY list i y = changeItoYunder list (length list - i - 1) y

changeItoYunder :: Vector -> Int -> Int -> Vector
changeItoYunder (x:xs) i y
    | xs == []          = if ((length xs) == i )
                            then y : [] else x: []
    | (length xs) == i  = y : changeItoYunder xs i y
    | otherwise         = x : changeItoYunder xs i y

fillOneV :: Int -> Int -> Int -> Vector
fillOneV x y dim
    | dim == 0  = []
    | y == dim  = x : fillOneV x y (dim-1)
    | otherwise = 0 : fillOneV x y (dim-1)
