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
type Vector = [Int]
--

--
-- vector creation
--

xVector :: Int -> a -> Vector a
xVector dim val
    | dim == 0  = []
    | otherwise = val : xVector (dim - 1) val

zeroV :: Int -> Vector Int
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

vectorAdd :: Vector a -> Vector a -> Vector a
vectorAdd (x:xs) (y:ys)
    | xs == []  = x+y : []
    | otherwise = x+y : vectorAdd xs ys


vectorSub :: Vector a -> Vector a -> Vector a
vectorSub (x:xs) (y:ys)
    | xs == []  = x-y : []
    | otherwise = x-y : vectorSub xs ys

vectorDiv :: Vector a -> Int -> Vector a
vectorDiv (x:xs) quotient
    | xs == []  = (x `div` quotient) : []
    | otherwise = (x `div` quotient) : (vectorDiv xs quotient)

changeXtoY :: Vector a -> Int -> Int -> Vector a
changeXtoY (x:xs) ix y
    | xs == []  = if (x == ix)
                    then y : [] else x: []
    | ix == x   = y : changeXtoY xs ix y
    | otherwise = x : changeXtoY xs ix y

changeItoY :: Vector a -> Int -> Int -> Vector a
changeItoY list i y = changeItoYunder list (length list - i - 1) y

changeItoYunder :: Vector a -> Int -> Int -> Vector a
changeItoYunder (x:xs) i y
    | xs == []          = if ((length xs) == i )
                            then y : [] else x: []
    | (length xs) == i  = y : changeItoYunder xs i y
    | otherwise         = x : changeItoYunder xs i y

fillOneV :: Int -> Int -> Int -> Vector a
fillOneV x y dim
    | dim == 0  = []
    | y == dim  = x : fillOneV x y (dim-1)
    | otherwise = 0 : fillOneV x y (dim-1)
