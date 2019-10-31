module Matrix (
    transpose,
    matrixMult,
    unit,
    zero,
    zeroV,
    xMatrix,
    changeXtoY,
    xVector
) where


-- upperTri :: Int -> [[Int]]
-- upperTri dim
--     | 
--     | otherwise = 

xMatrix :: Int -> Int -> Int -> [[Int]]
xMatrix rowDim colDim val
    | rowDim == 0   = []
    | otherwise     = (xVector colDim val) : (xMatrix (rowDim-1) colDim val)

zero :: Int -> Int -> [[Int]]
zero rowDim colDim = xMatrix rowDim colDim 0

unit :: Int -> [[Int]]
unit dim = unitCnt dim dim

unitCnt :: Int -> Int -> [[Int]]
unitCnt dim dimCnt
    | dimCnt-1 == 0   = fillOneV 1 dimCnt dim : []
    | otherwise     = (fillOneV 1 dimCnt dim) : (unitCnt dim (dimCnt-1))

--
-- Vector stuff
--

changeXtoY :: [Int] -> Int -> Int -> [Int]
changeXtoY (x:xs) ix y
    | xs == []  = if (x == ix)
                    then y : [] else x: []
    | ix == x   = y : changeXtoY xs ix y
    | otherwise = x : changeXtoY xs ix y

changeItoY :: [Int] -> Int -> Int -> [Int]
changeItoY list i y = changeItoYunder list (length list - i - 1) y

changeItoYunder :: [Int] -> Int -> Int -> [Int]
changeItoYunder (x:xs) i y
    | xs == []          = if ((length xs) == i )
                            then y : [] else x: []
    | (length xs) == i  = y : changeItoYunder xs i y
    | otherwise         = x : changeItoYunder xs i y

fillOneV :: Int -> Int -> Int -> [Int]
fillOneV x y dim
    | dim == 0  = []
    | y == dim  = x : fillOneV x y (dim-1)
    | otherwise = 0 : fillOneV x y (dim-1)

xVector :: Int -> Int -> [Int]
xVector dim val
    | dim == 0  = []
    | otherwise = val : xVector (dim - 1) val

zeroV :: Int -> [Int]
zeroV dim = xVector dim 0

-- onesTill :: Int -> Int -> [Int]
-- onesTill dim a
--     | a == 0    = 
--     | otherwise = 

-- onesFrom :: Int -> [Int]

transpose :: [[Int]] -> [[Int]]
transpose (x:xs)
    | xs == []  = arrPlode x
    | otherwise = arrConn (arrPlode x) (transpose xs)


arrPlode :: [Int] -> [[Int]]
arrPlode (x:xs)
    | xs == []  = [[x]]
    | otherwise = [x] : (arrPlode xs)

arrConn :: [[Int]] -> [[Int]] -> [[Int]]
arrConn (x:xs) (y:ys)
    | xs == []  = [x ++ y]
    | otherwise = (x ++ y) : (arrConn xs ys)


matrixMult :: [[Int]] -> [[Int]] -> [[Int]]
matrixMult a b = matrixMultTransposed a (transpose b)


matrixMultTransposed  :: [[Int]] -> [[Int]] -> [[Int]]
matrixMultTransposed (x:xs) b
    | xs == [] = rowByColumns x b : []
    | otherwise = (rowByColumns x b) : (matrixMultTransposed xs b)


rowByColumns :: [Int] -> [[Int]] -> [Int]
rowByColumns a (x:xs)
    | xs == [] = (rowByColumn a x) : []
    | otherwise = (rowByColumn a x) : (rowByColumns a xs)

rowByColumn :: [Int] -> [Int] -> Int
rowByColumn [] [] = 0
rowByColumn (x:xs) (y:ys) = x*y + (rowByColumn xs ys)