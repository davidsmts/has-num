--

--
-- vector creation
--

xVector :: Int -> a -> [a]
xVector dim val
    | dim == 0  = []
    | otherwise = val : xVector (dim - 1) val