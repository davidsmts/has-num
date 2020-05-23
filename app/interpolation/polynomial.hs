

--todo: Lagrange Grundpolynome

--todo: Polynome in Lagrange Form
--todo: Aitken / Neville Algorithms

--todo: Newton Grundpolynome

--todo: polyInterp_divDiff ::

dividedDifferences :: (Fractional a, Num a) => [a] -> [a] -> a
dividedDifferences xi yi
  | n > 1     = ((dividedDifferences (drop 1 xi) (drop 1 yi)) - (dividedDifferences (take (n-1) xi) (take (n-1) yi)))/((xi !! (n-1)) - (xi !! 0))
  | otherwise = yi !! 0
  where
    n = length yi
