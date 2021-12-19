module C4'8 (
    sqRoot
) where

sqRoot :: Int ->Int
sqRoot n = findRoot 1 n

findRoot :: Int -> Int -> Int
findRoot a b
    | a^2 > b =a-1
    | otherwise = findRoot (a+1) b
