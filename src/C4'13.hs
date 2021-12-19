module C4'13 (
    comFac
) where

--max commen factor


remainder :: Int -> Int ->Int
remainder x y
    | x<y = x
    | otherwise = remainder (x-y) y
    
comFac :: Int -> Int -> Int
comFac n 0  = n
comFac n m = comFac m (remainder n m)