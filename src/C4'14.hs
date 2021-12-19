module C4'14 (
    power
) where

power :: Int -> Int
power n 
    | n == 0 = 1
    | even n = ( power (divide n 2 )) ^ 2
    | otherwise = (power (divide (n-1) 2 ) ) ^ 2 * 2

divide:: Int -> Int -> Int
divide m n 
    | m <n =0
    | otherwise =1+divide (m-n) n