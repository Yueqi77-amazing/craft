module Chapter4'1 (
    
    maxFour1
    ) where

    maxThree :: Integer -> Integer -> Integer -> Integer
    maxThree a b c =  max (max a b) c

    maxFour1, maxFour2, maxFour3 :: Integer -> Integer -> Integer -> Integer -> Integer

    maxFour1 a b c d
        | a > b &&  a > c &&  a > d  = a
        | b > c &&  b > d  = b
        | c > d      = c
        | otherwise   = d


    maxFour2 a b c d =  max ( max (max a b) c ) d


    maxFour3 a b c d = max (maxThree a b c) d

