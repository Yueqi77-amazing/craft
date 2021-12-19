module C4'5 (
    rangeProduct
    ) where


rangeProduct :: Int -> Int -> Int
rangeProduct a b
    | a> b = 0
    | a== b = b
    | otherwise = let x = a +1 in rangeProduct x b  * a
                    

fib :: Int -> Int
fib n
    | n == 0 = 0
    | n==1 = 1
    | n>1 =fib(n-2) + fib (n-1)