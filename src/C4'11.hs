module C4'11 (
    regions
) where  

sumFun :: (Int -> Int) -> Int -> Int
sumFun f n
    | n==0  = f 0
    | n>0   = sumFun f (n - 1) + f n

    
regions :: Int -> Int
regions n = sumFun (\x -> x) n