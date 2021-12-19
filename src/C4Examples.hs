module C4Examples (
    power2
) where  

power2 :: Int -> Int
power2 n 
    | n == 0 =1
    | n>0 =2*power2(n-1)
    
--2的n次方

sumFacs :: Int -> Int
sumFacs n
    | n == 0 =1
    | n >0 =sumFacs(n-1) + fac n

fac :: Int -> Int
fac n 
    | n == 0  =1
    | n >0  = fac(n-1) * n
    | otherwise =0

    
--线可以划分为几个区域
regions :: Int -> Int
regions n 
    | n==0 =1
    | n>0 = regions n-1 +n