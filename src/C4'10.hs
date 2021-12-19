module C4'10 (
    find  
) where

f :: Int -> Int
f 1 = 1
f 2 =23
f 0 =23
f 3 = 22
f 4 =298
f 5= 0
f 6 =99
f _ = 100

find :: Int -> Bool
find 0 = f 0 == 0
find n = f n ==0 || find(n-1)