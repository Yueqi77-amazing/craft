module C4'9 (
    pickMax
) where

pickMax :: Int -> Int
pickMax 0 = find 0
pickMax n = max (pickMax n-1) (find n)

find :: Int -> Int
find 0 = 12
find 1 = 44
find 2 = 89
find 3 = 76
find 4 = 56
find 5 = 34
find 6 = 8


