module C4'2 (
weakAsendingOrder
) where 

between :: Integer ->Integer -> Integer -> Bool
between x y z 
    | x<=y && z>=y = True
    | otherwise = False

middleNumber :: Integer -> Integer-> Integer ->Integer
middleNumber x y z
    | between y x z  = z
    | between x y z  = y
    | otherwise      = z

weakAsendingOrder :: Integer -> Integer -> Integer -> Bool
weakAsendingOrder x y z 
    | between x y z = True
    | otherwise = False

