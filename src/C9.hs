module C9(
   printM
) where
    
import Prelude

doubleAll3 :: [Int] ->[Int]
doubleAll3 [] = []
doubleAll3 a = [2*x|x<-a]

doubleAll1 ::[Int] ->[Int]
doubleAll1 [] =[]
doubleAll1 (x:xs) = 2*x:doubleAll1 xs

doubleAll2 :: [Int]-> [Int]
doubleAll2 [] = []
doubleAll2 a = map (*2) a

length1 :: [a] ->Int
length1 lst = sum (map (\x->1) lst)

addUp:: [Int]->[Int]
addUp lst = map (+1) (filter (>0) lst)

checkEq :: (Int -> Int) -> [Int] -> Bool
checkEq f [] = False
checkEq f a =and (zipWith (==) lst (replicate (length a ) n)) 
      where n = f (head a)
            lst = map f a

iter :: (Int->Int) ->Int->Int -> Int
iter f 0 x = x
iter f n x = f (iter f (n-1) x)

--9.11
sumN :: Int -> Int
sumN n = foldl (+) 0 [1..n]

--9.12
sumM :: [Int] -> Int
sumM a = foldl (+) 0 (map f c) 
      where c = [m| m<-a,m>0] 
            f x = x*x

--9.13
unZipa :: [(a,b)] -> ([a],[b])
unZipa lst = (left lst, right lst)
left a =foldr (++) [] ([map fst a])
right a = foldr (++) [] ([map snd a])

filterFirst :: (Char->Bool)-> String -> String
filterFirst p (x:xs) 
      | p x  = x:xs
      | otherwise =xs


             
printM = print $ filterFirst (==' ' ) "string"