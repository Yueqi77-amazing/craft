module C7 (
    printNow
) where

--7.4
products :: [Int] -> Int
products a = case a of
    [] -> 0
    x:xs -> x* product xs 

--7.5
and1 :: [Bool] -> Bool
and1 [] = True
and1 (x:xs) = x&& and1 xs

--7.6
elemNum :: Int ->[Int]->Int
elemNum x xs = length [a|a<-xs,a==x]

--7.7
unique::[Int] -> [Int]
unique x = [a|a<-x, elemNum a x==1] 

--7.8
rever :: [Int] -> [Int]
rever (x:xs) = concat[(rever xs),[x]]
rever [] =[]

unzip2 :: [(a,b)] -> ([a],[b])
unzip2 [] = ([],[])
unzip2 (x:xs) = (fst x :fst( unzip2 xs),snd x:snd(unzip2 xs))

unzip4 :: [(Int,Int)] -> ([Int],[Int])
unzip4 [] = ([],[])
unzip4 [(a,b)] = ([a],[b])

--7.9
ins :: Int -> [Int] ->[Int]
ins x [] = [x]
ins x (y:ys)
    | x<y =x:(y:ys)
    | x==y =y:ys
    | otherwise =y:ins x ys

iSort::[Int]->[Int]
iSort [] =[]
iSort (x:xs) = ins x (iSort xs)

drop1:: Int->[a]->[a]
drop1 0 a = a
drop1 _ [] =[]
drop1 n (x:xs) = drop1 (n-1) xs

subList :: Eq a => [a] -> [a] -> Bool
subList a b
    | null a =True
    | null b = False
    | head a /= head b  =subList a (tail b)
    | head a==head b =subList (tail a) (tail b)

subSequence :: Eq a => [a] -> [a] -> Bool
subSequence a b
    | null a =True
    | null b = False 
    | a == take (length a) b = True
    | otherwise = subSequence a (tail b)

--7.20
whitespace = ['\n', '\t', ' ']
type Word1 = String
type Line = [Word1]
lineLen :: Int
lineLen = 35




joinLine:: Line -> String
joinLine [] = ""
joinLine (x:xs) = x ++ " " ++joinLine xs

joinLines :: [Line] -> String
joinLines [] =""
joinLines (x:xs) = joinLine x ++ "\n" ++ joinLines xs


isPalin :: String -> Bool
isPalin [] = True
isPalin (x:xs) 
    | x== last (x:xs) = isPalin str 
    | x == ' ' = isPalin xs
    | otherwise = False
            where str = take (length xs -1) xs


printNow = print $ drop1 10 [1,2,3,4,5,6,7]
