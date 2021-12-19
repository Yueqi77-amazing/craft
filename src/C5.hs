module C5(
   fibTable
) where

import Data.Char
import Data.String
--5.1
maxOccur :: Int -> Int -> (Int,Int)
maxOccur x y 
    | x >y = (x,1)
    | x == y  =(x,2)
    | otherwise  = (y,1)

--
maxThreeOccur :: Int -> Int -> Int -> (Int, Int)
maxThreeOccur a b c 
    | a == b && b== c =(a,3)
    | a/=b = maxOccur (fst(maxOccur a b)) c
    | otherwise = maxOccur (fst (maxOccur a c)) b

--5.2
orderTriple :: (Int , Int , Int) -> ( Int , Int , Int)
orderTriple (a, b, c) = (minThree a b c, middle a b c , fst(maxThreeOccur a b c))


minThree :: Int -> Int -> Int -> Int
minThree a b c
    | a <= b && a<=c =a
    | b <=c && b <= a =b 
    | c <=a && c<=b =c        
middle :: Int -> Int -> Int -> Int
middle a b c
    | a >= b && a<=c || a <= b && a>=c  =a
    | b >=c && b <= a ||b <=c && b >= a=b 
    | c >=a && c<=b || c <=a && c>=b=c    

--5.8
doubleAll :: [Int] -> [Int]
doubleAll lst = [n*2|n<-lst]

--5.9


offset :: Int
offset = ord 'A' - ord 'a'
smallToUpper :: Char -> Char
smallToUpper ch
   | (ch >= 'a') && ('z' >= ch)  = chr (ord ch + offset)
   | otherwise                   = ch

capitalize :: String -> String
capitalize s = [smallToUpper c | c<-s]

capitalizeLetters :: String -> String
capitalizeLetters s = [smallToUpper c | c<-s, isAlpha c]

--5.10
divisors :: Int -> [Int]
divisors n = [i | i<-[1 .. n], mod n i == 0]

isPrime :: Int -> Bool
isPrime n
     | n >= 0     = length (divisors n) == 2
     | otherwise  = False
--5.11
matches :: Int -> [Int] ->[Int]
matches x y = [s | s<-y, s == x]

ele :: Int -> [Int] -> Bool
ele i lst =length (matches i lst) > 0

--5.20
romanDigit :: Char -> String
romanDigit x = ["0", "I", "II", "III", "IV", "V", "VI", "VII", "IIX", "IX"]!!((ord x )- 48)

--5.21
onThreeLines :: String -> String -> String ->String
onThreeLines a b c = a ++ "\t" ++ b ++ "\n" ++ c

--5.22
onSeperateLines :: [String] -> String
onSeperateLines (x:xs)
    | length (x:xs) == 1 = x
    | otherwise = x ++ "\n" ++ onSeperateLines xs

--5.23
duplicate :: String -> Int -> String
duplicate x n 
    | n >= 1 = x ++ (duplicate x (n-1))
    | n==0 =""

--5.24
pushRight :: String -> String
pushRight a
    | length a <12 = duplicate " " (12-(length a )) ++ a
    | otherwise =a

--5.26
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = (fib (n - 1)) + (fib (n - 2))

fibs :: Int -> Int -> String
fibs n to
    | n == to    = ""
    | otherwise  = (show n) ++ "\t" ++ (show (fib n)) ++ "\n" ++ (fibs (n + 1) to)

fibTable :: Int ->String
fibTable n = "n\tfib n\n" ++ (fibs 0 (n + 1))