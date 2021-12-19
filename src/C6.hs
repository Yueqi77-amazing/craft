module C6 (
    printBill
) where

import Data.Char
import Data.Foldable
type Picture = [[Char]]
type Position = (Int , Int)
type Image = (Picture, Position)
cheese :: Picture    
cheese = ["############",
          "#....##..#.#",
          "#..##.....##",
          "#.#.......##",
          "#.#...#...##",
          "#.#...###.##",
          "##....#..###",
          "#.#...#....#",
          "#..#...#...#",
          "#...#..#...#",
          "#....#.#...#",
          "############"]
--6.1
superImposeChar :: Char -> Char -> Char
superImposeChar '.' '.' = '.'
superImposeChar a b = '#'

--6.2
superImposeLine :: [Char] -> [Char] ->[Char]
superImposeLine linea lineb = [superImposeChar a b |(a,b)<- zip linea lineb ]

--6.3
superImpose :: Picture -> Picture -> Picture
superImpose pica picb = [superImposeLine a b | (a,b) <- zip pica picb]

--6.4
printPic :: Picture -> IO()
printPic pic = putStr(foldl (\x y -> x++ y ++ "\n") "" pic)

--6.6
rotateLine :: Picture -> Int -> [Char]
rotateLine pic n = reverse [line !! n| line <- pic] 

rotate90 :: Picture -> Picture
rotate90 pic = [rotateLine pic i|i<-[0..(length pic)-1] ]

--6.8
scaleX :: [Char] -> Int ->[Char]
scaleX line n = foldl (++) "" [replicate n c | c<-line]

scaleL :: Picture -> Int -> Picture
scaleL pic n =concat [replicate n ch| ch<-pic]

scale:: Picture -> Int -> Picture
scale pic n = scaleL [scaleX l n|l<-pic] n

--6.9
makeImage :: Picture ->Position ->Image    
makeImage pic pos = (pic ,pos)

--6.10
changePosition :: Image -> Position -> Image
changePosition im po = let (x,y) = im in (x,po)

--6.11
posA :: Image->Int
posA im = (fst.snd)im
posB :: Image ->Int
posB im = (snd.snd) im
moveImage :: Image -> Int -> Int -> Image
moveImage im a b = changePosition im (posA im + a, posB im +b)

--6.18
maxThreeOccurs :: Int -> Int -> Int -> (Int,Int)
maxThreeOccurs a b c 
    | a == b && b== c =(a,3)
    | a/=b = maxOccur (fst(maxOccur a b)) c
    | otherwise = maxOccur (fst (maxOccur a c)) b
    where maxOccur x y 
            | x >y = (x,1)
            | x == y  =(x,2)
            | otherwise  = (y,1)
maxThreeOccurs1 :: Int -> Int -> Int -> (Int,Int)
maxThreeOccurs1 a b c = (maxO, times)
        where maxO = max a (max b c )
              times = length(filter(\x -> x == maxO) [a,b,c])

--6.20
type Name     = String
type Price    = Int
type BarCode  = Int
type Database = [(BarCode, Name, Price)]

type TillType = [BarCode]
type BillType = [(Name, Price)]

codeIndex :: Database
codeIndex = [(4719, "Barries nuts", 345),
               (3513, "Smolik deck", 5945),
               (3883, "Badass Jacket", 595),
              (4758, "Cheeseburger", 3943),
             (1523, "Whale dildo", 1535),
             (8583, "Muska Shoes", 7900)]
lineLength :: Int
lineLength = 30

   --produceBill :: TillType -> String
  -- produceBill = formatBill . makeBill

prifixZero :: String -> String
prifixZero a 
    | length a < 2 = "0"++a
    | otherwise =a 

formatPence :: Price -> String
formatPence price = a ++ "."++b
    where a = show( div price 100)
          b = (prifixZero.show) (mod price 100)
 
seperate:: String -> String
seperate a = replicate (lineLength - length a) '.'

formatLine :: (Name,Price) ->String
formatLine (name,price) = name ++ se++ pr ++"\n"
    where pr = formatPence price
          se = seperate name

formatLines :: [(Name,Price)] -> String
formatLines a = foldl (++) "" formatedLine 
        where formatedLine = [formatLine line | line <- a]

makeTotal :: BillType -> Price
makeTotal bill = foldl (+) 0 price where price = [snd pair | pair<-bill]

formatTotal :: Price -> String
formatTotal price = "\nTotal" ++ se ++ pr
    where pr = formatPence price
          se = seperate (formatPence price)
          
formatBill :: BillType -> String
formatBill bill = re ++ "Haskel Stores\n\n"  ++ li ++ total
    where li = formatLines bill
          total = (formatTotal.makeTotal) bill
          re =replicate 8 ' '

look :: Database ->BarCode -> (Name,Price)
look database barcode 
    | null match = ("unknown Item" , 0)
    | otherwise = head match 
    where match = [(a,c)| (m,a,c)<-database,m==barcode]

lookupbc :: BarCode -> (Name, Price)
lookupbc bc = look codeIndex bc

makeBill :: TillType -> BillType
makeBill bar = [lookupbc barc|barc <-bar]

printBill :: IO ()
printBill = (putStr . formatBill . makeBill) [3513, 2000, 3883, 1523]