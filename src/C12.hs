{-# OPTIONS_GHC -XFlexibleContexts #-}
{-# OPTIONS_GHC -XFlexibleInstances #-}
module C12 (
    print12
) where
import Data.Int
onLookupFirst :: Eq a=>[(a,b)]->a->b

onLookupFirst (x:xs) a
    | a== fst x = snd x
    | otherwise = onLookupFirst xs a

oneLookupSecond :: Eq b=>[(a,b)]->b->a
oneLookupSecond lst c =( head .map fst. filter (\(_,y)->y==c )) lst 

numEqual :: Eq a => [a] -> a -> Int
numEqual xs x    = length (filter (== x) xs)




class Info a where
    examples :: [a]
    size :: a -> Int
       
       
instance Info Char where
    examples =  [ 'a' , 'A' , 'z' , 'Z' , '0' , '9' ]
    size _   =  1
       
       -- GHCi> putStr (examples !! 0)
       --
       
       -- GHCi> putStr (examples !! 1)
       -- a
       
       -- GHCi> examples :: [Char]
       -- "aAzZ09"
       
       
instance Info Bool where
    examples =  [ True , False ]
    size _   =  1
       
       
instance Info Int where
    examples =  [ -100 .. 100 ]
    size _   =  1
       
       
       -- instance Info Shape where
       -- 
       --  examples =  [ Circle 3.0 , Rectangle 45.9 87.6 ]
       --  size _   =  round . area
       
       
instance Info a => Info [a] where
    examples =      [ [] ]
        ++  [ [x] | x <- examples ]
        ++  [ [x,y] | x <- examples , y <- examples ]
    size     =  foldr (+) 1 . map size
       
       
instance (Info a , Info b) => Info (a , b) where
    examples     =  [ (x , y) | x <- examples , y <- examples ]
    size (x , y) =     size x  
                        +  size y
                        +  1 
       
           
       
       --
       
       
instance (Info a, Info b, Info c) => Info (a,b,c) where
    examples = [(x,y,z)|x<-examples,y<-examples,z<-examples]      
    size (x,y,z) = size x + size y + size z +1

instance Info (Int->Bool) where
    examples = [(==1)]
    size _ =1

instance Info (Int -> Int) where

    examples =  [ id]
    size _   =  1
              
infoCompare :: (Info a, Info b)=> a -> b -> Bool
infoCompare a1 b1 = size a1 <=size b1
       
showBoolFun :: (Bool -> Bool) -> String
showBoolFun b
 =      " v     | b(v)  \n" ++  "---------------\n"
    ++  " True  | " ++ (show $ b True )  ++ "\n" ++  " False | " ++ (show $ b False) ++ "\n"

showBoolFunGen :: (a -> String) -> (Bool -> a) -> String
showBoolFunGen f b
    =      " v | b(v)  \n"
        ++  "----------\n"
        ++  " T | " ++ (f . b) True  ++ "\n"
        ++  " F | " ++ (f . b) False ++ "\n"
    
f :: Bool -> String 
f True = "T" 
f False = "F"
b :: Bool -> Bool   
b = not                     

                              
print12 :: IO()
print12 =putStr $ "\n" ++ showBoolFunGen f b ++ "\n"