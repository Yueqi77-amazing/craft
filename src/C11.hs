module C11(
    print11
) where

range :: [Char]
range = ['m'..'q'] 

--11.2
simplePalCheck :: String->Bool
simplePalCheck string 
    | (length first1) == 0 = True  
    | otherwise = False
        where com =zip (filter f string) (reverse (filter f string))
              f a 
                | elem a [' ','\t','\n','.',','] = False
                | otherwise = True
              first1 = [m|m<-com, fst m /= snd m]


subSeq :: String -> String-> Bool
subSeq [] _ = True
subSeq (_:_) [] =False
subSeq (x:xs) (y:ys)
    = subSeq (x:xs) ys|| frontSeq (x:xs) (y:ys)

frontSeq :: String -> String -> Bool
frontSeq [] _ = True
frontSeq (_:_) [] = False
frontSeq (x:xs) (y:ys) = x==y && frontSeq xs ys

count :: String -> String -> Int
count (x:xs) (y:ys)
    | take (length (x:xs)) (y:ys) == (x:xs) =0
    | otherwise = 1 + count (x:xs) ys

replace1 :: String -> String-> String -> String
replace1 (x:xs) (y:ys) new 
    | subSeq (x:xs) (y:ys) == True = take (count (x:xs) (y:ys)) (y:ys) ++ new 
            ++ drop (count (x:xs) (y:ys) + length (x:xs)) (y:ys)
    | otherwise = []
    

print11 :: IO()
print11 = print $ replace1 "srt" "stttttttjgjsrtdfasdfag" " love "