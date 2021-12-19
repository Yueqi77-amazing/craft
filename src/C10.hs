module C10 (
    print10
)where
import Prelude

com2 :: (a->b)->(b->b->c)->(a->a->c)
com2 f g =(\x y -> g (f x) (f y))

plu = com2 sq add 3 4
    where sq x = x*x
          add y x = y+x

iter1 :: Int ->(a->a)->(a->a)
iter1 n f
    | n>0 = f.iter1(n-1) f
    | otherwise =id

succ1 n = n+1




total :: (Int -> Int)-> (Int -> Int)
total f =(\n ->foldl (+) 0 (map f [1..n]))
 
pl = total sq 5
    where sq  n = n*n

fun :: (t1 -> t1 -> t0) -> (t0 -> t0 -> t2) -> t1 -> t1 -> t2
fun = (\f g x y -> g (f x x) ( f y y))

pl3 = fun (+) (*) 3 5

print10::IO()
print10 = print  pl