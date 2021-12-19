{-# OPTIONS_GHC -XFlexibleContexts #-}
{-# OPTIONS_GHC -XFlexibleInstances #-}
module C14'1 (
    print1414
) where
import Prelude
data NTree = NilT | Node Integer NTree NTree
sumTree, depth :: NTree -> Integer
sumTree NilT                      = 0
sumTree (Node val tree1 tree2)    = val + sumTree tree1 + sumTree tree2
depth NilT                        = 0
depth (Node _ tree1 tree2)        = 1 + max (depth tree1) (depth tree2)
--2
occurs :: NTree -> Integer -> Integer
occurs NilT _                = 0
occurs (Node val t1 t2) p    = (if val == p then 1 else 0) + occurs t1 p + occurs t2 p
occurs1 :: NTree-> Integer -> Integer
occurs1 NilT _ = 0
occurs1 (Node val t1 t2) p =(if val == p then 1 else 0) + occurs t1 p + occurs t2 p

data Expr = Lit Integer | Add Expr Expr | Sub Expr Expr
eval:: Expr -> Integer
eval (Lit n )= n
eval (Add e1 e2) = eval e1 + eval e2
eval (Sub e1 e2 )=eval e1 - eval e2
--3




assoc :: Expr -> Expr 
assoc (Lit n) = Lit n
assoc (Add e1 e2) = Add (assoc e1) (assoc e2)
assoc (Sub e1 e2) = Sub (assoc e1) (assoc e2)

--4
sizeOp :: Expr -> Integer
sizeOp (Lit n) = 1
sizeOp (Add e1 e2) = sizeOp e1 + sizeOp e2 +1

--5 
leftTree :: NTree -> NTree
leftTree NilT = error "no left"
leftTree (Node val t1 t2) = leftTree t1

--6
rightTree :: NTree -> NTree
rightTree NilT = error "no right"
rightTree (Node val t1 t2 )= rightTree t1

-- 7
lstTree ::NTree -> [Integer]
lstTree NilT =[]
lstTree (Node n t1 t2) = [n] ++ lstTree t1 ++ lstTree t2

--8
inTree :: NTree -> Bool
inTree (Node n t1 t2)= elem n (lstTree (Node n t1 t2))

--9
collapse,sortt:: NTree -> [Integer]
collapse NilT = []
collapse (Node n a b) = collapse a ++ [n] ++ collapse b
--10
sortt NilT =[]
sortt t1 = quicksort (collapse t1)

--11
quicksort :: [Integer] -> [Integer]
quicksort [] = []
quicksort (p:xs) = (quicksort lesser) ++ [p] ++ (quicksort greater)
    where
        lesser = filter (< p) xs
        greater = filter (>= p) xs
--12
mapTree :: (Integer-> Integer) -> NTree -> NTree 
mapTree _ NilT                  = NilT
mapTree f (Node vnd lt rt)    = Node (f vnd) (mapTree f lt) (mapTree f rt)

--13
print1414 :: IO()
print1414 = print $ quicksort [12222,452,234,333]