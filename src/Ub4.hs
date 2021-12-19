module Ub4(

) where


type Env t = t -> Integer 


data Evp t = Var t| Const Integer | Add (Evp t) (Evp t) 
            | And (Evp t) (Evp t) | Less (Evp t) (Evp t) 
            |  Not (Evp t)  | If (Evp t) (Evp t) (Evp t)
eval :: Env t -> Evp t -> Integer
eval env(Var t) = env t
eval env(Const a) = a 
eval env(Add a b ) = eval env a + eval env b
eval env (Less e1 e2)
    | eval env e1 < eval env e2 = 1
    | otherwise = 0
eval env (And e1 e2)
    | eval env e1 == 0 || eval env e2 == 0 = 0
    | otherwise = 1 

eval env (Not e)
    | eval env e == 1 =0
    | otherwise =1

eval env (If a b c)
    | eval env a ==1 = eval env b
    | otherwise = eval env c