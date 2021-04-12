module Chapter14 where


import Data.List
import Test.QuickCheck
import Data.Char


--14.1
-- re implementing the library db
type Price = Float 
type Name = String 
type Barcode = Int

data Database = DB Barcode Name Price
    deriving(Read, Show, Eq)


--data Expr = Lit Integer | 
 --   Add Expr Expr | 
 --  Sub Expr Expr

{--
eval,size :: Expr->Integer
eval (Lit a) = a 
eval (Add a b) = eval a + eval b
eval (Sub a b) = eval a - eval b

showw::Expr -> String
showw (Lit n) = show n
showw (Add e1 e2) = "(" ++ showw e1 ++ "+" ++ showw e2 ++ ")"
showw (Sub e1 e2) = "(" ++ showw e1 ++ "-" ++ showw e2 ++ ")"

size (Lit n) = undefined
--}





--Finally treeeeeeeeeeeeees
data NTree = NilT | Node Integer NTree NTree
    deriving(Eq, Show )


 
e1 :: NTree
e1 = Node 10 NilT NilT

e2 :: NTree
e2 = Node 17 ( Node 14 NilT NilT) (Node 20 NilT NilT) 


sumTree, depth :: NTree-> Integer
sumTree NilT = 0
sumTree (Node n t1 t2) = n + sumTree(t1) + sumTree(t2)


depth NilT = 0
depth (Node n t1 t2) = 1 + max (depth t1) (depth t2)




-- how many times a number occurs in a tree
occur :: NTree -> Integer -> Integer
occur NilT  x = 0
occur (Node n t1 t2) x 
    | n==x = 1 + occur t1 x+ occur t2 x
    | otherwise = occur t1 x+ occur t2 x



data Expr = Lit Integer | 
    Op Ops Expr Expr | If BExpr Expr Expr--If is a consturcor



data Ops = Add | Sub | Mul | Div | Mod
data BExpr = BoolLit Bool | And BExpr BExpr | Not BExpr | Equal Expr Expr | Greater Expr Expr




eval :: Expr->Integer
eval (Lit a) = a 
eval (Op Add a b) = eval a + eval b
eval (Op Sub a b) = eval a - eval b
eval (Op Mul a b) = eval a * eval b
eval (Op Div a b) = div (eval a)  (eval b)
eval (Op Mod a b) = mod (eval a)  (eval b)


beval::BExpr->Bool
beval( BoolLit b  ) = b
beval (And a b) = beval a && beval b
beval(Not a) = not (beval a)
beval (Equal a b) = eval a == eval b
beval (Greater a b) = eval a > eval b








--Left NTree
leftTree::NTree-> NTree
leftTree (Node n x y) 
    | x == NilT = NilT
    | otherwise = x

--Roght NTree
rightTree::NTree-> NTree
rightTree (Node n x y) 
    | y == NilT = NilT
    | otherwise = y




elemInTree::NTree->Integer->Bool
elemInTree NilT z = False
elemInTree (Node n x y) z = n==z || elemInTree x z || elemInTree y z

collapse::NTree->[Integer]
collapse NilT = []
collapse (Node n x y) = [n]++ collapse x ++ collapse y


maxInTree,minInTree::NTree->Integer

minInTree t = head . sort $ collapse t
maxInTree t = last . sort $ collapse t



reflect::NTree->NTree
reflect NilT = NilT
reflect (Node n x y) = Node n (reflect y) (reflect x)


sortTreetoList::NTree->[Integer]
sortTreetoList t = sort $ collapse t



--Buitl in list definition

data List a = NilL | a ::: (List a)
    deriving(Eq,Ord,Show,Read)




data Tree a = Nil | Nod a (Tree a) (Tree a)
    deriving(Eq,Ord,Show,Read)


depth2::Tree a -> Integer
depth2 Nil = 0
depth2 (Nod a b c) = 1 + max (depth2 b)  (depth2 c)

collapse2::Tree a -> [a]
collapse2 Nil = []
collapse2 (Nod a b c) = [a] ++ collapse2 b ++ collapse2 c 


--mapTree!
mapTree::(a->b)->Tree a->Tree b
mapTree f Nil = Nil
mapTree f (Nod n a b) = Nod (f n) (mapTree f a) (mapTree f b) 



data EITHER a b =  LEFT a | RIGHT b
    deriving(Eq,Ord,Read,Show)



isLEFT:: EITHER a b -> Bool 
isLEFT (LEFT _) = True
isLEFT (RIGHT _) = False 


apply::(a->c)->(b->c)-> EITHER a b -> c
apply f g (LEFT x) = f x
apply f g (RIGHT y) = g y
--otherwise applyleft:: (a->c) -> Either a b -> c ; apply f (LEFT x) = f and thennnnnn applyRigh::(b->c)-> Either a b -> c....


--14.17
twist::EITHER a b -> EITHER b a
twist (LEFT x) = RIGHT x
twist (RIGHT y) = LEFT y



applyLeft :: (a -> c) -> EITHER a b -> c
applyLeft f x = apply f (error "applyLeft applied to Right") x

join::(a->c)->(b->d)->EITHER a b -> EITHER c d
join f g (LEFT x) =  LEFT (f x)
join f g (RIGHT y) =  RIGHT (g y)
  
data GTree a = Leaf a | Gnode [GTree a]

countLeaves::GTree a -> Integer
countLeaves (Leaf x) = 1
countLeaves (Gnode [y]) = 1 +  countLeaves y 


data MAYBE a = NOTHING | JUST a
    deriving(Eq, Ord,Show,Read)



errDiv2::Integer->Integer->  Integer
errDiv2 n m 
    | (m /= 0) = div n m
    | otherwise = 0

errDiv::Integer->Integer-> MAYBE  Integer
errDiv n m 
    | (m /= 0) = JUST (div n m)
    | otherwise = NOTHING  



--Mininmum distance

data Edit = Change  Char | Copy | Delete | Insert Char | Kill
    deriving(Eq, Show)


transform ::String -> String -> [Edit]
--simply from and to
transform [] [] = []
transform from []  = [Kill]
transform [] to = map Insert to


