{-# LANGUAGE DataKinds #-}
module Chapter13 where


import Data.List
import Test.QuickCheck
import Data.Char



-- A function that compares element to an element in a list
equalToInList::Eq a => a->[a]->Bool
equalToInList elem [elemInList] = elem == elemInList
--equalToInList 3 [3]
--equalToInList (2,"hi","Salut") [(2,"hi","Salut")]



-- 13.1
notEqual::Eq a => a->a->Bool 
notEqual x y
    | x==y = False
    | otherwise = True
-- /= redefined


--13.2

-- a function that returns how many times and item shows up in a list
numEqual::Eq a => a ->[a]->Int
numEqual elem [] = 0
numEqual elem (x:xs)
    | elem == x = 1 + numEqual elem xs
    | otherwise = numEqual elem xs


--Redefine Equality class and defining an instance over the type Bool

class MyEquality a where
    equalEqual,notEquall::a->a->Bool 
    equalEqual x y = not (notEquall x y)
    notEquall x y = not (equalEqual x y) 


instance MyEquality Bool where
    equalEqual True True = True
    equalEqual False False = True
    equalEqual _ _ = False 
    notEquall True True = False
    notEquall False False = False
    notEquall _ _ = True

--instance MyEquality Int where
--No thanks too difficult


-- Generic allequal by primative recusrion

allEqualList::Eq a =>[a]->Bool 
allEqualList [] = True 
allEqualList [x] = True 
allEqualList (x:xs:xss) = x==xs && allEqualList xss



class Info a where
    examples:: [a]
    size:: a -> Int -- If I remove I will be having a top level definition of size that cannot be overrided
    size _ = 1 --adds default value to the size function when taking any type variable as input, given type variable belongs to class

instance Info Bool where
    examples=[True,False]
    size _ = 1


instance Info Char where
    examples=['a','b', 'c', 'd','z', '0','9']
    size _ = 1

instance Info Integer where
    examples=[-100..100]
    size _ = 1

instance Info Int where
    examples=[-100..100]
    size _ = 1


data Shape = Circle Float | Rectangle Float Float | Triangle Float Float Float
    deriving(Show,Ord,Eq)
area::Shape -> Float
area (Circle r) = pi*r*r
area (Rectangle l w) = l*w 


instance Info Shape where
    examples = [Circle 3.0, Rectangle 4 7]
    size = round.area

instance Info a => Info [a] where
    examples = [[]]++
               [[x]|x<-examples]++
               [[x,y] | x<-examples, y<-examples]
    size = foldr (+) 1 . map size




--Building up to infocheck clone
-- (\x-> elem x examples)  A function that checks whether an argument belongs to examples! :p
-- map (\x-> elem x examples)  A function that checks whether an argument (list) belongs to examples! :p
-- and $ map (\x-> elem x examples)  A function that checks whether all argument (list items) belongs to examples! :p

--13.4
instance (Info a , Info b , Info c) => Info (a , b , c) where

 examples         =  [ (x , y , z) | x <- examples , y <- examples , z <- examples ]
 size (x , y , z) = size x + size y + size z + 1 


--13.6
instance Info Float where
    examples = [3.23,5.14]
    size _ = 1

--13.7
myCompareFunctionLT::(Ord a,Ord b, Info a, Info b) => a->b->Bool
myCompareFunctionLT x y = size x <= size y

data Ordering2 = MyLT | MyEQ | MyGT

--Bounded types i,e
--minBound::Int
--mindBound::Integer

--difference between rem and mod!!!
-- mod caluculates the remainder after plugging in a number into another number as many times as long as it does not exceed it
-- when the second arg number is higher mod automatically returns the first arg.
--Note when numbers are negative the result of mod is always negative i.e mod -3  -2 will be -1. -2 goes one time in -3 and remaind is -1
-- rem does the same

--Differences occur when one arg is negative!

--5 `mod` (-3) == -1
--5 `rem` (-3) == 2

--(-5) `mod` 3 == 1
--(-5) `rem` 3 == -2

--mod takes the sign of the first arg and the result is as if both were positive
-- rem takes the sign of the first arg and plugs in the number as many times (not truncated by zero) then returns remainder


--difference between quot and div:: both only return how many times a number is plugges with slight differences
--div is not truncated by zero infact it is truncated by -infinity.
--quot is truncated by zero and will only change sign in case of negative args.

--Chapter13> quot 3 (-5)
--0
--Chapter13> div 3 (-5) 
---1


--Fractional classextends Num which have the operators discussed above to include fractional division and reciprocal! / and recip

--Floating class include operations (functions) for the types Float and Double. namey pi exp log sqrt sin cos tan


--13.21
myfunction::(a,[a])->Bool
myfunction (x,list) = True


--13.22
[a]->[b]->a->b = [a]->[b]->a->b


