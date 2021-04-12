module Chapter11 where


import Data.List
import Test.QuickCheck
import Data.Char

add::Integer->Integer->Integer
add x y = x+y

sq,cube::Integer ->Integer 
sq x = x*x
cube x = x*x*x


--Function Composition

cubeSq = (sq . cube)

comp2::(a->b)->(b->b->c)->(a->a->c)
comp2 f g = (\x y -> g (f x) (f y))

whiteSpace::String 
whiteSpace = " \t\n"

--11.7
isWhiteSpace::Char->Bool
isWhiteSpace x =  (\x -> elem x (" \t\n")) x


--11.8
total::(Integer ->Integer )->(Integer ->Integer )
total f =  \n -> sum  $ map f [0..n]


multiply::Int->Int->Int 
multiply x y = x*y

--Parital Functions
doubleAll::[Int]->[Int]
doubleAll = map (multiply 2)

--multiply is applied to only one arg so as map!

--The order of the arguments affects the plausability of applying partial functions
--ie
--elem Char whiteSpace cannot be elem Char if we want to partially apply a function that checks whether a to-be defined input will be
    --white space
--to solve the issue 
member xs x = elem x xs

paritalisWhiteSpace :: Char -> Bool
paritalisWhiteSpace = member whiteSpace
--and now only the Char is missing


--lambda abstraction vs Parial application 
--addNum n = (\m->n+m)
--addNum n m = n+m
--addNum n

--(+) 2 9
-- +2 is partial!

addOneThenFilter :: [Integer] -> [Integer]
addOneThenFilter = filter (>0) . map (+1)



--11.11
--comp2::(a->b)->(b->b->c)->(a->a->c)
--comp2 f g = (\x y -> g (f x) (f y))


--IT IS IMPORTANT TO NOTICE THAT Comp2 above is already partial since the x and y are hidden by lambda abstraction

comp2full::(a->b)->(b->b->c)->(a->a->c)
comp2full f g x y = g (f x) (f y)


comp2Partial f g x = g   (f x) . f

totalPartial f = sum . map f . \n->[0..n]


--11.12
addOneThenFilterAlternative = map (+1) . filter (>(-1))


multiplyUC::(Int,Int) -> Int
multiplyUC (x,y) = x*y

--11.14
--uncurry ($)
--uncurry applies curried functions to uncurried/tuble arguments. the args of $ will then have to be in a tuble


--11.16
prop_uncurryZipUnzip :: (Eq a, Eq b) => a -> b -> Bool
prop_uncurryZipUnzip x y= (uncurry zip $ unzip [(x,y)]) == [(x,y)]


--11.17
curry3::(a->b->c->d)->((a,b,c)->d)
curry3 f (a,b,c) = f a b c


uncurry3::((a,b,c)->d)->a->b->c->d
uncurry3 f a b c = f (a,b,c)


--11.18
twice :: (a -> a) -> (a -> a)
twice f = f.f 

succc::Int->Int
succc x = x+1

--test twice succ 4
--Non Parial version :)
twice2 :: (b -> b) -> b -> b
twice2 f a = f . f $ a


--More generic not just twice

iter:: Int->(a->a)->(a->a)
iter n f
    | n>0 = f. iter (n-1) f
    | otherwise = id



--11.19



{--
iter 3 double 1=
double . iter 2 double 1=
double . double . iter 1 double 1=
double . double . double . iter 0 double 1=
double . double . double . 1=
double . double . 2=
double . 4=
8
--}

{--
--comp2::(a->b)->(b->b->c)->(a->a->c)
--comp2 f g = (\x y -> g (f x) (f y))

(comp2 succc (*)) 3 4=
(*)  succ 3 succc 4=
(*) 4 5=
20

--}

{--
comp2 sq add 3 4=
add (sq 3) (sq 4)=
add 9 16=
25

--}


--11.21

listOfFunctions :: Integer -> (a->a) -> [(a->a)]
listOfFunctions n f
    | n > 0 = ([f] ++ listOfFunctions (n-1) f)
    | otherwise = []



iterByfoldr :: Integer -> (a -> a) -> (a -> a)
iterByfoldr n f = foldr1 (.) (listOfFunctions n f)




addNumPartial::Integer -> (Integer->Integer )
addNumPartial n = (+n) 


--double all (applied on list) alternative definition using partial functions
doublAll2::[Int]->[Int]
doublAll2 = map (*2)

puzzle :: (a1 -> b -> c) -> a1 -> (a2 -> b) -> a2 -> c
puzzle = (.) (.)



getEvens,getOdds::[Int]->[Int]
getEvens = filter ((==0). (`mod`2))
getOdds = filter ((/=0). (`mod`2))


--Apply a list of functions to a particular argument
mapFuns,mapFuns2 :: [b -> a] -> b -> [a]
mapFuns [] x = []
mapFuns (f:fs) x = f x : mapFuns fs x
--Lambda Abstraction
mapFuns2 fs x = map (\f->f x) fs


--11.22
--lambda abstraction + even more partial function
mapFuns3 :: t -> [t -> b] -> [b]
mapFuns3 x = map (\f->f x)


switchArgs :: (t1 -> t2 -> t3) -> t2 -> t1 -> t3
switchArgs f x y = f y x

mapFuns4::[a->a]->(a->a)
mapFuns4 fs = foldr1 (.) fs


--11.23

myplusplus::[a]->[a]->[a]
myplusplus xs []= xs
myplusplus (x:xs) ys = x: myplusplus xs ys




doty2 :: (b -> c) -> (a -> b) -> a -> c
doty2 f g = \x->f (g x)




prop_filterPromotion :: Eq b => (b -> Bool) -> (a -> b) -> [a] -> Bool
prop_filterPromotion p f = \xs -> (filter p . map f) xs == (map f . filter (p.f)) xs


