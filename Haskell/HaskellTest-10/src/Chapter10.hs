module Chapter10 where


import Data.List
import Test.QuickCheck
import Data.Char

doubleAll1,doubleAll2::[Integer]->[Integer]
doubleAll1 list = [2*x| x<-list]

doubleAll2 [] = []
doubleAll2 (x:xs)= 2*x:doubleAll2 xs

--Using Mapping
double::Integer ->Integer 
double x = 2*x

-- Now you can use map double (any list of integers) will get same output to doublAll1 and doubleAll2
--By doing so we define a function by the use of another function!

--10.1

--    doubleAll [ 2 , 1 , 7 ]
-- ~> [ integer * 2 | integer <- [ 2 , 1 ,  7 ] ]
--
--      integer                    2   1    7
--      integer * 2                4   2   14
--
-- ~>                            [ 4 , 2 , 14 ]



--    doubleAll' [ 2 , 1 , 7 ]
-- ~> doubleAll' ( 2 : [ 1 , 7 ] )
-- ~> ( 2 * 2 ) : doubleAll' [ 1 , 7 ]
-- ~> 4 : doubleAll' [ 1 , 7 ]
-- ~> 4 : doubleAll' ( 1 : [ 7 ] )
-- ~> 4 : ( 1 * 2 ) : doubleAll' [ 7 ]
-- ~> 4 : 2 : doubleAll' [ 7 ]
-- ~> 4 : 2 : doubleAll' ( 7 : [] )
-- ~> 4 : 2 : ( 7 * 2 ) : doubleAll' []
-- ~> 4 : 2 : 14 : doubleAll' []
-- ~> 4 : 2 : 14 : []
-- ~> [ 4 , 2 , 14 ]




-- ~> map double [ 2 , 1 , 7 ]
-- ~> ( double 2 ) : map double [ 1 , 7 ]
-- ~> ( 2 * 2 ) : map double [ 1 , 7 ]
-- ~> 4 : map double [ 1 , 7 ]
-- ~> 4 : ( double 1 ) : map double [ 7 ]
-- ~> 4 : ( 1 * 2 ) : map double [ 7 ]
-- ~> 4 : 2 : map double [ 7 ]
-- ~> 4 : 2 : ( double 7 ) : map double []
-- ~> 4 : 2 : ( 7 * 2 ) : map double []
-- ~> 4 : 2 : 14 : map double []
-- ~> 4 : 2 : 14 : []
-- ~> [ 4 , 2 , 14 ]


--length2,length3 ::[a]->Integer | NO NEED AS IT WILL BE HIGHER ORDER FUNCTION! 
length2 list = sum  (map (const 1) list)
{- Or
length = sum . map (const 1)
---Or
-}--Dot operator!


length3 list= sum (map one list)
    where
        one::t->Integer 
        one _ = 1

--10.3
addUp ns = filter greaterOne (map addOne ns)
    where
        greaterOne n = n>1
        addOne n = n+1

--Re-difinitions

addUpWrong ns = map addOne (filter greaterOne ns)
    where
        greaterOne n = n>1
        addOne n = n+1



addUpCorrect ns =  map addOne (filter greaterZero ns)
    where
        greaterZero n = n>0
        addOne n = n+1

prop_addUp::[Integer]->Bool 
prop_addUp list1 = addUp list1 == addUpCorrect list1 --Pass Quick Check



--10.6

square :: Integer -> Integer 
square x = x*x


squareList :: [Integer] -> [Integer]
squareList ns= map square ns

sumSquaresList :: [Integer] -> Integer
sumSquaresList ns = sum (squareList ns)

checkGreaterZero::[Integer]->Bool
checkGreaterZero ns = ns == filter g ns
    where
        g::Integer->Bool
        g 0 = False
        g _ = True


--REVISION!!!!


--A function that maximises another function!------------------------------------------------------------------------------------
givenFunction:: Integer -> Integer
givenFunction 0 = 2
givenFunction 1 = 42
givenFunction 2 = 17
givenFunction 3 = 0
givenFunction 4 = 44
givenFunction _ = 0

maxOfTwo :: Integer -> Integer -> Integer
maxOfTwo x y
  |x>y = x
  |otherwise = y

pickMax:: Integer -> Integer
pickMax 0 = givenFunction 0
pickMax n =  maxOfTwo a b
  where
    a = givenFunction n
    b = pickMax (n-1)



fiver::Integer -> Integer 
fiver 0 = 0
fiver 5 = 100
fiver n
    | n `mod` 5 == 0 = 100 + fiver (n - 5)
    | otherwise = 0


iSort::[Integer]->[Integer]
iSort [] = []
iSort (x:xs)= ins x (iSort xs)

ins::Integer -> [Integer] -> [Integer]
ins x [] = [x]
ins x (y:ys)
    | x<=y = x:(y:ys)
    | otherwise = y:ins x ys



maxFiver::[Integer]->Integer 
maxFiver list = last (iSort (map fiver list))


maxFunction :: (a -> Integer) -> [a] -> Integer
maxFunction f list = last (iSort (map f list))


--Revision ended

--10.7

minimiseUptoN::(Integer -> Integer) -> Integer  -> Integer
minimiseUptoN f n = head (iSort (map f ([0..n])))


listelemEqual::[Integer]->Bool 
listelemEqual [] = True
listelemEqual [_] = True 
listelemEqual (x:xs) = (x==head xs) && listelemEqual xs

allEqual::(Integer ->Integer )->Integer ->Bool
allEqual f n = listelemEqual( map f [0..n])

listelemGZero::[Integer]->Bool 
listelemGZero [] = True
listelemGZero [x] 
    | x>0 = True
    | otherwise = False  
listelemGZero (x:xs) = (x>0) && listelemGZero xs



greaterZero :: (Integer -> Integer )->Integer ->Bool
greaterZero f n = listelemGZero (map f [0..n])


increaseOrder::[Integer]->Bool 
increaseOrder [] = True
increaseOrder [_] = True 
increaseOrder (x:xs) = (x<=head xs) && increaseOrder xs


increasing::(Integer->Integer)->Integer->Bool 
increasing f n = increaseOrder (map f [0..n])

concat2::[[a]]->[a]
concat2 [] = []
concat2 (x:xs)= x ++ concat2 xs



rev2::[a]->[a]
rev2 [] =[]
rev2 (x:xs)= rev2 xs ++ [x]



--10.13
sumSquaresUptoN :: Integer -> Integer 
sumSquaresUptoN n = foldr (+) 0 (map square [1..n])

--10.14
sumSquaresNatural :: [Integer] -> Integer 
sumSquaresNatural xs = foldr (+) 0 (map square xs)


--10.15
--unzipByfoldr::[(a,a)]->([a],[a])
--unzipByfoldr listOfpair = 
item2::a->[a]->[a]
item2 x xs = xs

lastByfoldr::[a]->a
lastByfoldr xs = head (foldr item2 [] xs)
 
item22::a->a->a
item22 _ x = x

lastByfoldr1::[a]-> a 
lastByfoldr1 xs = foldr1 item22 xs

cons::a -> [a]->[a]
cons x xs = [x] ++ xs

initByfoldr::[a]->[a]
initByfoldr xs = tail (foldr cons [] xs)

--respectable last
showFirst::[Integer ]->Integer 
showFirst (x:xs)= x


lastByPrimRecs::[Integer ]->Integer
lastByPrimRecs list =  showFirst (shifting list)


lastByfoldyyy::[Integer ]->Integer 
lastByfoldyyy list= showFirst (foldr shift [] list) 



initByPrimRecS::[a]->[a]
initByPrimRecS list = dropFirst (shiftLasttoFirst list)

dropFirst::[a]->[a]
dropFirst []= []
dropFirst (x:xs)= xs

shiftLasttoFirst::[a]->[a]
shiftLasttoFirst [] = []
shiftLasttoFirst (x:xs)= (shifingtLasttoFirst x (shiftLasttoFirst xs))

shifingtLasttoFirst::a->[a]->[a]
shifingtLasttoFirst lastItem [] = [lastItem]
shifingtLasttoFirst predecessor (x:xs) = x:predecessor:xs


initByfoldrAdvanced::[a]->[a]
initByfoldrAdvanced xs = dropFirst (foldr shifingtLasttoFirst [] xs) 

--10.16
--mystery xs = xs


--Practice--No Primative recursion
switchFirstTwo::[a]->[a]
switchFirstTwo []= []
switchFirstTwo (x:xs) = [head xs]++[x]++tail xs



--Practise agaaaaiiin!
ins2::Integer->[Integer ]->[Integer ]
ins2 x []= [x]
ins2 x (y:ys)
    | x>=y = y:ins x ys
    | otherwise = x:y:ys


iSort2::[Integer ]->[Integer ]
iSort2 []= []
iSort2 (x:xs)= ins x (iSort2 xs)


--init AGAAAAAAAAAYN
shift::b -> [b ]->[b] 
shift a [] = [a]
shift a (x:xs)= x:a:xs

shifting ::[a]->[a ]
shifting []= []
shifting (x:xs)= shift x (shifting xs)

initBaby::[Integer ]->[Integer ]
initBaby [] = []
initBaby list= dropFirst (shifting list)
--initfoldyyy
initFoldyyy::[Integer ]->[Integer ]
initFoldyyy list = dropFirst (foldr shift [] list)



--10.18
filterFirst,filterLast::(a->Bool)->[a]->[a]
filterFirst p (x:xs)
    | p x = xs
    | otherwise = x:filterFirst p xs 

filterLast p xs = rev2 (filterFirst p (rev2 xs))


isEven::Integer -> Bool 
isEven 0 = True 
isEven n = isOdd (n-1)

isOdd::Integer->Bool 
isOdd 0 = False 
isOdd n = isEven (n-1)



--10.20: >Trials
addOne,addOneMapped,addTen::[Integer ]->[Integer ]
addOne []= []
addOne (x:xs)= (x+1):addOne xs
addOneToInt,addTenToInt::Integer -> Integer 
addOneToInt x = x+1
addTenToInt x = x+10
addOneMapped list = map addOneToInt list 
addTen []= []
addTen (x:xs)= (x+1):addTen xs


concatAlternateList::[Integer ]->[Integer ]->[Integer ]
concatAlternateList [] xs = xs
concatAlternateList (x:xs) (y:ys) = addOneToInt x: addTenToInt y:concatAlternateList (addOne xs) (addTen ys)

switchMap::(a->b)->(a->b)->[a]->[b]
switchMap f1 f2 [] = []
switchMap f1 f2 [a1] = [f1 a1]
switchMap f1 f2 [a1,a2]=[f1 a1, f2 a2]
switchMap f1 f2 (a1:a2:rest)= [f1 a1, f2 a2] ++ switchMap f1 f2 rest



--10.21
merge::([a ],[a ])->[a ]
merge ([x],[y])= [x]++[y]
merge((x:xs),(y:ys))= [x]++[y]++merge(xs,ys)


split::[a]->([a],[a])
split []= ([],[])
split [a1] = ([a1],[])
split ( a1 : a2 : rest )
 =  let ( l , r ) = ( split rest ) in ( a1 : l , a2 : r )

