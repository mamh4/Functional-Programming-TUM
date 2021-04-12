module HaskellTest where


import Data.List
import Test.QuickCheck



fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

main :: IO ()
main = do
  let fibs = map fib [0..9]
  print fibs

-----Book Exercises


--Chapter 2 ending Task 4
double::Integer->Integer
double x = x*2

square :: Integer->Integer
square x = x*x

doubleThenSquare :: Integer -> Integer
doubleThenSquare x = square ( double x)

squareThenDouble :: Integer -> Integer
squareThenDouble x = double ( square x)

--Chapter 3
--Understanding quickcheck
myNot :: Bool -> Bool
myNot True = False
myNot False = True

exOr:: Bool -> Bool -> Bool--This definition is called literals!
exOr True True = False
exOr True False = True 
exOr False True = True 
exOr False False = False


prop_myNot::Bool -> Bool
prop_myNot x=
  not x == myNot x

--To run quick check write in the terminal: quickCheck prop_myNot
threeDifferent:: Integer -> Integer -> Integer -> Bool
threeDifferent m n p = m /= n && n/= p && m /= p



fourDifferent:: Integer ->Integer -> Integer -> Integer -> Bool
fourDifferent a b c d = a==b && b==c && c==d


fourDifferent2 :: Integer ->Integer -> Integer -> Integer -> Bool
fourDifferent2 a b c d = not (threeDifferent a b c) && c == d

prop_fourDifferent :: Integer ->Integer -> Integer -> Integer -> Bool
prop_fourDifferent a b c d =
  fourDifferent a b c d  == fourDifferent2 a b c d

--Having three different evaluated to false does not mean that all are equal!


--Writing multiple properties
maxOfTwo :: Integer -> Integer -> Integer
maxOfTwo x y
  |x>y = x
  |otherwise = y
--Let's specify two properties, Namely:
  --1) MaxOfTwo has to be either bigger than or equal to x or y.
  --2) MaxOfTwo has to be equal to x or y or both

prop_maxOfTwo1:: Integer -> Integer -> Bool
prop_maxOfTwo1 x y = maxOfTwo x y >= x || maxOfTwo x y >= y

prop_maxOfTwo2:: Integer -> Integer -> Bool
prop_maxOfTwo2 x y = maxOfTwo x y == x || maxOfTwo x y == y


-- To redefine prelude function we need to have the following line uncommented!
--import prelude hiding (the function/functions from prelude we want to redefine)
--doing so will avoid error "Ambigious occurance"


averageThree:: Integer -> Integer -> Integer ->Integer
averageThree a b c = (a+b+c) `div`3

howManyAboveAverage::Integer -> Integer -> Integer ->Float
howManyAboveAverage a b c 
    |  a > (averageThree a b c) &&  b > (averageThree a b c)  = 2
    |  a > (averageThree a b c) &&  c > (averageThree a b c)  = 2
    |  b > (averageThree a b c) &&  c > (averageThree a b c)  = 2
    |  a > (averageThree a b c)                               = 1
    |  b > (averageThree a b c)                               = 1
    |  c > (averageThree a b c)                               = 1

--Properties (averageThree)
--Maximum two numbers can be above average (or equal in case all numbers equal)
--Minimum of of one number below average (or equal in case all numbers equal)
--Absolute Sum of all three numbers has to be greater than or equal to absolute average

prop_averageThree1 :: Integer -> Integer -> Integer ->Bool
prop_averageThree1 a b c = a >= averageThree a b c || b >= averageThree a b c|| c >= averageThree a b c

prop_averageThree2 :: Integer -> Integer -> Integer ->Bool
prop_averageThree2 a b c = a <= averageThree a b c || b <= averageThree a b c || c <= averageThree a b c


prop_averageThree3 :: Integer -> Integer -> Integer ->Bool
prop_averageThree3 a b c = abs (a + b + c) >= abs (averageThree a b c)


--Properties (howManyAboveAverage)
--Maximum of 2/3 Numbers can be above average

prop_howManyAboveAverage :: Integer -> Integer -> Integer ->Bool
prop_howManyAboveAverage a b c = howManyAboveAverage a b c <=2

--------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------Chapter 4----------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------


--How to use where! "Local definitions"
--Area of a triangle is = sqrt(s* (s-a)*(s-b)*(s-c)) Where s = (a+b+c)/2

--In Haskell

triangleArea :: Float->Float->Float->Float --Only Three args since s is comprised of those 3 we don't specify it (or do another func)
triangleArea a b c = sqrt(s* (s-a)*(s-b)*(s-c))
  where
    s = (a+b+c)/2
--However what if the user entered a non possible triangle (length of side is not less than sum of other two sides)
triangleArea2 :: Float->Float->Float->Float
triangleArea2 a b c 
  | possible = sqrt(s* (s-a)*(s-b)*(s-c))
  | otherwise = 0
    where
    s = (a+b+c)/2
    possible = a+b >= c && b+c >= a && a+c >= b--By adding this as first argument we can minimise the time of computation as calculating 
    -- s may be redundant in case possible is set to False.  

--Scopes We can define functions scope!! in one line and define the function later!
{--
isOdd, isEven :: Int -> Bool

isOdd n 
  | n<=0 = False
  | otherwise = isEven(n-1)

isEven n
  |n<0 = False
  |n==0 = True
  |otherwise = isOdd(n-1)
------------------------Wow!! Recursion thrown between the two functions unless either 0 -> even or -ve -> odd! 

--}
--4.9 maxThreeOccurs
maxThreeOccurs:: Integer -> Integer -> Integer -> (Integer,Integer)
maxThreeOccurs a b c 
  | a > b && a > c = (a,1)
  | b > a && b > c = (b,1)
  | c > a && c > b = (c,1)
  | a == b && a > c = (a,2)
  | b == a && b > c = (b,2)
  | c == a && c > b = (c,2)
  | a==b && b == c = (a,3)

----------------------------------------------------------------------------------------------------------------------------------
--still chapter 4. Defining Data Types! i.e Integer, String, Char, Bool.. etc

data Move = Rock | Paper | Scissors
  deriving(Show, Eq)--Later!!

beat:: Move -> Move
beat Rock = Paper
beat Scissors = Rock
beat _ = Scissors

lose:: Move -> Move
lose Rock = Scissors
lose Scissors = Paper
lose _ = Rock

--4.11
data Results = Win | Lose | Draw
  deriving(Show, Eq)


outcome :: Move -> Move -> Results
outcome Rock Rock = Draw
outcome Paper Paper = Draw
outcome Scissors Scissors = Draw
outcome Rock Paper = Lose
outcome Rock Scissors = Win
outcome Paper Scissors = Lose
outcome Paper Rock = Win
outcome Scissors Rock = Lose
outcome _ Paper = Win --Wildcard Again!

--Properties of move -- Doesn't work need some advanced library!
prop_Move :: Move -> Bool
prop_Move x = beat x /= lose x


--4.16
data Month = January | February|  March|  April|  May|  June|  July|  August|  September|  October|  November|  December
  deriving(Show,Eq)

season :: Month -> String
season January = "Winter"
season February = "Winter"
season March = "Winter"
season April = "Winter"
season May = "Winter"
season June = "Spring"
season July = "Summer"
season August = "Summer"
season September = "Fall"
season October = "Fall"
season November = "Winter"
season December = "Winter"

pow :: Integer -> Integer -> Integer
pow a b 
  | b==0 = 1
  | b>0 = a * pow a (b-1)
  | otherwise = error "pow only works for Natural Numbers"

-- Recursion!

fac:: Integer->Integer
fac 0 = 1
fac x = x*fac(x-1)
--Why it stops at 0? because at 0 = 1 not 1* fac(x-1)!!!!


sumFacs:: Integer -> Integer
sumFacs 0 = 1 
sumFacs x = fac x + sumFacs (x-1)

--Try sumFacs fac 3! Explain! 


--Exercise 4.19
--Recursive definition of Multiplicatio using addition!
recMultiply:: Integer -> Integer -> Integer
recMultiply x 0 = 0
recMultiply x y = x + recMultiply x (y-1)


--For all real numbers
recMultiply2:: Integer -> Integer -> Integer
recMultiply2 x y
  | y==0 || x == 0 = 0
  | x>0 && y>0 || y<0 && x<0 = a + recMultiply a b
  | x>0 && y<0 || x<0 && y>0 = negate ( a + recMultiply a b)
    where
      a = abs x
      b = abs y -1



--Exercise 4.19
intRoot1 :: Integer -> Integer
intRoot1 x
  | x >= (x-1)*(x-1) = x
  | otherwise = intRoot2 (x-1)


intRoot2:: Integer -> Integer
intRoot2 x 
  | x >= (x-1)*(x-1) = x
  | otherwise = intRoot1 (x-1)


findroot :: Int -> Int -> Int
findroot n s
  | s^2 <= n   = s
  | otherwise  = findroot n (s - 1)

--A function that maximises another function!------------------------------------------------------------------------------------
givenFunction:: Integer -> Integer
givenFunction 0 = 2
givenFunction 1 = 42
givenFunction 2 = 17
givenFunction 3 = 0
givenFunction 4 = 44
givenFunction _ = 0


pickMax:: Integer -> Integer
pickMax 0 = givenFunction 0
pickMax n =  maxOfTwo a b
  where
    a = givenFunction n
    b = pickMax (n-1)


--4.22
--A function that counts the number of times a function generates zero for inputs 0 till n. The function takes n as input!
counter:: Integer -> Integer
counter n
  |n < 0 = 0
  |givenFunction n == 0 = 1 + counter(n-1)
  |otherwise = counter(n-1)


zerosBool:: Integer->Bool
zerosBool n
 | counter n >= 1 = True
 | otherwise = False


--Remainder and divide
remainder, divide :: Integer -> Integer -> Integer
remainder m n 
  | m>n = remainder(m-n) n
  | otherwise = m

--Revise!!
divide m n
  | m<n = 0
  | otherwise = 1 + divide (m-n) n

--4.31
hcf :: Integer -> Integer -> Integer
hcf x y
  | x>y = remainder x y
  | x<y = remainder y x


twoPowCondition:: Integer -> Integer -> Integer
twoPowCondition 2 n
  | n ==0 = 1
  | even n = 2*twoPowCondition 2 (n-1)
  | odd n = 2*twoPowCondition 2 (n-1)


--Easier!!
twoPowCondition2:: Integer -> Integer
twoPowCondition2 n
  | n ==0 = 1
  | even n = 2*twoPowCondition 2 (n-1)
  | odd n = 2*twoPowCondition 2 (n-1)

--More required!
twoPowCondition3:: Integer -> Integer
twoPowCondition3 0 = 1
twoPowCondition3 1 = 2
twoPowCondition3 n = 2 * twoPowCondition3 (n-1)



--------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------Chapter 5----------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------

--Defining a type for a tuble
--Here we define a a type of a tuble that we name shop item where the tuble consists of a pair the item name and the price in pence!
type ShopItem = (String, Integer) --Remember types begin with capital letters!

--Now we define a type for a list, the list contains ShopItems and hence we name it Basket
type Basket = [ShopItem]--synonymally [(String, Integer)]

itemNames::Basket -> [String]
itemNames basket =  [str| (str,price) <- basket]

-- A function that provides Maximum and Minimum of two integers
maxAndMin::Integer -> Integer -> (Integer,Integer)
maxAndMin x y 
  |x>y = (x,y)
  |otherwise = (y,x)


--First Pattern Matching example
-- A function that adds up pairs
addPair::(Integer,Integer) -> Integer
addPair(x,y) = x+y

--By using addPair(3,2) the value 3 is matched to x and the value 2 is matched to y
--Patterns can contain literals ie. addPair (0,y) = y
--Patterns can contain nested patterns ie. shift:: ((Integer), Integer, Integer)-> (Integer, Integer, (Integer)) tuble within tuble!
-------- FUNCTIONS THAT PICK OUT PARTICULAR PARTS OF TUBLE CAN BE DEFINED BY PATTERN MATCHING, see below:
--------- These Functions are called selector functions, they avoid pattern matching
name::ShopItem -> String
price::ShopItem -> Integer
name (n,p) = n
price (n,p) = p



--Example addPair2 does not carry out pattern matching!
addPair2::(Integer,Integer)->Integer
addPair2 p = fst p + snd p
--Generally Pattern Matching is easier to read! 


-- Exercise 5.1
maxOccurs::Integer->Integer->(Integer,Integer)
maxOccurs x y
  | x==y = (y,2)
  | x>y = (x,1)
  | y>x = (y,1)

maxThreeOccurs2 :: Integer -> Integer -> Integer ->(Integer, Integer)
maxThreeOccurs2 x y z
  | fst (maxOccurs x y) == z = (x,3)
  | fst (maxOccurs x y) > z && x==y = (x, 2)
  | fst (maxOccurs x y) > z && x>y = (x, 1)
  | fst (maxOccurs x y) > z && x<y = (y, 1)

orderTriple :: (Integer,Integer,Integer)->(Integer,Integer,Integer)
orderTriple (x,y,z)
  | x<=y && x<=z && y<=z = (x,y,z)
  | x<=y && x<=z && y>=z = (x,z,y)
  | x<=y && x>=z = (z,x,y)
  | x>=y && x<=z = (y,x,z)
  | x>=y && x>=z && y>=z=(z,y,x)
  | x>=y && x>=z && y<=z=(y,z,x)

--Constructing data types and systems! 
--Non Pattern Matching Case
data People = Person Name Age--Person here is a binary constructor as it takes two arguments!
  deriving(Show, Eq)
--Here Persion serves as a constructor
type Name = String
type Age = Integer

showPerson:: People->String
showPerson (Person x y)= x ++ "--" ++ show y --show here relates to show in the deriving arg.


--Notice here we do not need the constructor person!!!!!! 
type People2 = (Name,Age)

showPerson2 ::People2 ->String
showPerson2 (x,y) = "The name is " ++ x ++ "And the age is " ++ show y  

{--
--Circles and rectangles!
data Shape = Circle Float | Rectangle Float Float
  deriving(Eq,Ord,Show)

--Here we define a function that tells us whether or not the shape is round!
isRound::Shape ->Bool
isRound (Circle _) = True
isRound (Rectangle _ _) = False


area::Shape -> Float
area (Circle r) = pi*r*r
area (Rectangle l w) = l*w 

--the area function is overloaded! 

--}

--Exercise 5.5 & 5.7
data Shape = Circle Float | Rectangle Float Float | Triangle Float Float Float
  deriving(Eq, Ord, Show)

isRound :: Shape -> Bool
isRound (Circle _) = True
isRound (Rectangle _ _) = False
isRound ( Triangle _ _ _) = False

area::Shape->Float
area ( Circle r) = pi*r*r
area (Rectangle l w) = l*w
area (Triangle a b c) = triangleArea a b c


perimeter::Shape ->Float
perimeter (Circle r)= 2*pi*r
perimeter (Rectangle l w)= 2*(l+w)
perimeter (Triangle a b c)= a+b+c


--Exercise 5.6
data ShopItem2 = Item (ItemName, Price)
type ItemName = String
type Price = Integer

showItem :: ShopItem2 -> (String ,ItemName, Price)
showItem ( Item (x,y)) = ("Item Name followed by its price", x,y)

--Constructor can be built as such Item::String->Integer->ShopItem----------------------
-- For non nullary constructors we need a word serving as a constructor!

--5.8

isRegular:: Shape ->Bool
isRegular (Circle _) = True --Circles are always regular!
isRegular(Rectangle l w) = l==w
isRegular(Triangle a b c) = a==b && b==c 


--Ascending list from 2 to 10 with a difference of two
--[2,4..10]--Start from 2 with difference of 4-2 until 10
-- also for chars/string
--[a,c..n]



-- show and read functions from the prelude!
--show converts integer and bool to string
--read converts from string to bool/Integer
-- if the automatic conversion is not to the type we want we can specify ie (read "3") :: Float!


--List Comprehension!
ex = [2,4,7]
--Try [2*n | n<-ex]

--Alternative
doubleThree:: Integer -> Integer ->Integer ->[Integer]
doubleThree x y z = [2*x, 2*y, 2*z]


--Very Important-- List Compression WITHOUT Pattern Matching:
addPairCompression::[(Integer, Integer)]->[Integer]
addPairCompression [(xs,ys)] = [x+y | (x,y)<- [(xs,ys)] ]

--With PatternMatching
addPairCompression2::[(Integer,Integer)]->[Integer]
addPairCompression2 pairList = [x+y | (x,y) <- pairList]
-- (x,y) are not matched to pairlist::[(Integer,Integer)]

--VERY IMPORTANT: NOTICE THE TWO FUNCTIONS WHEN ADDING TWO PAIRS IN A LIST AS AN INPUT INSTEAD OF ONLY ONE PAIR

-- A boolean Function that determines where all elements of a list are even

isEven, isOdd :: Integer -> Bool
isEven 0 = True
isEven n = isOdd(n-1)

isOdd 0 = False
isOdd 1 = True
isOdd n = isEven(n-1)

allEven::[Integer]->Bool
allEven list = [x|x<-list, isEven x]==list

totalRadii :: [Shape] -> Float
totalRadii list = sum [r | Circle r <- list] 

--What is the difference?!
sings::[[Integer]] -> [[Integer]]
sings xs = [[ x ]| [x] <- xs]

sings2::[[Integer]] -> [Integer]
sings2 xs = [ x | [x] <- xs]

sings3 ::[[Integer]] -> [[Integer]]
sings3 xs = [[x,y]| [x] <-xs, [y] <-xs]

sings4:: [Integer] -> [Integer]--Not integer, again what is the data type of 1,3,4!!!!!!!!!
sings4 xs = [x | x <- xs]

--Exercise 5.20
divisors:: Integer -> [Integer]
divisors x = [xs| xs <- [1..x], mod x xs == 0]



remainderAgain :: Integer -> Integer -> Integer
remainderAgain x y
  | x == y = y
  | otherwise = remainder (x-y) y

modulus:: Integer -> Integer -> Integer
modulus x y
  | remainderAgain x y == y = 0
  | otherwise = remainderAgain x y


isPrime:: Integer ->Bool
isPrime x
  | length (divisors x) ==2 = True
  | otherwise = False

--5.21

matches :: Integer -> [Integer] -> [Integer]
matches xs list = [x | x <- list, x == xs]

elem1 :: Integer -> [Integer] -> Bool
elem1 xs list
  | matches xs list /= [] = True
  | otherwise = False

-- Important	
onSeparateLines :: [String] -> String
onSeparateLines [] = ""
onSeparateLines lines
 | length lines == 1  = head lines
 | otherwise          = (head lines) ++ "\n" ++ (onSeparateLines (tail lines))



duplicate2:: String -> Integer -> String
duplicate2 text 0 = ""
duplicate2 text 1 = text
duplicate2 text copies = text ++ duplicate2 text (copies-1)

fibs :: Integer -> Integer -> String
fibs n to
  | n == to    = ""
  | otherwise  = (show n) ++ "\t" ++ (show (fib n)) ++ "\n" ++ (fibs (n + 1) to)



pushRight :: String -> Integer-> String 
pushRight s 0 = s
pushRight s linelength = " " ++ pushRight "" (linelength-1) ++ s

--A data base showing the borrower and the book as a pair.
--Data base example! 
type Database = [(Person, Book)]
type Person = String
type Book = String

exampleBase::Database
exampleBase = [("Alice", "Asterix"), ("Anna", "Little Women"),("Alice","Tintin"),("Rory", "Tintin")]

--Functions to be applied on the data Basket
books::Database->Person->[Book]
books db findperson = [ book | (p, book) <- db, p == findperson]


borrowers :: Database -> Book -> [Person]
borrowers db findbook = [person | (person, book) <- db, book == findbook]

borrowed :: Database-> Book ->Bool
borrowed db book = [] /= borrowers db book

numBorrowed :: Database -> Person -> Int
numBorrowed db person = length (books db person)

--Database updating functions!

makeLoan, returnLoan::Database -> Person -> Book -> Database
makeLoan db person book = [(person,book)] ++ db
returnLoan db person book = [(newperson,newbook) | (newperson,newbook) <- db, (newperson,newbook) /= (person,book)]


allBooks::Database->[Book]
allBooks db = [books | (person,books) <- db] -- All borrowed books in the database.





