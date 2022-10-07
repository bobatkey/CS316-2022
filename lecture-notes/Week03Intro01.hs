{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Week03Intro01 where

{-  WEEK 3 : HIGHER ORDER FUNCTIONS

   myFunc = lambda x: x*x    -- Python

   x => x*x                  -- Javascript

   x -> x*x                  -- Java

   \x -> x*x                 -- Haskell
-}




{-  FUNCTIONS THAT RETURN FUNCTIONS -}

add :: Int -> (Int -> Int)
add x y = x + y

addTen :: Int -> Int
addTen = add 10

-- Lambda notation
add0 :: Int -> (Int -> Int)
add0 = \x -> (\y -> x + y)

 --   add0 5
 -- = (\x -> (\y -> x + y)) 5
 -- = (\y -> 5 + y)

 --   add0 5 6
 -- = (\x -> (\y -> x + y)) 5 6
 -- = (\y -> 5 + y) 6
 -- = 5 + 6
 -- = 11


-- Partial application
tag :: String -> String -> String
tag tagname body = "<" ++ tagname ++ ">" ++ body ++ "</" ++ tagname ++ ">"

p, strong, em, div :: String -> String
p = tag "p"
strong = tag "strong"
em = tag "em"
div = tag "div"

helloworld tagName = tag tagName "Hello world"

flip_v2 :: (a -> b -> c) -> b -> a -> c
flip_v2 f b a = f a b

helloworld_v2 = flip tag "hello world"
        --    = \a -> tag a "hello world"

{-  FUNCTIONS THAT TAKE FUNCTIONS AS ARGUMENTS -}

ten :: Int
ten = add 5 5

double :: Int -> Int
double x = add x x

-- applyCopy :: (Int -> Int -> Int) -> Int -> Int
applyCopy :: (t1 -> t1 -> t2) -> t1 -> t2
applyCopy f x = f x x

--   applyCopy add 5
-- = add 5 5
-- = 5 + 5
-- = 10

double_v2 :: Int -> Int
double_v2 = applyCopy add

square :: Int -> Int
square = applyCopy (\x -> \y -> x * y)
   --  = \x -> (\x -> \y -> x * y) x x
   --  = \x -> (\y -> x * y) x
   --  = \x -> x * x

{-  MAP and FILTER -}

doubleList :: [Int] -> [Int]
doubleList []     = []
doubleList (x:xs) = (x * 2) : doubleList xs

incrementList :: [Int] -> [Int]
incrementList []     = []
incrementList (x:xs) = (x + 1) : incrementList xs

-- These two functions generalised:

-- myMap :: (Int -> Int) -> [Int] -> [Int]
myMap :: (a -> b) -> [a] -> [b]
myMap f []     = []
myMap f (x:xs) = f x : myMap f xs

-- In the Haskell standard library as 'map'

-- Write a function that duplicates every element of a list
--    dupAll [1,2,3] == [1,1,2,2,3,3]
-- write your function using map and concat.

--  [1,     2,    3    ]
--   |      |     |
--   v      v     v
--  [[1,1],[2,2],[3,3]]

dupAll :: [a] -> [a]
dupAll xs = concat (map (\x -> [x,x]) xs)

-- WHY NOT: dupAll2 []     = []
--          dupAll2 (x:xs) = x : x : dupAll2 xs
--
--- This works, but (a) you have to think about the recursion and what is happening to each element at the same time
--                  (b) it doesn't demonstrate the 'bulk' nature of the 'map' operation

----- FILTERING

elementsLessThan5 :: [Int] -> [Int]
elementsLessThan5 []     = []
elementsLessThan5 (x:xs) = if x < 5 then x : elementsLessThan5 xs else elementsLessThan5 xs

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f []     = []
myFilter f (x:xs) = if f x then x : myFilter f xs else myFilter f xs

-- In the Haskell standard library as 'filter'

-- FROM table
-- WHERE condition
-- SELECT a * 2, b, c

db :: [(String,String)]
db = [("Alice", "A Christmas Carol"),
      ("Bob", "Tinker Tailor Soldier Spy"),
      ("Carol", "Alice in Wonderland")]

query = map (\(name, booktitle) -> booktitle)
        (filter (\(name, booktitle) -> length name > 3)
         db)

mapFilter selection condition db = map selection (filter condition db)
