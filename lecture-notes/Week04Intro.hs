{-# LANGUAGE ParallelListComp #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Week04Intro where

import Prelude hiding (foldr, foldl, Maybe (..), Left, Right, filter, zip, map, concat, tail)
import Data.List.Split (splitOn)
import Data.List hiding (foldr, foldl, filter, map, concat)

{-
   ANNOUNCEMENT:

     - Week 6 class test:
        - Week 6: 12:00 Wednesday 26th until 12:00 Thursday 27th
          - should take ~ 1-2 hours
        - Ten questions on the material from Weeks 1 - 5
        - Second attempt in week 9
        - Worth 50%

     - Coursework: to be released this afternoon
       - Deadline at the end of the semester
       - Worth 50%

      - 2pm this afternoon: RC512
        - Presentations by returning internship students
-}



{-
    WEEK 04 : PATTERNS OF RECURSION



-}




{- Part 4.1 : FOLDING RIGHT -}

-- total
total :: [Int] -> Int
total []     = 0
total (x:xs) = x + total xs

-- len
len :: [a] -> Int
len []     = 0
len (x:xs) = 1 + len xs

map_v0 :: (a -> b) -> [a] -> [b]
map_v0 f []     = []
map_v0 f (x:xs) = f x : map_v0 f xs

totalPlus :: Int -> [Int] -> Int
totalPlus n []     = n
totalPlus n (x:xs) = x + totalPlus n xs

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f n []     = n
foldr f n (x:xs) = f x (foldr f n xs)

-- data List a
--   = Nil
--   | Cons a (List a)

-- Goal : a value of type 'b'
-- Nil       ~~~> n :: b
-- Cons x xs ~~~> f :: a -> b -> b

-- Induction:   (by analogy; see the problems for this week)
--   - Goal: P
--   - base case: P(0)
--   - step case: P(n) -> P(n + 1)

--   foldr f n (1 : 2 : 3 : [])
-- = f 1 (foldr f n (2 : 3 : []))
-- = f 1 (f 2 (foldr f n (3 : [])))
-- = f 1 (f 2 (f 3 (foldr f n [])))
-- = f 1 (f 2 (f 3 n))
-- = 1 `f` (2 `f` (3 `f` n))
--   1  :  (2  :  (3  : []))

-- append

append :: [a] -> [a] -> [a]
append []     ys = ys
append (x:xs) ys = x : append xs ys

append2 :: [a] -> [a] -> [a]
append2 ys []     = ys
append2 ys (x:xs) = x : append2 ys xs

append3 :: [a] -> [a] -> [a]
append3 ys xs = foldr (\x xs -> x : xs) -- (:) -- (a -> [a] -> [a])
                      ys                       -- [a]
                      xs

append4 xs ys = foldr (:) ys xs

--   append4 [1,2] [3,4]
-- = foldr (:) [3,4] [1,2]
-- = 1 : foldr (:) [3,4] [2]
-- = 1 : 2 : foldr (:) [3,4] []
-- = 1 : 2 : [3,4]
-- = [1,2,3,4]

-- tail :: [a] -> [a]
-- tail []     = []
-- tail (x:xs) = xs
--
-- EXERCISE: How to write tail using 'foldr'?

tail :: [a] -> [a]
tail xs = snd (foldr (\x (ys,tail_ys) -> (x:ys, ys))  -- cons case
                     ([],[])               -- nil case
                     xs)

--   foldr (\x (ys, tail_ys) -> (x:ys, ys)) ([],[]) [1,2,3]
-- = (1:ys,tail_ys) where (ys,tail_ys) = foldr cons nil [2,3]
-- = (1:ys,tail_ys) where (ys,tail_ys) = (2:zs,tail_zs)
--                        (zs,tail_zs) = foldr cons nil [3]
-- = (1:ys,ys) where (ys,tail_ys) = (2:zs,zs) = (2:3:[], 3:[])
--                   (zs,tail_zs) = (3:as,as) = (3:[], [])
--                   (as,tail_as) = ([],[])
-- = (1:2:3:[], 2:3:[])


map' :: (a -> b) -> [a] -> [b]
map' f []     = []
map' f (x:xs) = f x : map' f xs

mapFromFoldr f = foldr (\x ys -> f x : ys) []

-- filter'

filter' :: (a -> Bool) -> [a] -> [a]
filter' f [] = []
filter' f (x:xs) = if f x then x : filter' f xs else filter' f xs

filterFromFoldr f = foldr (\x ys -> if f x then x : ys else ys) []


-- SUMMARY:
--   - foldr is a way of abstracting the idea of recursion on a list
--   - "nearly every" function on lists can be expressed as a foldr
--   - but it is not necessarily a good idea
--   - very related to the "Visitor Pattern" in OO languages


{- Part 4.2 : FOLDING LEFT -}

{-
          foldr f a [x1,x2,x3]
      ==  f x1 (foldr f a [x2,x3])
      ==  f x1 (f x2 (foldr f a [x3]))
      ==  f x1 (f x2 (f x3 (foldr f a [])))
      ==  f x1 (f x2 (f x3 a))
      ==  x1 `f` (x2 `f` (x3 `f` a))

what about:

          ((a `f` x1) `f` x2) `f` x3

    result = 0
    for (String a : strings) {
       result = f(result, a.length());
    }
-}

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f accumulator []     = accumulator
foldl f accumulator (x:xs) = foldl f (f accumulator x) xs

--   foldl (\xs x -> x : xs) [] [1,2,3]
-- = foldl (\xs x -> x : xs) (1 : []) [2,3]
-- = foldl (\xs x -> x : xs) (2 : 1 : []) [3]
-- = foldl (\xs x -> x : xs) (3 : 2 : 1 : []) []
-- = (3 : 2 : 1 : [])
-- = [3,2,1]

{-
   ArrayList<String> list = new ArrayList();
   for (String a : strings) {
      list.prepend(a);
   }
   return list;
-}

-- SUMMARY:
--   - foldl  is the 'accumulator' version of foldr
--   - useful for simulating stateful traversals of a list
--   - tutorial question: implement foldl in terms of foldr

{-   Part 4.3 : FOLD FOR OTHER TYPES -}

data List a = Nil | Cons a (List a)

data ListC a b = NilC | ConsC a b

-- type List a = ListC a (List a)

--  Nil        ~~> nil  :: b
--  Cons x xs  ~~> cons :: a -> b -> b

-- foldr :: (a -> b -> b) -> b -> [a] -> b'

data Tree a
  = Leaf
  | Node (Tree a) a (Tree a)
  deriving Show

-- Leaf       ~~> leaf :: b
-- Node l x r ~~> node :: b -> a -> b -> b

foldTree :: b -> (b -> a -> b -> b) -> Tree a -> b
foldTree leaf node Leaf         = leaf
foldTree leaf node (Node l x r) = node (foldTree leaf node l) x (foldTree leaf node r)

--   Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)
--   node (node leaf 1 leaf) 2 (node leaf 3 leaf)

--   add  (add  0    1 0)    2 (add  0    3 0)


sumtree = foldTree 0 (\sum_l x sum_r -> sum_l + x + sum_r)

data Expr
  = Const Int
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  deriving Show

foldExpr :: (Int -> b) -> (b -> b -> b) -> (b -> b -> b) -> (b -> b -> b) -> Expr -> b
foldExpr const add sub mul (Const i) = const i
foldExpr const add sub mul (Add e1 e2) =
   add (foldExpr const add sub mul e1) (foldExpr const add sub mul e2)
foldExpr const add sub mul (Sub e1 e2) =
   sub (foldExpr const add sub mul e1) (foldExpr const add sub mul e2)
foldExpr const add sub mul (Mul e1 e2) =
   mul (foldExpr const add sub mul e1) (foldExpr const add sub mul e2)

eval = foldExpr (\i -> i) (+) (-) (*)

printExpr = foldExpr (\i -> show i)
                     (\e1 e2 -> "(" ++ e1 ++ " + " ++ e2 ++ ")")
                     (\e1 e2 -> "(" ++ e1 ++ " - " ++ e2 ++ ")")
                     (\e1 e2 -> "(" ++ e1 ++ " * " ++ e2 ++ ")")
{- SUMMARY

   - Folds are more general than just lists
   - Generic way of describing how to "visit" values of a datatype
   - In the notes: fold for the Maybe type

   - 'reduce' in other languages
   - from functools import reduce
   - reduce(lambda x y: x + y, 0, [1,2,3,4])   -- Python
-}


{- Part 4.4 : LIST COMPREHENSIONS -}

list1 = 1 : (2 : (3 : (4 : [])))

list2 = [1,2,3,4]

list3 = [ x * x | x <- [1,2,3,4,5,6] ]

-- Python: [ x * x for x in [1,2,3,4,5,6] ]

list4 = [ x * y | x <- [1,2,3,4,5,6], y <- [1..x]]

library = [ ("Alice", "A Christmas Carol"),
            ("Bob", "Tinker Tailor Soldier Spy"),
            ("Carol", "Alice in Wonderland") ]

late = ["Alice", "Carol"]

dueBooks = [ book |
             (person, book) <- library,
             late_person <- late,
             person == late_person,
             person !! 0 == 'C']

-- (!!) : [a] -> Int -> a



-- { x | x in X, x > 5 }

-- SELECT book
-- FROM Library, Late
-- WHERE Library.Person = Late.Person
