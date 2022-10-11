{-# LANGUAGE ParallelListComp #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Week04Intro where

import Prelude hiding (foldr, foldl, Maybe (..), Left, Right, filter, zip, map, concat)
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
       result += a.length();
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
