{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Week01Intro02 where

-- Recap

--- 1. Data (with types)
--- 2. Functions, defined by Pattern Matching







-- Today: defining functions by recursion





--- Example 1: factorial

-- factorial 6 = 6 * 5 * 4 * 3 * 2 * 1
--             = 6 * factorial 5

factorial :: Integer -> Integer
factorial 0 = 1
factorial 1 = 1
factorial n = n * factorial (n-1)

-- factorial 6 = 6 * factorial (6-1)
--             = 6 * factorial 5
--             = 6 * (5 * factorial (5-1))
--             = ..
--             = 6 * (5 * (4 * (3 * (2 * factorial 1))))
--             = 6 * (5 * (4 * (3 * (2 * 1))))
--             = 720







--- Example 2: Recursive types

{-
data ListofCards
  = NoCards
  | OneCard Card
  | TwoCards Card Card
  | ThreeCards Card Card Card
-}

data ListOfInts
  = NoInts
  | ConsInt Int ListOfInts
  deriving Show


---- Sum of a list of integers

sumListOfInts :: ListOfInts -> Int
sumListOfInts NoInts         = 0
sumListOfInts (ConsInt x xs) = x + sumListOfInts xs

--   sumListOfInts (ConsInt 12 (ConsInt 76 NoInts))
-- = 12 + sumListOfInts (ConsInt 76 NoInts)
-- = 12 + (76 + sumListOfInts NoInts)
-- = 12 + (76 + 0)
-- = 88


---- List a
data List a
  = Nil
  | Cons a (List a)   --- Int ListOfInts
  deriving Show

-- abstraction:
 -- double x = 2 * x
---
 -- multiply n x = n * x


---- Sum of a list of integers
sumList :: List Int -> Int
sumList Nil         = 0
sumList (Cons x xs) = x + sumList xs

---- Product of a list of integers
productList :: List Int -> Int
productList Nil         = 1
productList (Cons x xs) = x * productList xs

---- Making Lists
range :: Int -> List Int
range 0 = Nil
range n = if n < 0 then Cons n (range (n+1)) else Cons n (range (n-1))

-- range 5 = Cons 5 (Cons 4 (Cons 3 (Cons 2 (Cons 1 Nil))))


---- Factorial by combing product and 'range'
factorial_v2 :: Int -> Int
factorial_v2 n = result
  where numbersFromN = range n
        result       = productList numbersFromN

---- Actual lists
-- data [a]
--  = []          -- Nil
--  | a : [a]    -- Cons a (List a)

---- Search and replace in strings
-- type String = [Char]

-- A function to capitalise all 'e's in a String:

capitaliseEs :: String -> String
             -- [Char] -> [Char]
capitaliseEs "" = ""
capitaliseEs (c:cs) = if c == 'e' then 'E' : capitaliseEs cs else c : capitaliseEs cs

-- capitaliseEs ('e':cs) = 'E' : capitaliseEs cs
-- capitaliseEs (c:cs)   = c : capitaliseEs cs
