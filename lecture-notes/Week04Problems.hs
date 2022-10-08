{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Week04Problems where

import Prelude hiding (foldr, foldl, Maybe (..), Left, Right, filter, zip, map, concat)
import Data.List.Split (splitOn)
import Data.List hiding (foldr, foldl, filter, map, concat)
import Week04

{------------------------------------------------------------------------------}
{- TUTORIAL QUESTIONS                                                         -}
{------------------------------------------------------------------------------}

{- 1. The following recursive function returns the list it is given as
      input: -}

listIdentity :: [a] -> [a]
listIdentity []     = []
listIdentity (x:xs) = x : listIdentity xs

{- Write this function as a 'foldr' (fill in the 'undefined's): -}

listIdentity' :: [a] -> [a]
listIdentity' = foldr undefined undefined

{- 2. The following recursive function does a map and a filter at the
      same time. If the function argument sends an element to
      'Nothing' it is discarded, and if it sends it to 'Just b' then
      'b' is placed in the output list. -}

mapFilter :: (a -> Maybe b) -> [a] -> [b]
mapFilter f [] = []
mapFilter f (x:xs) = case f x of
                       Nothing -> mapFilter f xs
                       Just b  -> b : mapFilter f xs

{- Write this function as a 'foldr' by replacing the 'undefined's: -}

mapFilter' :: (a -> Maybe b) -> [a] -> [b]
mapFilter' f xs = foldr undefined undefined xs



{- For example, if we define -}

decodeBinaryDigit :: Char -> Maybe Int
decodeBinaryDigit '0' = Just 0
decodeBinaryDigit '1' = Just 1
decodeBinaryDigit _   = Nothing

{-
     mapFilter' decodeBinaryDigit "a0b1c0" == [0,1,0]
-}


{- 3. Above we saw that 'foldl' and 'foldr' in general give different
      answers. However, it is possible to define 'foldl' just by using
      'foldr'.

      First try to define a function that is the same as 'foldl',
      using 'foldr', 'reverse' and a '\' function: -}

foldlFromFoldrAndReverse :: (b -> a -> b) -> b -> [a] -> b
foldlFromFoldrAndReverse f x xs = undefined

{-   Much harder: define 'foldl' just using 'foldr' and a '\' function: -}

foldlFromFoldr :: (b -> a -> b) -> b -> [a] -> b
foldlFromFoldr f x xs = undefined


{- 4. The following is a datatype of Natural Numbers (whole numbers
      greater than or equal to zero), represented in unary. A natural
      number 'n' is represented as 'n' applications of 'Succ' to
      'Zero'. So '2' is 'Succ (Succ Zero)'. Using the same recipe we
      used above for 'Tree's and 'Maybe's, work out the type and
      implementation of a 'fold' function for 'Nat's. -}

data Nat
  = Zero
  | Succ Nat
  deriving Show

{- HINT: think about proofs by induction. A proof by induction has a
   base case and a step case. -}


{- 5. Write a list comprehension to generate all the cubes (x*x*x) of
      the numbers 1 to 10: -}

cubes :: [Int]
cubes = undefined


{- 6. The replicate function copies a single value a fixed number of
      times:

         > replicate 5 'x'
         "xxxxx"

      Write a version of replicate using a list comprehension: -}

replicate' :: Int -> a -> [a]
replicate' = undefined

{- 7. One-pass Average.

   It is possible to use 'foldr' to
   implement many other interesting functions on lists. For example
   'sum' and 'len': -}

sumDoubles :: [Double] -> Double
sumDoubles = foldr (\x sum -> x + sum) 0

lenList :: [a] -> Integer
lenList = foldr (\_ l -> l + 1) 0

{- Putting these together, we can implement 'avg' to compute the average
   (mean) of a list of numbers: -}

avg :: [Double] -> Double
avg xs = sumDoubles xs / fromInteger (lenList xs)

{- Neat as this function is, it is not as efficient as it could be. It
   traverses the input list twice: once to compute the sum, and then
   again to compute the length. It would be better if we had a single
   pass that computed the sum and length simultaneously and returned a
   pair.

   Implement such a function, using foldr: -}

sumAndLen :: [Double] -> (Double, Integer)
sumAndLen = undefined

{- Once you have implemented your 'sumAndLen' function, this alternative
   average function will work: -}

avg' :: [Double] -> Double
avg' xs = total / fromInteger length
  where (total, length) = sumAndLen xs

{- 8. mapTree from foldTree

   Here is the 'Tree' datatype that is imported from the Week04 module:

data Tree a
  = Leaf
  | Node (Tree a) a (Tree a)
  deriving Show

   As we saw in the lecture notes, it is possible to write a generic
   recursor pattern for trees, similar to 'foldr', copied here for reference:

foldTree :: b -> (b -> a -> b -> b) -> Tree a -> b
foldTree l n Leaf           = l
foldTree l n (Node lt x rt) = n (foldTree l n lt) x (foldTree l n rt)

   Your job is to implement 'mapTree' (from Week03) in terms of
   'foldTree': -}

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree = undefined

{- Here is the explicitly recursive version of 'mapTree', for
   reference: -}

mapTree0 :: (a -> b) -> Tree a -> Tree b
mapTree0 f Leaf           = Leaf
mapTree0 f (Node lt x rt) = Node (mapTree0 f lt) (f x) (mapTree0 f rt)

{- 9. Finally, use 'foldTree' to flatten a tree to list in left-to-right
   order: -}

flatten :: Tree a -> [a]
flatten = undefined
