{-# LANGUAGE ParallelListComp #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Week04Solutions where

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

{- Write this function as a 'foldr': -}

listIdentity' :: [a] -> [a]
listIdentity' = foldr (\x r -> x : r) -- step case, combines the head and the tail using ':'
                      []                -- base case, the empty list []

{- See how the base case is the same as the first clause in the original
   definition of 'listIdentity'. The step case is the same as the
   second clause, except that the recursive call 'listIdentity xs' has
   been replaced by 'r'.

   We can also shorten this to:

     listIdentity' = foldr (:) []

   because '(:)' is the same thing as '(\x r -> x : r)': any infix
   operation, like ':' can be written as a function that takes two
   arguments by putting it in brackets.

   Let's see how this works by writing out the steps on a short list:

     foldr (\x r -> x:r) [] [1,2]
   = (\x r -> x:r) 1 (foldr (\x r -> x:r) [] [2])
   = (\x r -> x:r) 1 ((\x r -> x:r) 2 (foldr (\x r -> x:r) [] []))
   = (\x r -> x:r) 1 ((\x r -> x:r) 2 [])
   = (\x r -> x:r) 1 (2:[])
   = 1:2:[]
   = [1,2]
-}

{- 2. The following recursive function does a map and a filter at the
      same time. If the function argument sends an element to
      'Nothing' it is discarded, and if it sends it to 'Just b' then
      'b' is placed in the output list. -}

mapFilter :: (a -> Maybe b) -> [a] -> [b]
mapFilter f [] = []
mapFilter f (x:xs) = case f x of
                       Nothing -> mapFilter f xs
                       Just b  -> b : mapFilter f xs

{- Write this function as a 'foldr': -}

mapFilter' :: (a -> Maybe b) -> [a] -> [b]
mapFilter' f = foldr (\x r -> case f x of Nothing -> r         -- \___  step case
                                          Just b  -> b : r)    -- /
                     []     -- base case

{- The base case is the same as for 'listIdentity'' above.

   In the step case, we have to decide whether or not to add the new
   element to the list. We 'case' on the result of 'f x'. If it is
   'Nothing', we return 'r' (which is representing the recursive call
   'mapFilter f xs'). If it is 'Just b', we put 'b' on the front of
   'r' (compare the 'listIdentity' function above). -}


{- 3. Above we saw that 'foldl' and 'foldr' in general give different
      answers. However, it is possible to define 'foldl' just by using
      'foldr'.

      First try to define a function that is the same as 'foldl',
      using 'foldr', 'reverse' and a '\' function: -}

{- The key thing to notice is that the difference between 'foldl' and
   'foldr' is that 'foldl' goes left-to-right and 'foldr' goes right
   to left. So it makes sense to reverse the input list. The function
   argument 'f' then takes its arguments in the wrong order, so we
   flip them using a little '\' function. -}

foldlFromFoldrAndReverse :: (b -> a -> b) -> b -> [a] -> b
foldlFromFoldrAndReverse f x xs = foldr (\a b -> f b a) x (reverse xs)

{-  We could have also used the 'flip' function from last week's
    questions, which is provided by the standard library: -}

foldlFromFoldrAndReverse_v2 :: (b -> a -> b) -> b -> [a] -> b
foldlFromFoldrAndReverse_v2 f x xs = foldr (flip f) x (reverse xs)

{-   Much harder: define 'foldl' just using 'foldr' and a '\' function: -}

{-
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f a []     = a
foldl f a (x:xs) = foldl f (f a x) xs
-}

-- This is quite a bit more complex than the other solution using
-- 'reverse'. The key idea is to construct a "transformer" function
-- with 'foldr' that acts like 'foldl' would. Try writing out some
-- steps of this function with some example 'f's to see how it works.

foldlFromFoldr :: (b -> a -> b) -> b -> [a] -> b
foldlFromFoldr f a xs = foldr (\a g b -> g (f b a)) id xs a

{- Remember from Week03 that 'id' is '\x -> x': the function that just
   returns its argument. -}

{- Understanding 'foldlFromFoldr' may take a bit of work. The key point
   is that we use 'foldr' to build a /function/ from the input list
   'xs' that will compute the left fold from any given initial value.

   In more detail, the 'foldr' is used to build a function that takes
   an accumulator argument, similar to the 'fastReverse' function in
   Week01:

     - The 'id' is the base case: it takes the accumulator and returns
       it (compare the first clause of 'foldl', which returns 'a').

     - The '\a g b -> g (f b a)' is the step case:

       - 'a' is the value from the input list
       - 'g' is the result of processing the rest of the list, which a
         /function/ that is expecting an accumulator.
       - 'b' is the accumulator so far.

       So this function combines the value 'a' and the accumulator 'b'
       using 'f', and passes that to 'g'.

   So it is doing a 'fastReverse' and a 'foldr' at the same time (with
   the flipped arguments to 'f'), so can be seen as an optimised
   version of the first solution.

   It may be helpful to understand the /types/ involved. We are
   writing a function with this type (the type of 'foldl'):

     (b -> a -> b) -> b -> [a] -> b

   and 'foldr' has this generic type:

     (a -> b -> b) -> b -> [a] -> b

   but we are *using* 'foldr' with this type:

     foldr :: (a -> (b -> b) -> (b -> b)) -> -- 'step case'
              (b -> b) ->                    -- 'base case'
              [a] ->                         -- 'input list'
              b ->                           -- 'initial accumulator'
              b                              -- result

   Note that the 'step case' takes a function and returns a function:
   we are building a /function/ by recursion.

   Don't worry if you don't get this at the first few attempts. It
   takes some time to rewrite your mind to see functions as something
   that can be built incrementally by other functions! Looking at the
   types is usually a good way to not get lost. -}



{- 4. The following is a datatype of Natural Numbers (whole numbers
      greater than or equal to zero), represented in unary. A natural
      number 'n' is represented as 'n' applications of 'Succ' to
      'Zero'. So '2' is 'Succ (Succ Zero)'. Using the same recipe we
      used above for 'Tree's and 'Maybe's, work out the type and
      implementation of a 'fold' function for 'Nat's. -}

data Nat
  = Zero          -- a bit like []
  | Succ Nat      -- a bit like x : xs, but without the 'x'
  deriving Show

foldNat :: (b -> b) -> b -> Nat -> b
foldNat succ zero Zero     = zero
foldNat succ zero (Succ n) = succ (foldNat succ zero n)

{- HINT: think about proofs by induction. A proof by induction has a
   base case and a step case. -}

{- Here we have 'zero' for the base case, 'succ' for the step case.

   As an example, we can define 'add' for 'Nat' in terms of 'foldNat',
   which has a similar structure to 'append' for lists: -}

add :: Nat -> Nat -> Nat
add x y  = foldNat Succ y x

{- 5. Write a list comprehension to generate all the cubes (x*x*x) of
      the numbers 1 to 10: -}

cubes :: [Int]
cubes = [ x*x*x | x <- [1..10] ]


{- 6. The replicate function copies a single value a fixed number of
      times:

         > replicate 5 'x'
         "xxxxx"

      Write a version of replicate using a list comprehension: -}

replicate' :: Int -> a -> [a]
replicate' n a = [ a | _ <- [1..n]]

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
sumAndLen = foldr (\x (sum, len) -> (x + sum, len + 1)) (0,0)

-- NOTE: The solution combines the functions used in 'sumDoubles' and
-- 'lenList' by making it take a pair '(sum,len)' as well as the list
-- element 'x'. It then adds 'x' to the 'sum' part and '1' to the
-- 'len' part.

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
mapTree f = foldTree Leaf                        -- Leaf case: 'Leaf's become 'Leaf's
                     (\l x r -> Node l (f x) r)  -- Node case: 'Node's become 'Node's, but with the data changed

{- Here is the explicitly recursive version of 'mapTree', for
   reference: -}

mapTree0 :: (a -> b) -> Tree a -> Tree b
mapTree0 f Leaf           = Leaf
mapTree0 f (Node lt x rt) = Node (mapTree0 f lt) (f x) (mapTree0 f rt)


{- 9. Finally, use 'foldTree' to flatten a tree to list in left-to-right
   order: -}

flatten :: Tree a -> [a]
flatten = foldTree []                        -- Leaf case: has no elements, so is the empty list
                   (\l x r -> l ++ [x] ++ r) -- Node case: append the left, middle, and right together
