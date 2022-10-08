{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Week03Solutions where

import Data.Char

{------------------------------------------------------------------------------}
{- TUTORIAL QUESTIONS                                                         -}
{------------------------------------------------------------------------------}

{- 1. Lambda notation.

   Rewrite the following functions using the '\x -> e' notation (the
   "lambda" notation), so that they are written as 'double =
   <something>', and so on. -}

mulBy2 :: Int -> Int
mulBy2 = \x -> 2*x

mul :: Int -> Int -> Int
mul = \x y -> x * y

invert :: Bool -> Bool
invert = -- \x -> if x then False else True
         -- not
         \x -> case x of
                 True  -> False
                 False -> True
  {- HINT: use a 'case', or an 'if'. -}


{- 2. Partial Application

   The function 'mul' defined above has the type 'Int -> Int ->
   Int'. (a) What is the type of the Haskell expression:

       mul 10

   ANSWER: mul 10 has type 'Int -> Int'

   (b) what is 'mul 10'? How can you use it to multiply a number?

   ANSWER: 'mul 10' is a function that multiplies its argument bt
   10. You can use to do this multiplication by applying it to a
   value. So 'mul 10 20' gives the answer 200.
-}


{- 3. Partial Application

   Write the 'mulBy2' function above using 'mul'. Can you make your
   function as short as possible? -}

double_v2 :: Int -> Int
double_v2 = mul 2

{- The longer version is:

     double_v2 x = mul 2 x

   but every time you have a function definition that looks like this:

     fname x y z = <something> z

   it can be shortened to:

     fname x y = <something>
-}

{- 4. Using 'map'.

   The function 'toUpper' takes a 'Char' and turns lower case
   characters into upper cases one. All other characters it returns
   unmodified. For example:

       > toUpper 'a'
       'A'
       > toUpper 'A'
       'A'

   Strings are lists of characters. 'map' is a function that applies a
   function to every character in a list and returns a new list.

   Write the function 'shout' that uppercases a string, so that:

      > shout "hello"
      "HELLO"
-}

shout :: String -> String    -- remember that String = [Char]
shout = map toUpper


{- Longer version:

    shout xs = map toUpper xs

-}

{- 5. Using 'map' with another function.

   The function 'concat' concatenates a lists of lists to make one
   list:

      > concat [[1,2],[3,4],[5,6]]
      [1,2,3,4,5,6]

   Using 'map', 'concat', and either a helper function or a function
   written using '\', write a function 'dupAll' that duplicates every
   element in a list. For example:

      > dupAll [1,2,3]
      [1,1,2,2,3,3]
      > dupAll "my precious"
      "mmyy  pprreecciioouuss"

   HINT: try writing a helper function that turns single data values
   into two element lists. -}

dupAll :: [a] -> [a]
dupAll xs = concat (map (\x -> [x,x]) xs)


{- A shorter version is this:

    dupAll = concat . map (\x -> [x,x])

   which uses the composition operator '.'
-}


-- [1,2,3]
-- [[1,1],[2,2],[3,3]]
-- [1,1,2,2,3,3]

{- Compare this to the recursive version:

     dupAll [] = []
     dupAll (x:xs) = x : x : dupAll xs

   The difference between this and the definition using 'map' is that
   it mixes the concerns of 'duplicate an element' and 'do this to
   every element'. The version using 'map' explicitly makes clear that
   something is happening to every element of the list.

   In a small example like this, it is not immediately clear which
   version is easier, but when the amount of things we want to do to
   every element gets larger, map can often be clearer to show intent. -}



{- 6. Using 'filter'

   (a) Use 'filter' to return a list consisting of only the 'E's in
       a 'String'.

   (b) Use 'onlyEs' and 'length' to count the number of 'E's in a string.

   (c) Write a single function that takes a character 'c' and a string
       's' and counts the number of 'c's in 's'. -}

onlyEs :: String -> String
onlyEs = filter (\x -> x == 'E')

numberOfEs :: String -> Int
numberOfEs xs = length (onlyEs xs)

{- A shorter version is:

      numberOfEs = length . onlyEs
-}

numberOf :: Char -> String -> Int
numberOf c = length . filter (\x -> x == c)


{- 7. Rewriting 'filter'

   (a) Write a function that does the same thing as filter, using
      'map' and 'concat'.

   (b) Write a function that does a 'map' and a 'filter' at the same
       time, again using 'map' and 'concat'.
-}

{- This is idea of the solution below. If the predicate is "\x -> x == 'E'", then we map every 'E' to ['E'] and everything else to []. Then we concatenate all the lists.

         ['E',  'I','E',   'I','O']
   ==>   [['E'],[], ['E'], [], [] ]
   ==>   ['E',      'E'           ]
-}

filter_v2 :: (a -> Bool) -> [a] -> [a]
filter_v2 p = concat . map (\x -> if p x then [x] else [])

filterMap :: (a -> Maybe b) -> [a] -> [b]
filterMap p = concat . map (\x -> case p x of
                                    Nothing -> []
                                    Just y  -> [y])


{- 8. Composition

   Write a function '>>>' that composes two functions: takes two
   functions 'f' and 'g', and returns a function that first runs 'f'
   on its argument, and then runs 'g' on the result.

   HINT: this is similar to the function 'compose'. -}

(>>>) :: (a -> b) -> (b -> c) -> a -> c
(>>>) f g x = g (f x)

   -- NOTE: the functions 'f' and 'g' appear the other way round in
   -- the function body.

{- Try rewriting the 'numberOfEs' function from above using this one. -}

numberOfEs_v2 = filter (\x -> x == 'E') >>> length

{- 9. Backwards application

   Write a function of the following type that takes a value 'x' and a
   function 'f' and applies 'f' to 'x'. Note that this functions takes
   its arguments in reverse order to normal function application! -}

(|>) :: a -> (a -> b) -> b
(|>) x f = f x


{- This function can be used between its arguments like so:

       "HELLO" |> map toLower

   and it is useful for chaining calls left-to-right instead of
   right-to-left as is usual in Haskell:

       "EIEIO" |> onlyEs |> length
-}

{- 10. Flipping

   Write a function that takes a two argument function as an input,
   and returns a function that does the same thing, but takes its
   arguments in reverse order: -}

flip :: (a -> b -> c) -> b -> a -> c
flip f a b = f b a


{- 11. Evaluating Formulas

   Here is a datatype describing formulas in propositional logic, as
   in CS208 last year. Atomic formulas are represented as 'String's. -}

data Formula
  = Atom String
  | And  Formula Formula
  | Or   Formula Formula
  | Not  Formula
  deriving Show

{- (a) Write a function that evaluates a 'Formula' to a 'Bool'ean value,
       assuming that all the atomic formulas are given the value
       'True'. Note that the following Haskell functions do the basic
       operations on 'Bool'eans:

           (&&) :: Bool -> Bool -> Bool    -- 'AND'
           (||) :: Bool -> Bool -> Bool    -- 'OR'
           not  :: Bool -> Bool            -- 'NOT'
-}

eval_v1 :: Formula -> Bool
eval_v1 (Atom a)  = True
eval_v1 (And p q) = eval_v1 p && eval_v1 q
eval_v1 (Or p q)  = eval_v1 p || eval_v1 q
eval_v1 (Not p)   = not (eval_v1 p)




{- (b) Now write a new version of 'eval_v1' that, instead of evaluating
       every 'Atom a' to 'True', takes a function that gives a 'Bool'
       for each atomic proposition: -}

eval :: (String -> Bool) -> Formula -> Bool
eval v (Atom a)  = v a
eval v (And p q) = eval v p && eval v q
eval v (Or p q)  = eval v p || eval v q
eval v (Not p)   = not (eval v p)

{- For example:

     eval (\s -> s == "A") (Or (Atom "A") (Atom "B"))  == True
     eval (\s -> s == "A") (And (Atom "A") (Atom "B")) == False
-}


{- 12. Substituting Formulas

   Write a function that, given a function 's' that turns 'String's
   into 'Formula's (a "substitution"), replaces all the atomic
   formulas in a Formula with whatever 'f' tells it to: -}

subst :: (String -> Formula) -> Formula -> Formula
subst v (Atom a)  = v a
subst v (And p q) = subst v p `And` subst v q
subst v (Or p q)  = subst v p `Or` subst v q
subst v (Not p)   = Not (subst v p)

{- For example:

     subst (\s -> if s == "A" then Not (Atom "A") else Atom s) (And (Atom "A") (Atom "B")) == And (Not (Atom "A")) (Atom "B")
-}

{- 13. Evaluating with failure

   The 'eval' function in 8(b) assumed that every atom could be
   assigned a value. But what if it can't? Write a function of the
   following type that takes as input a function that may or may not
   give a 'Bool' for each atom, and correspondingly, may or may not
   give a 'Bool' for the whole formula. -}

evalMaybe :: (String -> Maybe Bool) -> Formula -> Maybe Bool
evalMaybe v (Atom a)  = v a
evalMaybe v (And p q) =
  case evalMaybe v p of
    Nothing -> Nothing
    Just x ->
      case evalMaybe v q of
        Nothing -> Nothing
        Just y ->
          Just (x && y)
evalMaybe v (Or p q) =
  case evalMaybe v p of
    Nothing -> Nothing
    Just x ->
      case evalMaybe v q of
        Nothing -> Nothing
        Just y ->
          Just (x || y)
evalMaybe v (Not p) =
  case evalMaybe v p of
    Nothing -> Nothing
    Just x ->
      Just (not x)

{- This is pretty complex and noisy looking, because it makes all the
   error handling explicit. On the other hand, it is easy to trace
   what will happen in all of the possible cases, including those that
   happen when there are errors. We will see ways of making it look
   nicer in Weeks 6 & 7.

   One thing to think about is why the 'Atom a' case is this:

     evalMaybe v (Atom a) = v a

   and not:

     evalMaybe v (Atom a) =
       case v a of
         Nothing -> Nothing
         Just x -> Just x

   the answer is that anything like:

     case <something> of
       Nothing -> Nothing
       Just x -> Just x

   is always equal to '<something>' -- the 'case' is returning
   'Nothing' when the value is 'Nothing' and 'Just x' when it is 'Just
   x', so in the end it does nothing to the value returned by
   '<something>' and can be removed. -}
