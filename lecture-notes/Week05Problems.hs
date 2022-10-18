module Week05Problems where

import Data.Foldable
import Data.Monoid
import Data.Bits (FiniteBits(countLeadingZeros))

{------------------------------------------------------------------------------}
{- TUTORIAL QUESTIONS                                                         -}
{------------------------------------------------------------------------------}

{- 1. Define a 'Show' instance for the following datatype that prints
      out the data in a JSON-like format. For example,

         show (MkHost "www.cis.strath.ac.uk" 80) == "{\"name\":\"www.cis.strath.ac.uk\", \"port\": 80}"

      The backslashes before the '"'s in the string are "escape
      characters". They are there so that Haskell knows not to end the
      string at this point.
-}

data Host = MkHost String Int

instance Show Host where
  show = undefined



{- 2. Define an 'Eq' instance for the following datatype that makes two
      numbers equal if they have the same remainder after division by
      12 (use the 'mod' function to get remainders: '14 `mod` 12 ==
      2). -}

newtype ClockHour = MkClockHour Int

instance Eq ClockHour where
  x == y = undefined

{- You should have:

     > (MkClockHour 2) == (MkClockHour 2)
     True

     > (MkClockHour 2) == (MkClockHour 14)
     True

     > (MkClockHour 2) == (MkClockHour 13)
     False

     > (MkClockHour 1) == (MkClockHour 2)
     False
-}



{- 3. Define Semigroup and Monoid instances for the following data type
      for rough counting: -}

data RoughCount
  = Zero
  | One
  | Many
  deriving (Eq, Show)

{- So that:

   - 'Zero' combined with 'x' gives 'x'
   - 'One' combined with 'One' is Many, and
   - 'Many' combined with anything is 'Many'.

   What is the 'mempty' that does nothing? -}

instance Semigroup RoughCount where
  x <> y = undefined

instance Monoid RoughCount where
  mempty = undefined



{- 4. Define Semigroup and Monoid instances for the 'Tree a' data type,
      under the assumption that the type 'a' of data stored in the
      tree is a Semigroup. -}

data Tree a
  = Leaf
  | Node (Tree a) a (Tree a)
  deriving Show

{- The semigroup operation '<>' should merge trees. The rules of
   combination are as follows:

   - A leaf combined with any tree 't' is just 't'.

   - Combining a 'Node l1 x1 r1' and a 'Node l2 x2 r2' results in a
     'Node' with:

      - Left sub-tree from combining 'l1' and 'l2'
      - Data from combining 'x1' and 'x2'
      - Right sub-tree from combining 'r1' and 'r2'

   The notation 'Semigroup a =>' tells Haskell that we are assuming
   that the type 'a' is an instance of Semigroup, just as it does in
   function types. -}

instance Semigroup a => Semigroup (Tree a) where
  x <> y = undefined

{- What is the 'Tree' that combines to no effect by the above rules? -}

instance Semigroup a => Monoid (Tree a) where
  mempty = undefined



{- 5. Define Semigroup and Monoid instances for the following datatype. -}

newtype Fun a = MkFun (a -> a)

unFun :: Fun a -> (a -> a)
unFun (MkFun f) = f

instance Semigroup (Fun a) where
  MkFun f <> MkFun g = undefined

instance Monoid (Fun a) where
  mempty = undefined

{- HINT: Think about composition from Week 03. There are /two/ different
   right answers for the Semigroup part.

   To make it a Monoid, What is the function that has no effect when
   composed with another?

   You should have:

     unFun (MkFun reverse <> MkFun reverse) [1,2,3] == [1,2,3]

     unFun (MkFun reverse <> MkFun id) [1,2,3]      == [3,2,1]

     unFun (MkFun (+1) <> MkFun (+2)) 0             == 3
-}



{- 6. Define Semigroup and Monoid instances for the following datatype. -}

newtype MaybeFun a = MkMaybeFun (a -> Maybe a)

unMaybeFun :: MaybeFun a -> a -> Maybe a
unMaybeFun (MkMaybeFun f) = f

instance Semigroup (MaybeFun a) where
  MkMaybeFun f <> MkMaybeFun g = undefined

instance Monoid (MaybeFun a) where
  mempty = undefined

{- HINT: For this one, you'll need to define your own composition of
   functions that may fail, using a 'case'.

   You should have:

     unMaybeFun (MkMaybeFun (\_ -> Nothing) <> MkMaybeFun (\x -> Just x)) 1 == Nothing

     unMaybeFun (MkMaybeFun (\x -> Just x) <> MkMaybeFun (\x -> Just x)) 1  == Just 1
-}





{- 7. The 'OneTwoOrThree' type can be used to represent when we have
      either one, two, or three things: -}

data OneTwoOrThree a
  = One_ a
  | Two a a
  | Three a a a
  deriving Show

{-    (a) Define a Functor instance for the OneTwoOrThree type: -}

instance Functor OneTwoOrThree where
   fmap = undefined

{-    You should have:

        fmap (+1) (Three 1 2 3) == Three 2 3 4
-}

{-    (b) Define a Foldable instance for the OneTwoOrThree type. We will
          use the standard library Foldable, which requires that we
          define 'foldMap' as well. We use the definition in terms of
          'fmap' and 'fold' from Part 5.5 of the notes:
-}

instance Foldable OneTwoOrThree where
  foldMap f = fold . fmap f

  fold = undefined

{- The following ought to work:

      fold (Three [1,2] [3,4] [5,6]) == [1,2,3,4,5,6]
-}


{- 8. Define a function of the type:

        toList :: (Functor c, Foldable c) => c a -> [a]

      which shows that with 'Foldable' you can always define a
      'toList' function. -}

toList :: (Functor c, Foldable c) => c a -> [a]
toList = undefined

{- If you only have a 'toList' function for a container can you always
   define 'fold'? -}


{- 9. Use the 'RoughCount' monoid above to do a rough count of the
      number of 'True's in a container full of 'Bool's: -}

roughlyHowTrue :: Foldable c => c Bool -> RoughCount
roughlyHowTrue = undefined

{- HINT: use 'foldMap' with a function that converts each 'Bool' to a
   'RoughCount' that counts how 'True' it is.

   You should have:

      roughlyHowTrue [False, False, False] == Zero
      roughlyHowTrue [True, False, False]  == One
      roughlyHowTrue [False, True, False]  == One
      roughlyHowTrue [True, True, False]   == Many
      roughlyHowTrue [False, True, True]   == Many
-}


{- 10. Contrary to the notes, the standard library does not define
       Semigroup or Monoid instances for numeric types like 'Int' and
       'Double'. Instead, the Data.Monoid module (imported above)
       defines two newtypes:

newtype Product a = Product a

newtype Sum a = Sum a

      with functions 'getProduct :: Product a -> a' and
      'getSum :: Sum a -> a' that extract the values.

      When 'Num a' is true (i.e. 'a' is a numeric type), 'Product a'
      is a monoid that multiples and 'Sum a' is a monoid that adds.

      Use these functions with 'foldMap' to define generic 'sumAll'
      and 'productAll' functions for any foldable container 'c' and
      any kind of numeric type 'a':
-}

sumAll :: (Foldable c, Num a) => c a -> a
sumAll = undefined

productAll :: (Foldable c, Num a) => c a -> a
productAll = undefined

{- HINT: the trick is to think in three stages:
    1. Every 'a' in the container needs to be converted to a 'Sum a' (or 'Product a').
    2. The 'fold' then sums them, or multiplies them.
    3. We end up with a 'Product a' or 'Sum a', use the appropriate function to get back the 'a'
-}


{- 11. Use the 'Sum Int' monoid with foldMap to write a generic 'size'
       function, similar to the one in the notes. -}

sizeGeneric :: Foldable c => c a -> Int
sizeGeneric = undefined


{- 12. The standard library module contains definitions to tell Haskell
       that the type of pairs forms a Monoid if the two constituent
       types do:

instance (Monoid a, Monoid b) => Monoid (a,b) where
  ...


       Use this to write a generic 'average' function that combines
       the 'sumAll' and 'sizeGeneric' functions into one that does a
       *single* pass of the container.
-}

average :: Foldable c => c Double -> Double
average c = total / fromInteger count
  where (Sum total, Sum count) = undefined -- fill in the 'undefined'
