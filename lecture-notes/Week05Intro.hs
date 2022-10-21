module Week05Intro where

import Prelude hiding (Left, Right, Semigroup (..), Foldable (..), Functor (..), Monoid (..), Maybe (..))
import Data.Char (toUpper)



{-   WEEK 05 : CLASSES OF TYPES -}

--- REMINDER : Test next Wednesday / Thursday
--- REMINDER : Coursework has been released; start now to avoid stress






{-    Part 01 : Type Design -}


-- Haskell takes types very, very seriously


-- Playing Cards
data Suit
  = Hearts
  | Diamonds
  | Clubs
  | Spades
  deriving (Show, Eq)

{-
static int hearts = 0;
static int diamonds = 1;
static int clubs = 2;
static int spades = 3;

#define CLUBS 2

typedef enum {
  hearts, diamonds, clubs, spades
     } suit;

switch (x) {
  case hearts: sdfsdf break;
  case diamonds: skdjh break;
  case clubs: .. break;
  case spades: .. break;
};

-}

-- Student records

{- a student is (represented by)

   - a name
   - either a ds username or a registration number, or both
-}

{-
  public class Student {
    @Nonnull
    private String name;

    // Please let (at least) one of these be non-null
    private String dsUsername;
    private String regNumber;

    ...
  }
-}

-- Type synonyms vs data type declarations
type DSUsername = String
newtype RegNumber  = MkRegNumber String
  deriving (Eq,Show)


-- (Char Char, Char, Char, Char, Char, Char, Char, Char)

-- typedef char* username;

data StudentDetails
  = DSUsername   DSUsername
  | RegNumber    RegNumber
  | BothDSAndReg DSUsername RegNumber
  deriving (Eq, Show)

{- DSUsername is not a type -}

{- "Make Illegal States Unrepresentable"


   "The Billion-dollar mistake" -- Sir Tony Hoare on the null pointer
-}

{-
data Maybe a
  = Nothing
  | Just a

-- Java's Optional class
-- Rust, Swift, others. Result type
-}




{-    Part 02 : Type classes -}


-- what does deriving (Eq, Show) mean?

-- Monomorphic            : total :: [Int] -> Int
-- Polymorphic / Generics : length :: [a] -> Int

-- Ad hoc polymorphism    : (==) :: String -> String -> Bool
--                          (==) :: Int -> Int -> Bool
--                          (==) :: Double -> Double -> Bool
--                          (==) :: (String -> Bool) -> (String -> Bool) -> Bool -- none such

-- C-style: specific functions: strcmp, etc.

-- Java-style, "OO"-style: x.equals(y), x.toString()
--                         x.compare(y) -- analogous to the Ord typeclass

-- show :: String -> String
-- show :: Int -> String

-- typeclass : specification for a group of functions that a type may have

{- class Eq a where
     (==) :: a -> a -> Bool
     (/=) :: a -> a -> Bool
     x /= y = not (x == y)
     x == y = not (x /= y)
-}

newtype CaseInsenstiveString =
  MkCIString String
--   deriving (Eq)

instance Eq CaseInsenstiveString where
  MkCIString x == MkCIString y =
    map toUpper x == map toUpper y

-- Type classes:
--  - Allow overloading of names (e.g., (==))
--  - Allow us to define custom behaviours based on the types
--  - Codify certain patterns of behaviour as interfaces






{-    Part 03 : Semigroups and Monoids -}

--              Addable        AddableWithAZero

-- Typeclass design:
--   - A type is an X if it supports functions A, B, C ...
--   - How do we choose what things to call interfaces?
--   - Can be tricky to find coherent abstractions

class Semigroup a where
  (<>) :: a -> a -> a

instance Semigroup Int where
  x <> y = x + y

instance Semigroup Bool where
  x <> y = x && y

instance Semigroup [a] where
  x <> y = x ++ y

data Maybe a = Nothing | Just a deriving Show

-- Take the first
instance Semigroup (Maybe a) where
  Nothing <> x = x
  Just a  <> _ = Just a

-- Combine the successes
{-
instance Semigroup a => Semigroup (Maybe a) where
  Nothing <> x = x
  x <> Nothing = x
  Just a <> Just b = Just (a <> b)
-}

-- Law of a semigroup:
--    a <> (b <> c) = (a <> b) <> c   -- associativity

--    a <> b <> c <> d
--    a <> (b <> (c <> d))  -- sequential
--    (a <> b) <> (c <> d)  -- parallel

--    a <> b = b <> a  --- NOT REQUIRED commutativity

class Semigroup a => Monoid a where
  mempty :: a

-- mempty <> x = x
-- x <> mempty = x

instance Monoid Int where
  mempty = 0

instance Monoid Bool where
  mempty = True

instance Monoid [a] where
  mempty = []

instance Monoid (Maybe a) where
  mempty = Nothing


{-    Part 04 : Foldable -}

crush :: Monoid a => [a] -> a
crush []     = mempty
crush (x:xs) = x <> crush xs

data Tree a
  = Leaf
  | Node (Tree a) a (Tree a)
  deriving Show

crushTree :: Monoid a => Tree a -> a
crushTree Leaf         = mempty
crushTree (Node l x r) = crushTree l <> x <> crushTree r

class Foldable c where
  fold :: Monoid a => c a -> a

instance Foldable [] where
  fold = crush

instance Foldable Tree where
  fold = crushTree

{-    Part 05 : Functor / Mappable -}

-- addLengths :: [String] -> Int
-- addLengths = fold . map length

class Functor c where
  fmap :: (a -> b) -> c a -> c b

forall :: (Foldable c, Functor c) => (a -> Bool) -> c a -> Bool
forall p = fold . fmap p
    --   = \x -> fold (fmap p x)

-- f . g = \x -> f (g x)


{- combining fmap and fold covers a broad range of programming problems.

   with a bit more work it is amenable to parallelism

   lifts the level of abstraction in programming
-}
