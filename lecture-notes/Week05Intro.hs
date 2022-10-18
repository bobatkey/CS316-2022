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

newtype CaseInsenstiveString =
  MkCISString String
--   deriving (Eq)

instance Eq CaseInsenstiveString where
  MkCISString x == MkCISString y =
    map toUpper x == map toUpper y

-- Type classes:
--  - Allow overloading of names (e.g., (==))
--  - Allow us to define custom behaviours based on the types
--  - Codify certain patterns of behaviour as interfaces


{-    Part 03 : Semigroups and Monoids -}
