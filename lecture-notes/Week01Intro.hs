{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Week01Intro where

{-    WELCOME TO

        CS316

          FUNCTIONAL PROGRAMMING
-}

























{- In this course, you will:

     - Learn more about Functional Programming (in Haskell)



   (Typed) Functional Programming is

     - Defining Datatypes To Represent Problems

     - Defining Functions To Create New Data From Old

   a.k.a "Value-oriented" programming.

   A "Functional Programming Language" is a programming language that
   is designed to make it easy to use Functional Programming ideas. -}













{- We use Haskell as an example Functional Programming Language.

     - Many languages now include ideas originally from Functional Programming.

        - Functions as values (a.k.a "lambdas")

        - "Algebraic" data types

        - Immutability

        - Expressive Types

        - Errors as data, instead of Exceptions

        - No 'null' (the "Billion dollar mistake")

        - Close tracking of possible "side effects"

   Haskell is not perfect (I will grumble about it during the course
   [*]), but it does offer a place to learn about Functional
   Programming concepts without too many distractions.

   [*] "There are only two kinds of languages: the ones people
       complain about and the ones nobody uses.”  ― Bjarne Stroustrup,
       The C++ Programming Language
-}







{- Course arrangements:

   - In-person
     Tuesdays at 11:00 : Lecture in MC301
     Fridays  at 11:00 : Lecture in JA314

     Mondays  at 14:00-16:00 : Labs in Level 12 of Livingstone Tower
     Tuesdays at 14:00-16:00 : Labs in Level 13 of Livingstone Tower

   - Holes:
     - No labs on Monday 26th (Glasgow Holiday)
     - No lecture on Tuesday 27th

   - Video lectures, to support the in-person lectures
     - ~ 6 videos / week
     - ~ 10 minutes long

   - Online lecture notes in a GitHub repository
     - git clone https://github.com/bobatkey/CS316-2022.git
     - git pull

-}


{- This is a programming course

   You will be expected to do a lot of programming in order to understand
   the concepts.

   20 credit course : 12 hrs/week, 1 hour of videos, 2 of lectures, 2 labs.
-}















{- YOU WILL NEED A WORKING HASKELL INSTALLATION

   - Suggested setup:

       - Stack + VSCode + Haskell extension.

       - I use Emacs in the videos and lectures.

   - There are instructions on MyPlace

   - I (unfortunately) cannot test on Windows, so I will need the
     class's help to iron out Windows problems.

-}









{- Assessment:

   - One class test (24 hrs) (50%)
        Week 6

   - Redemption test
        Week 9
        A second chance to do the test

   - One large coursework "mini-project" (50%)
        Specification released Week 3 (Monday 3rd October)
        Submission Week 11 (Monday 28th November)

-}




{- Some actual content: -}

data Suit
  = Heart
  | Club
  | Spade
  | Diamond
  | Trowel
  deriving (Show, Eq)

data Colour = Red | Black
  deriving Show

colourOfSuit :: Suit -> Colour
colourOfSuit Heart   = Red
colourOfSuit Club    = Black
colourOfSuit Spade   = Black
colourOfSuit Diamond = Red
colourOfSuit Trowel  = Black


data Value
  = Ace | N2 | N3 | N4 | N5 | N6 | N7 | N8 | N9 | N10 | Jack | Queen | King
  deriving Show


numericValue :: Value -> Integer
numericValue Ace = 1
numericValue N2 = 2
numericValue N3 = 3
numericValue N4 = 4
numericValue N5 = 5
numericValue N6 = 6
numericValue N7 = 7
numericValue N8 = 8
numericValue N9 = 9
numericValue N10 = 10
numericValue Jack = 11
numericValue Queen = 12
numericValue King = 13


greaterValue :: Value -> Value -> Bool
greaterValue v1 v2 = numericValue v1 > numericValue v2



data Card = MkCard Suit Value
         -- Card Suit Value
  deriving Show

getSuit :: Card -> Suit
getSuit (MkCard suit value) = suit

getValue :: Card -> Value
getValue (MkCard suit value) = value

numericOfSuit :: Suit -> Integer
numericOfSuit Heart = 1
numericOfSuit Spade = 2
numericOfSuit Club = 3
numericOfSuit Diamond = 4
numericOfSuit Trowel = 5

greaterSuit :: Suit -> Suit -> Bool
greaterSuit s1 s2 = numericOfSuit s1 > numericOfSuit s2

-- instance Ord Suit where
--   (>) = greaterSuit

greaterCard :: Card -> Card -> Bool
-- greaterCard (MkCard suit1 value1) (MkCard suit1 value2) =
greaterCard (MkCard suit1 value1) (MkCard suit2 value2) =
  if suit1 == suit2 then greaterValue value1 value2
                    else greaterSuit suit1 suit2
