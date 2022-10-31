{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Week07Problems where

import Prelude hiding ( Monad (..)
                      , Applicative (..)
                      , mapM
                      , mapM_
                      , (<$>))
import Week07 hiding (search, lookupAll, ifThenElse, (>>))

{- This is needed due to the RebindableSyntax extension. I'm using this
   extension so the 'do' notation in this file uses my redefined
   'Monad' type class, not the standard library one. RebindableSyntax
   lets the user redefine what 'do', and 'if' mean. I've given 'if'
   the standard meaning here: -}
ifThenElse True  x y = x
ifThenElse False x y = y
(>>) x y = x >>= \_ -> y

{------------------------------------------------------------------------------}
{- TUTORIAL QUESTIONS                                                         -}
{------------------------------------------------------------------------------}

{- 1. The 'Maybe' monad is useful for simulating exceptions. But when an
      exception is thrown, we don't get any information on what the
      exceptional condition was! The way to fix this is to use a type
      that includes some information on the 'Error' case: -}

data Result a
  = Ok a
  | Error String
  deriving Show

{-    Write a Monad instance for 'Result', using the code from your
      'returnOk' and 'ifOK' functions from last week, and then use it
      to rewrite the 'search' and 'lookupAll' functions. -}


{- 2. Write a function using the Printing monad and 'do' notation that
      "prints out" all the strings in a tree of 'String's: -}

printTree :: Tree String -> Printing ()
printTree = undefined


{- 3. The implementation of 'sumImp' in the notes can only sum up lists
      of 'Int's.

      (a) What changes would you have to make to 'State' so that you
          can add up lists of 'Double's? You'll have to make a new
          newtype like 'State', and reimplement the 'runState', the
          'Monad' instance, the 'get' and 'put' function, and finally
          the 'sumpImp' function. The changes to the actual code will
          be minimal, if anything. All the changes are in the types. -}




{-    (b) Make an alternative version of 'State' that is parameterised
          by the type of the state (so that someone using it can
          decide whether it is 'Int' or 'Double' for instance). -}


{- 4. Write a function like mapM that works on 'Tree's instead of lists: -}

mapTreeM :: Monad m => (a -> m b) -> Tree a -> m (Tree b)
mapTreeM = undefined


{- 5. Write a function like mapM that works on 'Maybe's instead of lists: -}

mapMaybeM :: Monad m => (a -> m b) -> Maybe a -> m (Maybe b)
mapMaybeM = undefined
