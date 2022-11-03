{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Week07Solutions where

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

instance Monad Result where
  return = Ok

  Ok x      >>= k = k x
  Error msg >>= k = Error msg

search :: (Show k, Eq k) => k -> [(k,v)] -> Result v
search k [] = Error ("Key '" ++ show k ++ "' not found")
search k ((k',v'):kvs) =
  if k == k' then
    return v'
  else
    search k kvs

lookupAll :: (Show k, Eq k) => [(k,v)] -> Tree k -> Result (Tree v)
lookupAll kvs Leaf =
  return Leaf
lookupAll kvs (Node l k r) =
  do l' <- lookupAll kvs l
     v  <- search k kvs
     r' <- lookupAll kvs r
     return (Node l' v r')


{- 2. Write a function using the Printing monad and 'do' notation that
      "prints out" all the strings in a tree of 'String's: -}

printTree :: Tree String -> Printing ()
printTree Leaf =
  return ()
printTree (Node l x r) =
  do printTree l
     printLine x
     printTree r


{- 3. The implementation of 'sumImp' in the notes can only sum up lists
      of 'Int's.

      (a) What changes would you have to make to 'State' so that you
          can add up lists of 'Double's? You'll have to make a new
          newtype like 'State', and reimplement the 'runState', the
          'Monad' instance, the 'get' and 'put' function, and finally
          the 'sumpImp' function. The changes to the actual code will
          be minimal, if anything. All the changes are in the types. -}

-- To do this, we modify the 'State' newtype, to change the 'Int's to
-- 'Double's. I have added the suffix 'D' for 'D'ouble.

newtype StateD a = MkStateD (Double -> (Double, a))

-- Then we write the functions again, with new types:

runStateD :: StateD a -> Double -> (Double, a)
runStateD (MkStateD t) = t

instance Monad StateD where
  return :: a -> StateD a
  return x =
    MkStateD (\s -> (s, x))

  (>>=) :: StateD a -> (a -> StateD b) -> StateD b
  op >>= f =
    MkStateD (\s ->
               let (s0, a) = runStateD op s
                   (s1, b) = runStateD (f a) s0
               in (s1, b))

getD :: StateD Double
getD = MkStateD (\s -> (s,s))

putD :: Double -> StateD ()
putD i = MkStateD (\_ -> (i,()))

sumImpD :: [Double] -> StateD Double
sumImpD xs =
  do putD 0
     for_ xs (\x -> do
       total <- getD
       putD (total + x))
     result <- getD
     return result

{-    (b) Make an alternative version of 'State' that is parameterised
          by the type of the state (so that someone using it can
          decide whether it is 'Int' or 'Double' for instance). -}

-- To do this, we add an extra parameter to the 'State' newtype, which
-- we call 's' here. I have added the suffix 'G' for 'G'eneric.

newtype StateG s a = MkStateG (s -> (s, a))

-- then we rewrite all our functions with basically the same code, but
-- more general types:

runStateG :: StateG s a -> s -> (s, a)
runStateG (MkStateG t) = t

instance Monad (StateG s) where
  return :: a -> StateG s a
  return x =
    MkStateG (\s -> (s, x))

  (>>=) :: StateG s a -> (a -> StateG s b) -> StateG s b
  op >>= f =
    MkStateG (\s ->
               let (s0, a) = runStateG op s
                   (s1, b) = runStateG (f a) s0
               in (s1, b))

getG :: StateG s s
getG = MkStateG (\s -> (s,s))

putG :: s -> StateG s ()
putG i = MkStateG (\_ -> (i,()))

sumImpG :: Monoid m => [m] -> StateG m m
sumImpG xs =
  do putG mempty
     for_ xs (\x -> do
       total <- getG
       putG (total <> x))
     result <- getG
     return result

{- 4. Write a function like mapM that works on 'Tree's instead of lists: -}

mapTreeM :: Monad m => (a -> m b) -> Tree a -> m (Tree b)
mapTreeM f Leaf = return Leaf
mapTreeM f (Node l x r) =
  do l' <- mapTreeM f l
     y  <- f x
     r' <- mapTreeM f r
     return (Node l' y r')


{- 5. Write a function like mapM that works on 'Maybe's instead of lists: -}

mapMaybeM :: Monad m => (a -> m b) -> Maybe a -> m (Maybe b)
mapMaybeM f Nothing  = return Nothing
mapMaybeM f (Just x) =
  do y <- f x
     return (Just y)
