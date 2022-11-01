{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Week07Intro where

import Prelude hiding ( Monad (..)
                      , Applicative (..)
                      , mapM
                      , mapM_
                      , (<$>))
import Data.Char      (isDigit, digitToInt)

{- This is needed due to the RebindableSyntax extension. I'm using this
   extension so the 'do' notation in this file uses my redefined
   'Monad' type class, not the standard library one. RebindableSyntax
   lets the user redefine what 'do', and 'if' mean. I've given 'if'
   the standard meaning here: -}
ifThenElse True  x y = x
ifThenElse False x y = y
(>>) x y = x >>= \_ -> y



{------------------------------------------------------------------------------}
{-  Week 07 : MONADS                                                          -}
{------------------------------------------------------------------------------}

{- 7.1 WHAT IS A MONAD and THE MAYBE MONAD -}

-- Examples

--    f :: Int -> Int    --- does nothing but take an Int and return an Int (or not terminate)

--    f :: Int -> Maybe Int    -- this *may* throw an exception
--    f :: Int -> State Int    -- this *may* update some state
--    f :: Int -> Printing Int -- this *may* do some printing
--    f :: Int -> Process Int  -- this *may* so some I/O

-- ifOK     :: Maybe a   -> (a -> Maybe b)   -> Maybe b
-- andThen  :: State a   -> (a -> State b)   -> State b
-- sequ     :: Process a -> (a -> Process b) -> Process b

-- returnOk   :: a -> Maybe a
-- returnSt   :: a -> State a
-- returnProc :: a -> Process a

class Monad m where
  return :: a -> m a
  (>>=)  :: m a -> (a -> m b) -> m b   -- pronounced 'bind'

instance Monad Maybe where
  -- This is the same as 'returnOk' from last week
  return x = Just x

  -- This is the same as 'ifOK' from last week
  (>>=) Nothing  k = Nothing
  (>>=) (Just a) k = k a

-- search

search :: Eq k => k -> [(k,v)] -> Maybe v
search k [] = Nothing
search k ((k',v'):kvs) = if k == k' then return v' else search k kvs

lookupAll :: Eq k => [k] -> [(k,v)] -> Maybe [v]
lookupAll []     kvs = return []
lookupAll (k:ks) kvs =
  search k kvs     >>= \v ->
  lookupAll ks kvs >>= \vs ->
  return (v:vs)


{- 7.2 'do' NOTATION -}

-- this function is exactly the same as the previous 'lookupAll'
lookupAll_v2 :: Eq k => [k] -> [(k,v)] -> Maybe [v]
lookupAll_v2 []     kvs = return []
lookupAll_v2 (k:ks) kvs =
  do v <- search k kvs
     vs <- lookupAll_v2 ks kvs
     return (v:vs)


{- 7.4 STATE MONAD -}

data State a = MkState (Int -> (Int, a))

instance Monad State where
  return a = MkState (\s -> (s, a))

  (>>=) (MkState f) k =
    MkState (\s0 -> let (s1, a)   = f s0
                        MkState g = k a
                        (s2, b)   = g s1
                    in (s2, b))

get :: State Int
get = MkState (\s -> (s,s))

put :: Int -> State ()
put i = MkState (\_ -> (i, ()))

numberList :: [a] -> State [(a,Int)]
numberList [] = return []
numberList (x:xs) =
  do i <- get; put (i+1); ys <- numberList xs; return ((x,i) : ys)

-- "Overloaded semicolon"

runState :: State a -> Int -> a
runState (MkState f) s = case f s of (_, a) -> a



{- 7.5 FUNCTIONS FOR ALL MONADS -}

mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM f []     = return []
mapM f (x:xs) =
  do y <- f x
     ys <- mapM f xs
     return (y:ys)

-- forM

forM :: Monad m => [a] -> (a -> m b) -> m [b]
forM xs f = mapM f xs

lookupAll_v3 :: Eq k => [k] -> [(k,v)] -> Maybe [v]
lookupAll_v3 ks kvs =
  forM ks (\k ->
    search k kvs)

numberList_v2 :: [a] -> State [(a,Int)]
numberList_v2 xs =
  forM xs (\x -> do i <- get
                    put (i+1)
                    return (x,i))

-- lookupAll, again
