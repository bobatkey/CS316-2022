{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Week02Intro where

import Prelude hiding (Maybe, Nothing, Just)

{- Week 02; Lecture 03


      MAKING DECISIONS and DEALING WITH FAILURE

-}





-- isMember; using if-then-else
isMember :: Eq a
         => a -> [a] -> Bool
isMember y []     = False
isMember y (x:xs) = if x == y then True else isMember y xs
  -- y :: a
  -- x :: a
  -- xs :: [a]

-- We can't repeat the 'y' twice in the pattern to stand for equality

-- isMember2; using guards
isMember2 :: Eq a
         => a -> [a] -> Bool
isMember2 y []     = False
isMember2 y (x:xs)
  | x == y    = True
--  | x /= y    = isMember2 y xs    --- this by itself is not enough to guarantee completeness
  | otherwise = isMember2 y xs

isMember3 :: Eq a => a -> [a] -> Bool
isMember3 y []     = False
isMember3 y (x:xs) = (x == y) || isMember3 y xs

-- isMember4; using case
isMember4 :: Eq a => a -> [a] -> Bool
isMember4 y [] = False
isMember4 y (x:xs) =
  case x == y of
    True  -> True
    False -> isMember y xs
{-
  case x - y of
    0 -> True
    1 -> True
    _ -> isMember y xs
-}

isMember5 :: Ord a => a -> [a] -> Bool
isMember5 y [] = False
isMember5 y (x:xs) =
  case compare x y of
    EQ -> True
    LT -> isMember5 y xs
    GT -> isMember5 y xs

-- Trees
data Tree a
  = Leaf
  | Node (Tree a) a (Tree a)
  deriving Show

{-   1
    / \
   2   L
  /\
 L  L
-}

example = Node (Node Leaf 2 Leaf) 1 Leaf

sortedExample = Node Leaf 1 (Node Leaf 2 Leaf)

isTreeMember :: Ord a => a -> Tree a -> Bool
isTreeMember y (Node l x r) =
{-
  if x == y then
    True
  else if y < x then
    isTreeMember y l
  else
    isTreeMember y r
-}
  case compare y x of
    EQ -> True
    LT -> isTreeMember y l
    GT -> isTreeMember y r
isTreeMember y Leaf         = False


-- Maybe
data Maybe a
  = Nothing
  | Just a
  deriving Show

exampleKV :: Tree (String,Int)
exampleKV = Node (Node Leaf ("a",1) Leaf) ("b",2) Leaf

-- getKey
getKey :: Ord a => a -> Tree (a,b) -> Maybe b
getKey k Leaf = Nothing
getKey k (Node l (k', v) r) =
  case compare k k' of
    EQ -> Just v
    LT -> getKey k l
    GT -> getKey k r


-- dealing with Failure: reading a list of keys from a tree
getKeys :: Ord a => [a] -> Tree (a,b) -> Maybe [b]
getKeys []     tree = Just []
getKeys (k:ks) tree =
--  What we would like to write:   'getKey k tree : getKeys ks tree'
  case getKey k tree of
    Nothing ->
      Nothing
    Just v ->
      case getKeys ks tree of
        Nothing ->
          Nothing
        Just vs ->
          Just (v:vs)
