{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE InstanceSigs #-}
module Week08Intro where

import Data.Char          (toUpper, isDigit, digitToInt, isSpace)
import Data.Foldable      (for_)
import Data.IORef         (IORef, newIORef, readIORef,
                           writeIORef, modifyIORef)
import Control.Exception  (finally)
import System.IO          (openFile, hPutChar, hGetChar,
                           hClose, IOMode (..), hIsEOF, Handle)

{-    WEEK 8 : REAL I/O and PARSER COMBINATORS -}



{-    Part 8.1 : I/O Conceptually


   A great philosopher once wrote:

     The philosophers have only interpreted the world, in various
     ways. The point, however, is to change it.

      -- Karl Marx ( https://en.wikipedia.org/wiki/Theses_on_Feuerbach )
-}

-- Equational reasoning in the presence of IO

--  putChar :: Char -> IO ()
--  void putc(char c)  -- C

-- f x = (putChar x, putChar x)

-- f x = (y,y)
--     where y = putChar x

-- f x = let y = putChar x in
--       (y,y)

-- IO a


-- IOAction a

data IOAction a
  = Return a
  | PutChar Char (IOAction a)
  | GetChar (Char -> IOAction a)
-- Conceptually, this is what 'IO a' looks like
-- except that there are many, many more things that IO can do

getChar' = GetChar (\c -> Return c)

-- IO a

{-   Part 8.2 : Doing I/O in Haskell -}

-- putChar :: Char -> IO ()
-- getChar :: IO Char

putLine :: String -> IO ()
putLine str = do for_ str (\c -> putChar c)
                 putChar '\n'

readLine :: IO String
readLine = do c <- getChar
              if c == '\n' then return ""
                else do cs <- readLine
                        return (c:cs)

{-   Part 8.5 : Parser Combinators -}

-- What is parsing?

-- - Turning a sequence of bytes into structured representation

-- - {"a\"a\"" : 1, "b": true, "c": 5}

-- Sequence of bytes (0-255)
-- Encoded in the bytes are unicode codepoint (0-2^21-1)
-- We don't need to worry about this. Haskell (and most other languages will decode automatically)

type Parser_v1 a = String -> Maybe a

-- parsing booleans
parseBool_v1 :: Parser_v1 Bool
parseBool_v1 "True"  = Just True
parseBool_v1 "False" = Just False
parseBool_v1 _       = Nothing

-- True,False

newtype Parser a = MkParser (String -> Maybe (a, String))

runParser :: Parser a -> String -> Maybe (a, String)
runParser (MkParser p) = p

-- parsing booleans
parseBool :: Parser Bool
parseBool = MkParser (\input -> case input of
                         'T':'r':'u':'e':rest     -> Just (True, rest)
                         'F':'a':'l':'s':'e':rest -> Just (False, rest)
                         _                        -> Nothing)

-- parsePair
parsePair :: Parser a -> Parser b -> Parser (a,b)
parsePair (MkParser pa) (MkParser pb) =
  MkParser (\input -> case pa input of
                        Nothing -> Nothing
                        Just (a, rest) ->
                          case pb rest of
                            Nothing -> Nothing
                            Just (b, rest2) ->
                              Just ((a,b), rest2))

parseComma :: Parser ()
parseComma = MkParser (\input -> case input of
                                   ',':rest -> Just ((), rest)
                                   _        -> Nothing)

-- Monad

-- Using the monad

-- char
