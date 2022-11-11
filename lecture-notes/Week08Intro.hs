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

newtype Parser a = MkParser (String -> Maybe (a, String))

runParser :: Parser a -> String -> Maybe (a, String)
runParser (MkParser p) = p


-- Monad: 'return' and '>>='

-- 'orElse', and 'fail'

-- char

instance Monad Parser where
  return x = MkParser (\input -> Just (x, input))

  MkParser p >>= k =
    MkParser (\input ->
                case p input of
                  Nothing -> Nothing
                  Just (a, input') ->
                    case k a of
                      MkParser p2 ->
                        p2 input')

instance Applicative Parser where
  pure = return
  pf <*> pa = do f <- pf; a <- pa; return (f a)

instance Functor Parser where
  fmap f p = pure f <*> p

orElse :: Parser a -> Parser a -> Parser a
orElse (MkParser p1) (MkParser p2) =
  MkParser (\input ->
              case p1 input of
                Nothing -> p2 input
                Just (a,input') -> Just (a,input'))

failParse :: Parser a
failParse = MkParser (\input -> Nothing)

-- p `orElse` fail == fail `orElse` p == p

char :: Parser Char
char = MkParser (\input -> case input of
                             c:input' -> Just (c, input')
                             []       -> Nothing)

------------------------------------------------------------------------------

isChar :: Char -> Parser ()
isChar expected =
  do got <- char
     if got == expected then return () else failParse

parseTrue :: Parser Bool
parseTrue =
  do isString "True"
     return True

parseFalse :: Parser Bool
parseFalse =
  do isString "False"
     return False

isString :: String -> Parser ()
isString expected = for_ expected (\c -> isChar c)

parseBoolean :: Parser Bool
parseBoolean = parseTrue `orElse` parseFalse

listOfBooleans :: Parser [Bool]
listOfBooleans =
  (do b <- parseBoolean
      isString ","
      bs <- listOfBooleans
      return (b:bs))
  `orElse`
  (do b <- parseBoolean
      return [b])

sepBy1 :: Parser () -> Parser a -> Parser [a]
sepBy1 pSep pValue =
  (do b <- pValue
      pSep
      bs <- sepBy pSep pValue
      return (b:bs))
  `orElse`
  (do b <- pValue
      return [b])

sepBy :: Parser () -> Parser a -> Parser [a]
sepBy pSep pValue = sepBy1 pSep pValue `orElse` return []

-- JSON : JavaScript Object Notation

data JSON
  = Number Int      -- 12, 45645, 0
  | Bool Bool       -- true, false
  | Null            -- null
  | String String   -- "hello \"world\""    "\"hello \\\"world\\\"\""
  | Array [JSON]    -- [ 1, true, null, "a", [1,2,3] ]
  | Object [(String,JSON)]  -- { "hello": [1,2], "field": null }
  deriving Show


parseJSONBool :: Parser Bool
parseJSONBool =
  do isString "true"
     return True
  `orElse`
  do isString "false"
     return False

parseJSONNull :: Parser ()
parseJSONNull =
  isString "null"


parseStringChar :: Parser Char
parseStringChar =
  do c <- char
     case c of
       '"'  -> failParse
       '\\' -> do c <- char
                  return c
       c    -> return c

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p =
  do x <- p
     xs <- zeroOrMore p
     return (x:xs)
  `orElse`
  return []

parseString :: Parser String
parseString = do
  isChar '"'
  content <- zeroOrMore parseStringChar
  isChar '"'
  return content

parseArray :: Parser a -> Parser [a]
parseArray pValue =
  do isChar '['
     items <- sepBy (isChar ',') pValue
     isChar ']'
     return items

parseObject :: Parser a -> Parser [(String,a)]
parseObject pValue =
  do isChar '{'
     items <- sepBy (isChar ',') (do fieldname <- parseString
                                     isChar ':'
                                     value <- pValue
                                     return (fieldname, value))
     isChar '}'
     return items

parseJSON :: Parser JSON
parseJSON =
  do b <- parseJSONBool
     return (Bool b)
  `orElse`
  do parseJSONNull
     return Null
  `orElse`
  do str <- parseString
     return (String str)
  `orElse`
  do items <- parseArray parseJSON
     return (Array items)
  `orElse`
  do items <- parseObject parseJSON
     return (Object items)

testInput = "{\"a\":true,\"b\":[false,null,\"c\"]}"

-- Now we should be able to parse JSON:
--
--   > runParser parseJSON testInput
--   Just (Object [("a",Bool True),("b",Array [Bool False,Null,String "c"])],"")
