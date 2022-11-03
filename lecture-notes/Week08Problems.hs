{-# LANGUAGE InstanceSigs #-}
module Week08Problems where

import System.IO (Handle, openFile, IOMode (WriteMode), hClose, hPutChar)
import GHC.IO (finally)
import Data.Foldable (for_)
import Week08
import Data.List (intercalate)
import Data.Char (isAlphaNum)

{------------------------------------------------------------------------------}
{- TUTORIAL QUESTIONS                                                         -}
{------------------------------------------------------------------------------}

{- 1. (a) Write a function 'withOutputFile' that does what 'withInputFile'
          (section 8.3) does but for output. -}

withOutputFile :: FilePath -> (Handle -> IO a) -> IO a
withOutputFile = undefined

{-    (b) Use your 'withOutputFile' to write an exception safe version
          of 'writeToFile'. -}

writeFile :: FilePath -> String -> IO ()
writeFile = undefined


{- 2. Write a parser for primary colours, similar to the 'parseBool'
      function from the notes. Here is the PrimaryColour type: -}

data PrimaryColour
  = Red
  | Green
  | Blue
  deriving (Show, Eq)

parsePrimaryColour :: Parser PrimaryColour
parsePrimaryColour = undefined

{- For example,

      > runParser parsePrimaryColour "Red"
      Just ("", Red)
      > runParser parsePrimaryColour "Green"
      Just ("", Green)
      > runParser parsePrimaryColour "Blue"
      Just ("", Blue)
      > runParser parsePrimaryColour "Purple"
      Nothing
-}

{- 3. Use 'sepBy', 'isString' and 'parsePrimaryColour' to write a parser
      for comma separated lists of primary colours. -}

parseListOfPrimaryColours :: Parser [PrimaryColour]
parseListOfPrimaryColours = undefined

{- 4. Let us now make a little programming language. Expressions in this
      language follow Java-/C-style function use syntax. For example:

         f(4,5)         is           AppExp "f" [IntExp 4, IntExp 5]

         f(g(3),5)      is           AppExp "f" [AppExp "g" [IntExp 3], IntExp 5]

      The grammar is:

         <expr> ::=   <int>
                  |   <identifier> '(' ')'
                  |   <identifier> '(' <expr> (',' <expr>)* ')'

      That is, an <expr> is either:

         (a) an integer
         (b) an identifier (word without spaces) followed by "()"; or
         (c) an identifier followed by '(', then an <expr>, then zero or more commas and <expr>s, then a ')'

      Here is the datatype for expressions in this language: -}

data Expr
  = IntExp Int
  | AppExp String [Expr]
  deriving Show

{-    The following function prints out 'Expr's in the Java-/C-style
      syntax: -}

printExpr :: Expr -> String
printExpr (IntExp i) = show i
printExpr (AppExp funNm args) =
  funNm ++ "(" ++ intercalate "," (map printExpr args) ++ ")"


{-    Your task is to write a parser for 'Expr's. This will similar to
      the general structure of the JSON parser in the notes. Have a
      section of the parser for each constructor ('IntExp' and
      'AppExp'), and use the grammar above as a guide. Use the
      'number' parser from the notes to parse numbers. The
      'parseIdentifier' parser defined below will be useful for doing
      the function names. -}

parseExpr :: Parser Expr
parseExpr = undefined


parseIdentifier :: Parser String
parseIdentifier =
  do c  <- parseIdentifierChar
     cs <- zeroOrMore parseIdentifierChar
     return (c:cs)
  where
    parseIdentifierChar =
      do c <- char
         if isAlphaNum c then return c else failParse
