{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Week01Solutions where

import Week01
import Prelude hiding (take, drop, Left, Right, Maybe (..), reverse, length)

{----------------------------------------------------------------------}
{- Tutorial Questions                                                 -}
{----------------------------------------------------------------------}

{- In the questions below, replace 'undefined' with your answers. Use
   GHCi to test them.-}

{- 1. Write a function: -}

isHorizontal :: Direction -> Bool
isHorizontal Up    = False
isHorizontal Down  = False
isHorizontal Left  = True
isHorizontal Right = True

{- We could also write:

isHorizontal Up    = False
isHorizontal Down  = False
isHorizontal _     = True

or

isHorizontal Left  = True
isHorizontal Right = True
isHorizontal _     = False

-}

{- that returns 'True' if the direction is 'Left' or 'Right', and
   'False' otherwise. -}


{- 2. Write a function: -}

flipHorizontally :: Direction -> Direction
flipHorizontally Left  = Right
flipHorizontally Right = Left
flipHorizontally x     = x

{- that flips horizontally (Left <-> Right, Up and Down stay the same). -}

{- Could also write:

flipHorizontally Left  = Right
flipHorizontally Right = Left
flipHorizontally Up    = Up
flipHorizontally Down  = Down
-}


{- 3. Rewrite 'equalDirections' to take a 'Pair Direction Direction' as
      input: -}

pairOfEqualDirections :: Pair Direction Direction -> Bool
pairOfEqualDirections (MkPair Up    Up)    = True
pairOfEqualDirections (MkPair Down  Down)  = True
pairOfEqualDirections (MkPair Left  Left)  = True
pairOfEqualDirections (MkPair Right Right) = True
pairOfEqualDirections (MkPair _     _)     = False

{- 4. Define a datatype 'Triple a b c' for values that have three
      components. Write functions 'get1of3 :: Triple a b c -> a',
      'get2of3' and 'get3of3' that return the first, second and third
      components. You will have to come up with the type signatures
      for the second and third one. -}

data Triple a b c = MkTriple a b c
  deriving Show

get1of3 :: Triple a b c -> a
get1of3 (MkTriple a b c) = a

get2of3 :: Triple a b c -> b
get2of3 (MkTriple a b c) = b

get3of3 :: Triple a b c -> c
get3of3 (MkTriple a b c) = c

{- 5. Pattern matching on specific characters is done by writing the
      character to match. For example: -}

isA :: Char -> Bool
isA 'A' = True
isA _   = False

{-    Write a function 'dropSpaces' :: [Char] -> [Char]' that drops
      spaces from the start of a list of characters. For example, we
      should have:

         *Week01> dropSpaces "   hello"
         "hello"

      (Strings in Haskell are really lists of 'Char's) -}

dropSpaces :: [Char] -> [Char]
dropSpaces []       = []
dropSpaces (' ':xs) = dropSpaces xs
dropSpaces xs       = xs

{- Alternatively:

dropSpaces []      = []
dropSpaces (x:xs) = if x == ' ' then dropSpaces xs else (x:xs)

or

dropSpaces [] = []
dropSpaces (x:xs)
  | x == ' '  = dropSpaces xs
  | otherwise = (x:xs)
-}

{- 6. Using 'reverse' and 'dropSpaces', write a function that removes
      spaces at the *end* of a list of characters. For example:

         *Week10> dropTrailingSpaces "hello    "
         "hello"
-}

dropTrailingSpaces :: [Char] -> [Char]
dropTrailingSpaces xs = reverse (dropSpaces (reverse xs))

{- Alternatively, using knowledge from Week 03:

dropTrailingSpaces = reverse . dropSpaces . reverse

-}

{- 7. HTML escaping. When writing HTML, the characters '<', '&', and '>'
      are special because they are used to represent tags and
      entities. To have these characters display properly as
      themselves in HTML they need to be replaced by their entity
      versions:

         '<'  becomes  '&lt;'     ("less than")
         '>'  becomes  '&gt;'     ("greater than")
         '&'  becomes  '&amp;'    ("ampersand")

      Write a function that performs this replacement on a string. You
      should have, for example,

        Week01Problems*> htmlEscape "<not a tag>"
        "&lt;not a tag&gt;"
-}

htmlEscape :: String -> String
htmlEscape ""       = ""
htmlEscape ('<':cs) = "&lt;" ++ htmlEscape cs
htmlEscape ('&':cs) = "&amp;" ++ htmlEscape cs
htmlEscape ('>':cs) = "&gt;" ++ htmlEscape cs
htmlEscape (c:cs)   = c : htmlEscape cs

{- 8. The following datatype represents a piece of text marked up with
      style information. -}

data Markup
  = Text   String        -- ^ Some text
  | Bold   Markup        -- ^ Some markup to be styled in bold
  | Italic Markup        -- ^ Some markup to be styled in italics
  | Concat Markup Markup -- ^ Two pieces of markup to be displayed in sequence

{-    Here is an example: -}

exampleMarkup :: Markup
exampleMarkup = Concat (Bold (Text "Delays")) (Concat (Text " are ") (Italic (Text "possible")))

{-    Writing markup like this is tedious, especially when there are
      lots of 'Concat's. Write a function that takes a list of
      'Markup's and concatenates them all together using 'Concat'. -}

catMarkup :: [Markup] -> Markup
catMarkup []     = Text ""
catMarkup (m:ms) = Concat m (catMarkup ms)

{-    NOTE: There is no constructor for 'Markup' that directly
      represents an empty piece of Markup. I have used 'Text ""' to
      represent empty markup. Another possibility would be to add a
      constructor 'Empty' to the 'Markup' type. -}

{-    Another way of making the writing of Markup easier is the
      automatic insertion of spaces. Write another function that
      concatenates a list of 'Markup's putting spaces between them: -}

catMarkupSpaced :: [Markup] -> Markup
catMarkupSpaced []     = Text ""
catMarkupSpaced [m]    = m
catMarkupSpaced (m:ms) = Concat m (Concat (Text " ") (catMarkupSpaced ms))

{-    NOTE: Notice that this function matches specially on the single
      element list. This allows us to place spaces (i.e. 'Text " "')
      _between_ each element of the input list.

      Another way to write this function would be to do it in two
      stages. First take the original input list and place 'Text " "'
      between each element. This can either be done by writing a new
      function, or by using the 'intersperse' function from the
      'Data.List' module. Then the resulting list can be concatenated
      using the 'catMarkup' function defined above. -}

{-    Sometimes we want to remove all formatting from a piece of
      text. Write a function that removes all 'Bold' and 'Italic'
      instructions from a piece of Markup, replacing them with their
      underlying plain markup.

      For example:

        Week01Problems*> removeStyle exampleMarkup
        Concat (Text "Delays") (Concat (Text " are ") (Text "possible"))
-}

removeStyle :: Markup -> Markup
removeStyle (Text s)       = Text s
removeStyle (Bold m)       = removeStyle m
removeStyle (Italic m)     = removeStyle m
removeStyle (Concat m1 m2) = Concat (removeStyle m1) (removeStyle m2)

{-    Finally, we can 'render' our markup to HTML. Write a function that
      converts 'Markup' to its HTML string representation, using
      '<strong>..</strong>' for bold and '<em>...</em>' for
      italics. Use the 'htmEscape' function from above to make sure
      that 'Text' nodes are correctly converted to HTML.

      For example:

         Week01Problems*> markupToHTML exampleMarkup
         "<strong>Delays</strong> are <em>possible</em>"

      and

         Week01Problems*> markupToHTML (Bold (Text "<&>"))
         "<strong>&lt;&amp;&gt;</strong>"
-}

markupToHTML :: Markup -> String
markupToHTML (Text s)       = htmlEscape s
markupToHTML (Bold m)       = "<strong>" ++ markupToHTML m ++ "</strong>"
markupToHTML (Italic m)     = "<em>" ++ markupToHTML m ++ "</em>"
markupToHTML (Concat m1 m2) = markupToHTML m1 ++ markupToHTML m2
