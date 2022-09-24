module Week01Problems where

import Week01
import Prelude hiding (Left, Right, reverse)

{----------------------------------------------------------------------}
{- Exercises                                                          -}
{----------------------------------------------------------------------}

{- In the questions below, replace 'undefined' with your answers. Use
   GHCi to test them. -}

{- 1. Write a function: -}

isHorizontal :: Direction -> Bool
isHorizontal = undefined

{- that returns 'True' if the direction is 'Left' or 'Right', and
   'False' otherwise. -}


{- 2. Write a function: -}

flipHorizontally :: Direction -> Direction
flipHorizontally = undefined

{- that flips horizontally (Left <-> Right, Up and Down stay the same). -}


{- 3. Rewrite 'equalDirections' to take a 'Pair Direction Direction' as
      input: -}

pairOfEqualDirections :: Pair Direction Direction -> Bool
pairOfEqualDirections = undefined


{- 4. Define a datatype 'Triple a b c' for values that have three
      components. Write functions 'get1of3 :: Triple a b c -> a',
      'get2of3' and 'get3of3' that return the first, second and third
      components. You will have to come up with the type signatures
      for the second and third one. -}


{- 5. Pattern matching on specific characters is done by writing the
      character to match. For example: -}

isA :: Char -> Bool
isA 'A' = True
isA _   = False

{-    Write a function 'dropSpaces' :: [Char] -> [Char]' that drops
      spaces from the start of a list of characters. For example, we
      should have:

         *Week01Problems> dropSpaces "   hello"
         "hello"

      (Strings in Haskell are really lists of 'Char's, so you can use
      pattern matching on them.) -}

dropSpaces :: [Char] -> [Char]
dropSpaces = undefined

{- 6. Using 'reverse' and 'dropSpaces', write a function that removes
      spaces at the *end* of a list of characters. For example:

         *Week01Problems> dropTrailingSpaces "hello    "
         "hello"
-}

dropTrailingSpaces :: [Char] -> [Char]
dropTrailingSpaces = undefined

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
htmlEscape = undefined

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
catMarkup = undefined

{-    Another way of making the writing of Markup easier is the
      automatic insertion of spaces. Write another function that
      concatenates a list of 'Markup's putting spaces between them: -}

catMarkupSpaced :: [Markup] -> Markup
catMarkupSpaced = undefined

{-    Sometimes we want to remove all formatting from a piece of
      text. Write a function that removes all 'Bold' and 'Italic'
      instructions from a piece of Markup, replacing them with their
      underlying plain markup.

      For example:

        Week01Problems*> removeStyle exampleMarkup
        Concat (Text "Delays") (Concat (Text " are ") (Text "possible"))
-}

removeStyle :: Markup -> Markup
removeStyle = undefined

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
markupToHTML = undefined
