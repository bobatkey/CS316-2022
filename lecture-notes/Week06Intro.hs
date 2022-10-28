module Week06Intro where

-- REMEMBER:
--   - Test **tomorrow** Wednesday 26th 12:00 ---> Thursday 27th 12:00
--     - 10 questions on weeks 1-5
--     - should take ~1-2hrs
--     - counts for 50%
--     - redemption test in Week 9
--   - UG Placement presentations GH514 Wednesday 14:00-16:00 GH514


{-    WEEK 06 : SIMULATING SIDE EFFECTS

    Haskell doesn't have "side effects" or is "pure".
     - What does this mean?
     - Is it a good thing?

    In Haskell:

       f :: Int -> Int

    what can it do?

       - Not terminate (or crash with an unrecoverable error)
       - Or it can return an Int
       - if we give it the same input twice, we'll get the same answer


    In Java:

       public static int f(int x)

    what can it do?

       - Non terminate
       - throw an Exception
       - return an int
       - print things to the screen
       - generate random numbers
       - read files
       - make network calls
         - posting cat pictures to <social network of your choice>
         - buy things on amazon
         - launch nuclear missiles

   How do we make Haskell do these things?
-}


{- Part 6.1 : Simulating Exceptions -}

{- data Maybe a = Nothing | Just a -}

returnOk :: a -> Maybe a
returnOk x = Just x

failure :: Maybe a   -- throw new Exception();
failure = Nothing

search :: Eq k => k -> [(k,v)] -> Maybe v
search k [] = failure
search k ((k',v):kvs) = if k == k' then returnOk v else search k kvs

lookupList :: Eq k => [k] -> [(k,v)] -> Maybe [v]
lookupList [] kvs = returnOk []
lookupList (k:ks) kvs =
  case search k kvs of
    Nothing -> Nothing
    Just v ->
      case lookupList ks kvs of
        Nothing -> Nothing
        Just vs ->
          returnOk (v:vs)

ifOK :: Maybe a -> (a -> Maybe b) -> Maybe b
ifOK Nothing  k = Nothing
ifOK (Just a) k = k a

lookupList_v2 :: Eq k => [k] -> [(k,v)] -> Maybe [v]
lookupList_v2 [] kvs = returnOk []
lookupList_v2 (k:ks) kvs =
  search k kvs         `ifOK` (\v ->
  lookupList_v2 ks kvs `ifOK` (\vs ->
  returnOk (v:vs)))

--  "ifOK" is basically the semicolon

-- What if we could reprogram the semicolon ?

-- How to simulate 'catch'

-- data Either a b = Left a | Right b
-- data Result a = Ok a | Error String

{- Part 6.2 : Simulating (Mutable) State -}

{- We can make updatable state 'pure' by making fresh names for
   variables instead of treating each variable as a thing that can
   change.

   int i0 = 0;

   int i1 = 10;

   int i2 = i1 + 1;

   ...

   int i3 = i2 - 1;

     -- this is the form that compilers use internally when compiling most languages
     -- SSA (Static Single Assignment)
-}

{- output = new LinkedList<Pair<Int,String>>();
   int i = 0;
   for (String x : xs) {
      Pair<> p = new Pair(i, x);
      output.append(p);
      i++;
   }
-}

numberList :: [a] -> Int -> (Int, [(a,Int)])
numberList []     i = (i, [])
numberList (x:xs) i =
  let p        = (x,i)
      i0       = i + 1
      (i1, ys) = numberList xs i0
  in (i1, p:ys)

-- State type
type State a = Int -> (Int, a)

returnSt :: a -> State a
         -- a -> Int -> (Int, a)
returnSt x i = (i, x)

andThen :: State a           -> (a -> State b)         -> State b
        -- (Int -> (Int, a)) -> (a -> Int -> (Int, b)) -> Int -> (Int, b)
andThen computation1 k i =
  let (i0, a) = computation1 i
      (i1, b) = k a i0
  in (i1, b)

get :: State Int
    -- Int -> (Int, Int)
get i = (i, i)

put :: Int -> State ()
    -- Int -> Int -> (Int, ())
put newstate currentstate = (newstate, ())

numberList_v2 :: [a] -> State [(a,Int)]
numberList_v2 [] = returnSt []
numberList_v2 (x:xs) =
  get              `andThen` (\i ->
  put (i+1)        `andThen` (\() ->
  numberList_v2 xs `andThen` (\ys ->
  returnSt ((x,i) : ys))))


-- Simulating exceptions with 'Maybe' -- 'Nothing' means 'throw an exception'
--                                       'Just a'  means 'return a'

-- returnOk :: a -> Maybe a
-- returnSt :: a -> State a

-- ifOK    :: Maybe a -> (a -> Maybe b) -> Maybe b
-- andThen :: State a -> (a -> State b) -> State b

-- "do this and, with the result, do the next thing"



{-   Part 6.4 : SIMULATING PRINTING (or OUTPUT) -}

data Tree a
  = Leaf
  | Node (Tree a) a (Tree a)
  deriving (Eq, Show)

printAndSum :: Tree Int -> ([String], Int)
printAndSum Leaf =
  ([], 0)
printAndSum (Node l x r) =
  let (out1, lsum) = printAndSum l
      out          = ["Doing " ++ show x]
      (out2, rsum) = printAndSum r
  in (out1 ++ out ++ out2, lsum + x + rsum)

testTree :: Tree Int
testTree = Node (Node Leaf 4 Leaf) 7 (Node Leaf 9 Leaf)

data Printing a = MkPrinting [String] a
  deriving Show

returnPr :: a -> Printing a
returnPr x = MkPrinting [] x

andThenPrinting :: Printing a -> (a -> Printing b) -> Printing b
--                 first job which may print 'output1' and return 'a'
--                               second job requires an 'a', may print 'output2' and return 'b'
--                                                    output1 ++ output2, return 'b'
andThenPrinting (MkPrinting output1 a) k =
  case k a of
    MkPrinting output2 b ->
      MkPrinting (output1 ++ output2) b


printAndSum_v2 :: Tree Int -> Printing Int
printAndSum_v2 Leaf =
  returnPr 0
--  ([], 0)
printAndSum_v2 (Node l x r) =
  printAndSum_v2 l              `andThenPrinting`
    (\lsum ->
       printStr ("Doing " ++ show x) `andThenPrinting`
         (\() ->
            printAndSum_v2 r              `andThenPrinting`
              (\rsum ->
                  returnPr (lsum + x + rsum))))

printStr :: String -> Printing ()
printStr s = MkPrinting [s] ()

-- Another example of "do this, then do that"


{-   Part 6.5 : PROCESSES -}

data Process a
  = End a
  | Input (String -> Process a)
  | Output String (Process a)

example :: Process ()
example =
  Output "What is your name?"
         (Input (\input ->
             Output ("Hello " ++ input)
                    (End ())))


runProcess :: Process a -> IO a
runProcess (End a)      = return a
runProcess (Input k)    = do s <- getLine; runProcess (k s)
runProcess (Output s p) = do putStrLn s; runProcess p

returnProcess :: a -> Process a
returnProcess x = End x

{-
                Input
         /        |      \
   Output "a"  End 5    Output "b"
        |                |
     End 1             End 4
-}

{-  \i -> Output (show i) (End ()) -}

{-
                Input
         /        |            \
   Output "a"  Output "5"    Output "b"
        |         |             |
   Output "1"   End ()       Output "4"
        |                       |
      End ()                  End ()

-}


process1 = Input (\input ->
                    if input == "a" then Output "a" (End 1)
                    else if input == "b" then End 5
                    else Output "b" (End 4))

process2 i = Output (show i) (End ())

sequ :: Process a -> (a -> Process b) -> Process b
sequ (End a)      k = k a
sequ (Input p)    k = Input (\s -> sequ (p s) k)
sequ (Output s p) k = Output s (sequ p k)

output :: String -> Process ()
output s = Output s (End ())

input :: Process String
input = Input (\s -> End s)

example_v2 :: Process ()
example_v2 =
  output "What is your name?" `sequ` \() ->
  input                       `sequ` \str ->
  output ("Hello " ++ str)    `sequ` \() ->
  returnProcess ()


-- Four ways of "simulating side effects"
--
-- All have similar interfaces:
--
--   ifOK            :: Maybe a    -> (a -> Maybe b)    -> Maybe b
--   andThen         :: State a    -> (a -> State b)    -> State b
--   andThenPrinting :: Printing a -> (a -> Printing b) -> Printing b
--   sequ            :: Process a  -> (a -> Process b)  -> Process b


-- The common name is "Monad"

-- class Monad m where
--    return :: a -> m a
--    (>>=)  :: m a -> (a -> m b) -> m b
