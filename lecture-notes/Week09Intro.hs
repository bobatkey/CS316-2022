{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Week09Intro where

import Control.Concurrent (forkIO, MVar, newEmptyMVar, putMVar, takeMVar)
import Prelude hiding (mapM)
import Data.Traversable   (for)
import Network.HTTP       ( simpleHTTP
                          , getRequest
                          , getResponseBody
                          )
import Week08 (Parser, runParser, JSON (..), parseJSON)



{-   WEEK 9 : DATA DEPENDENCIES and APPLICATIVE FUNCTORS -}



{-   PART 9.1 : Sequences of Actions -}

{-
class Monad m where
  return :: a -> m a
  (>>=)  :: m a -> (a -> m b) -> m b
-}

-- Sequences of actions: mapM, mapTreeM

mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM f [] = return []
mapM f (x:xs) =
  do y  <- f x
     ys <- mapM f xs
     return (y:ys)

-- map f [] = []
-- map f (x:xs) = (:) (f x) (map f xs)

lift2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
lift2 f ma mb =
  do a <- ma
     b <- mb
     return (f a b)

mapM_v2 :: Monad m => (a -> m b) -> [a] -> m [b]
mapM_v2 f []     = return []
mapM_v2 f (x:xs) = lift2 (:) (f x) (mapM_v2 f xs)

data Tree a
  = Leaf
  | Node (Tree a) a (Tree a)
  deriving Show

mapTreeM :: Monad m => (a -> m b) -> Tree a -> m (Tree b)
mapTreeM f Leaf = return Leaf
mapTreeM f (Node l x r) =
  do l' <- mapTreeM f l
     y  <- f x
     r' <- mapTreeM f r
     return (Node l' y r')

-- mapTreeM_v2 f (Node l x r) = lift3 Node (mapTreeM_v2 f l) (f x) (mapTreeM_v2 f r)

{-
   do isChar '"'
      xs <- zeroOrMore stringChar
      isChar '"'
      return xs
-}


-- lift1, lift2, lift3, lift4

-- apply

-- f :: Int -> String -> Char

-- f 1 :: String -> Char

-- f 1 "a" :: Char

apply :: (a -> b) -> a -> b
apply f a = f a

-- f `apply` 1 `apply` "a"

mapply :: Monad m => m (a -> b) -> m a -> m b
mapply mf ma =
  do f <- mf
     a <- ma
     return (f a)

lift2_v2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
lift2_v2 f ma mb = ((return f) `mapply` ma) `mapply` mb

mapM_v3 :: Monad m => (a -> m b) -> [a] -> m [b]
mapM_v3 f []     = return []
mapM_v3 f (x:xs) = return (:) `mapply` (f x) `mapply` (mapM_v3 f xs)



{-   PART 9.2 : Applicative, a new type class -}

{-
class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
-}

mapM_v4 :: Applicative m => (a -> m b) -> [a] -> m [b]
mapM_v4 f []     = pure []
mapM_v4 f (x:xs) = pure (:) <*> (f x) <*> (mapM_v4 f xs)




{-   PART 9.3 : Data Dependencies and Parallelism -}

-- Requests and Responses
type Request = String
type Response = String

-- Fetch
data Fetch a
  = Blocked [Request] ([Response] -> Fetch a)
  | Return a

instance Show a => Show (Fetch a) where
  show (Blocked requests _) = "Blocked " ++ show requests ++ " <blocked>"
  show (Return a)           = "Return " ++ show a

makeRequest :: Request -> Fetch Response
makeRequest uri = Blocked [uri] (\[response] -> Return response)

-- Monad
instance Monad Fetch where
  return :: a -> Fetch a
  return x = Return x

  (>>=) :: Fetch a -> (a -> Fetch b) -> Fetch b
  Blocked reqs fetch >>= k = Blocked reqs (\resps -> (fetch resps) >>= k)
  Return a           >>= k = k a

instance Applicative Fetch where
  pure :: a -> Fetch a
  pure = return

  (<*>) :: Fetch (a -> b) -> Fetch a -> Fetch b
  Blocked reqs1 fetch1 <*> Blocked reqs2 fetch2 =
    Blocked (reqs1 ++ reqs2) (\resps -> let resp1 = take (length reqs1) resps
                                            resp2 = drop (length reqs1) resps
                                        in fetch1 resp1 <*> fetch2 resp2)
  Blocked reqs1 fetch1 <*> Return a =
    Blocked reqs1 (\resp -> fetch1 resp <*> Return a)
  Return f <*> Blocked reqs2 fetch2 =
    Blocked reqs2 (\resp -> Return f <*> fetch2 resp)
  Return f <*> Return a =
    Return (f a)

instance Functor Fetch where
  fmap f ma = pure f <*> ma


sequentialRequests :: Fetch (Response, Response)
sequentialRequests =
  do resp1 <- makeRequest "a"
     resp2 <- makeRequest "b"
     return (resp1, resp2)

parallelRequests :: Fetch (Response, Response)
parallelRequests =
  pure (\resp1 resp2 -> (resp1, resp2))
    <*> makeRequest "a"
    <*> makeRequest "b"


{- So far, we've just described a small Domain Specific Language (DSL)
   for talking about web requests, with the feature that it can
   distinguish between sequential and parallel dependencies.

-}


{-    Part 9.4 : Concurrency and Communication -}

-- forkIO

concurrentMessages :: IO ()
concurrentMessages =
  do forkIO (putStrLn "Hello from the background thread!")
     putStrLn "Hello from the foreground thread!"


-- MVars

{-  type MVar a

    newEmptyMVar :: IO (MVar a)

    putMVar :: MVar a -> a -> IO ()

    takeMVar :: MVar a -> IO a
-}

spawnReceiver :: MVar String -> IO ()
spawnReceiver mvar =
  do forkIO (do msg <- takeMVar mvar
                putStrLn ("Message received: " ++ msg))

     return ()

{-   Part 9.5 : A Logging object -}

data LogCommand
  = LogMessage String
  | LogStop (MVar ())

type Logger = MVar LogCommand

logger :: Logger -> Int -> IO ()
logger loggerVar counter =
  do cmd <- takeMVar loggerVar
     case cmd of
       LogMessage msg ->
         do putStrLn ("LOG(" ++ show counter ++ "): " ++ msg)
            logger loggerVar (counter + 1)
       LogStop resp ->
         do putStrLn ("LOG STOPPED")
            putMVar resp ()

makeLogger :: IO Logger
makeLogger =
  do m <- newEmptyMVar
     forkIO (logger m 0)
     return m

logMessage :: Logger -> String -> IO ()
logMessage loggerVar msg =
  do putMVar loggerVar (LogMessage msg)

logStop :: Logger -> IO ()
logStop loggerVar =
  do resp <- newEmptyMVar
     putMVar loggerVar (LogStop resp)
     ()   <- takeMVar resp
     return ()


{-   Part 9.6 : Making requests in parallel -}

doRequest :: Logger -> Request -> IO Response
doRequest log url =
  do log `logMessage` ("Requesting " ++ url)
     httpResp <- simpleHTTP (getRequest url)
     body <- getResponseBody httpResp
     log `logMessage` ("Request " ++ url ++ " finished")
     return body
