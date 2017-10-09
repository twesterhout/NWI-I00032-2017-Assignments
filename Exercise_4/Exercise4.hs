{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- |
-- Module      : Exercise4
-- Description : Functor, Applicative and Monad
-- Copyright   : (c) Tom Westerhout, 2017
-- License     : MIT


module Exercise4
    ( -- * Appetizer
      Student(..)
    , f2
    , f3

      -- * Serialisation using Functor, Applicative and Monad

    , Serialised(Serialised)

      -- ** State access for primitive types

    , State(State)
    , Serialise
    , rd
    , wrt

      -- ** Monad for State

      -- | See 'State'.

      -- ** Matching strings
    , match
    , pred

      -- ** Serialisation of Generic Types

      -- | In the previous exercise I made extensive use of the 'ReadP'
      -- monad, i.e. all was already done using 'Monad', 'Applicative', and
      -- 'Alternative' classes.
      --
      -- Anyways, to make better use of the tools developed in this
      -- exercise, let's adapt our 'Exercise3.Serialise' class a bit:
    , Serialisable(readPrec', writePrec')
    -- , SerialiseImpl(readPrecImpl, writePrecImpl)


    , Bin(..)
    , read'
    , show'

    ) where

import Prelude hiding(pred, readParen, showParen)
import Data.Proxy
import GHC.Generics
import Control.Monad
import System.IO
import Control.Applicative
import Control.Exception
import Control.Arrow

-- | Student record.
data Student = Student { fname :: String -- ^ First name.
                       , lname :: String -- ^ Last name.
                       , snum  :: Int    -- ^ Student number.
                       } deriving (Show)


-- | Reads a 'Student' from 'stdin'. This is a straighforward
-- translation of the @f2@ function from Clean to Haskell. The only thing I
-- couldn't get right is catching the exception. I'm quite new to them, and
-- from the implementation of '<|>' for 'IO':
--
-- > instance Alternative IO where
-- >     empty = failIO "mzero"
-- >     (<|>) = mplusIO
-- >
-- > -- with
-- > mplusIO m n = m `catchException` \ (_ :: IOError) -> n
--
-- I expected '<|>' to catch the exception thrown by 'Prelude.read'.
f2 :: IO Student
f2 = do f <- putStr "Your first name please: " >> getLine
        l <- putStr "Your last name please: "  >> getLine
        s <- putStr "Your student number please: " >> getLine
        return (Student f l) `ap` evaluate (read s)
     -- <|> fail "Hello world!"

-- | Reads a 'Student' from 'stdin'. One it's /very/ similar to 'f2', even
-- the implementation looks pretty much the same.
f3 :: IO Student
f3 = let f = putStr "Your first name please: " *> getLine
         l = putStr "Your last name please: "  *> getLine
         s = putStr "Your student number please: " *> getLine
     in pure Student <*> f <*> l <*> (read <$> s)

-- | Our own definition of State monad so that we can reimplement @(<$>)@,
-- @(>>=)@, @(<*>)@ etc. This is my second try at this. I've first
-- implemented everything for 'State' as @s -> (a, s)@. For 'Alternative',
-- however, 'Maybe' does come in handy. So I thought, maybe a 'MaybeT'
-- monad transformer will help. Turned out I needed 'StateT'+'Maybe'
-- combination rather than 'MaybeT'+'State', but 'StateT' is a bit too off
-- topic, so I've decided to just use @s -> (Maybe a, s)@ :D
newtype State s a = State { runState :: s -> (Maybe a, s) }
    deriving (Generic)

-- | According to the exercise, I'm supposed to come up with a type to hold
-- serialised version of objects that supports amortised \(\mathcal{O}(1)\)
-- reading and writing.
--
-- I've actually made a mistake here as appending to list is an
-- \(\mathcal{O}(N)\) operation. Don't know why I haven't noticed it
-- immediately, but it's quite a bit of work do re-implement everything, so
-- I'll leave it be.
newtype Serialised = Serialised { runSerialised :: [String] }
    deriving (Show, Generic)


{- FIRST TRY

-- | A type from the exercise.
type Serialise a = State Serialised (Maybe a)

-- | Serialises the argument and adds it to the state.
wrt :: Show a => a -> Serialise String
wrt x = let x' = show x
        in State $ \s -> ( return x'
                         , Serialised $ x' : runSerialised s )

-- | Returns the last string that's been added to the state.
rd :: Serialise String
rd = State $ \s -> case runSerialised s of
    []     -> (Nothing, s)
    (x:xs) -> (Just x, Serialised xs)

-- | Let's make 'State' a 'Functor'.
instance Functor (State s) where
    fmap f x = State $ runState x >>> first f

-- | Let's make 'State' an 'Applicative' functor.
instance Applicative (State s) where
    pure x = State $ \s -> (x, s)
    f <*> x = State $ runState f >>> second (runState x)
                                 >>> \(f', (x', s)) -> (f' x', s)

-- | Let's make 'State' a 'Monad'.
instance Monad (State s) where
    return = pure
    x >>= f = State $ runState x >>> \(x', s) -> runState (f x') s

-- | @match x@ Compares the serialised version of @x@ to the next string in
-- the state. If these match, @Just x@ is returned, otherwise @Nothing@.
--
-- This function is a perfect example of an absolutely unreadable Haskell
-- code :D I mean, it's short and elegant, but nowhere near easy to
-- undertand.
match :: Show a => a -> Serialise a
match x = let match' y = if (show x == y) then return x else Nothing
          in flip (>>=) match' <$> rd

-- | Very similar to 'match' except that a predicate is used rather than
-- operator @(==)@.
pred :: (String -> Bool) -> Serialise String
pred f = let match' y = if (f y) then return y else Nothing
         in flip (>>=) match' <$> rd

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance Functor m => Functor (MaybeT m) where
    fmap f x = MaybeT $ (liftM f) <$> (runMaybeT x)

instance (Monad m) => Applicative (MaybeT m) where
    pure = MaybeT . return . Just
    f <*> x = MaybeT $ runMaybeT f >>= \f' ->
        case f' of
            Nothing  -> return Nothing
            Just f'' -> runMaybeT x >>= return . fmap f''

instance (Monad m) => Alternative (MaybeT m) where
    empty   = MaybeT $ return Nothing
    x <|> y = MaybeT $ runMaybeT x >>= \x' ->
        case x' of
            Nothing  -> runMaybeT y
            Just x'' -> return x'

instance (Monad m) => Monad (MaybeT m) where
    return  = pure
    x >>= f = MaybeT $ do
        x' <- runMaybeT x
        case x' of
            Nothing  -> return Nothing
            Just x'' -> runMaybeT (f x'')
-}

-- | A type from the exercise.
type Serialise a = State Serialised a

-- | Serialises the argument and adds it to the state.
wrt :: Show a => a -> Serialise String
wrt x = wrt' (show x)

wrt' :: String -> Serialise String
wrt' x = State $ \s -> ( return x
                       , Serialised $ (runSerialised s) ++ [x])

-- | Returns the next string in the state.
rd :: Serialise String
rd = State $ \s -> case runSerialised s of
    []     -> (Nothing, s)
    (x:xs) -> (Just x, Serialised xs)

-- | Let's make 'State' a 'Functor'.
instance Functor (State s) where
    fmap f x = State $ runState x >>> first (fmap f)

-- | Let's make 'State' an 'Applicative' functor.
instance Applicative (State s) where
    pure x  = State $ \s -> (Just x, s)
    f <*> x = State $ \s ->
        case (runState f s) of
            (Nothing, s') -> (Nothing, s')
            (Just f', s') -> case (runState x s') of
                                 (Nothing, s'') -> (Nothing, s'')
                                 (Just x', s'') -> (Just (f' x'), s'')

instance Alternative (State s) where
    empty   = fail "empty"
    x <|> y = State $ \s ->
        case (runState x s) of
            (Nothing, _) -> runState y s
            x' -> x'

-- | Let's make 'State' a 'Monad'.
instance Monad (State s) where
    return  = pure
    fail _  = State $ \s -> (Nothing, s)
    x >>= f = State $ \s ->
        case (runState x s) of
            (Nothing, s') -> (Nothing, s')
            (Just x', s') -> runState (f x') s'

-- | @match x@ Compares the serialised version of @x@ to the next string in
-- the state. If these match, @Just x@ is returned, otherwise @Nothing@.
match :: Show a => a -> Serialise a
match x = let match' y = if (show x == y) then return x else fail "No match"
          in rd >>= match'

-- | Very similar to 'match' except that a predicate is used rather than
-- operator @(==)@.
pred :: (String -> Bool) -> Serialise String
pred f = let match' y = if (f y) then return y else fail "No match"
         in rd >>= match'

-- | Precision of function application.
appPrec = 10 :: Int


class SerialiseImpl a where
    readPrecImpl :: Int -> Serialise (a x)
    writePrecImpl :: Int -> (a x) -> Serialise String

-- | Out serialisation. Notice how we now use 'Serialise' rather than
-- 'ReadP' and 'ShowS'.
class Serialisable a where
    readPrec'  :: Int -> Serialise a
    writePrec' :: Int -> a -> Serialise String

    default readPrec' :: (Generic a, SerialiseImpl (Rep a))
        => Int -> Serialise a
    readPrec' p = readPrecImpl p >>= return . to

    default writePrec' :: (Generic a, SerialiseImpl (Rep a))
        => Int -> a -> Serialise String
    writePrec' p x = writePrecImpl p (from x)


-- | Equivalent to 'Prelude.readParen' except that it works in our own
-- 'State' monad.
readParen :: Bool -> Serialise a -> Serialise a
readParen p x = if p then mandatory
                else mandatory <|> x
    where mandatory = do pred ((==) "(")
                         x' <- x
                         pred ((==) ")")
                         return x'

-- | Equivalent to 'Prelude.showParen' except that it works in our own
-- 'State' monad.
showParen :: Bool -> Serialise String -> Serialise String
showParen p x = if p
    then wrt' "(" >> x >> wrt' ")"
    else x

-- | Both functions undefined, because one simply can create no instances
-- of type 'V1'.
instance SerialiseImpl V1 where
    readPrecImpl  = undefined
    writePrecImpl = undefined

-- | Reading always succeeds as there's nothing to read; writing fails!
-- This is __a very cheaty way to eliminate brackets__ :)
instance SerialiseImpl U1 where
    readPrecImpl  _ = return U1
    writePrecImpl _ U1 = fail "Nothing to write"

instance (SerialiseImpl a, SerialiseImpl b)
    => SerialiseImpl (a :+: b) where

    readPrecImpl p = ((readPrecImpl p :: (SerialiseImpl a) => Serialise (a x))
                          >>= return . L1)
                 <|> ((readPrecImpl p :: (SerialiseImpl b) => Serialise (b x))
                          >>= return . R1)
    writePrecImpl p (L1 x) = writePrecImpl p x
    writePrecImpl p (R1 x) = writePrecImpl p x

instance (SerialiseImpl a, SerialiseImpl b) => SerialiseImpl (a :*: b) where
    readPrecImpl p = readParen False $
        do x <- readPrecImpl 11
           y <- readPrecImpl 11
           return (x :*: y)

    writePrecImpl p (x :*: y) =
        (writePrecImpl 11 x) >> (writePrecImpl 11 y)

instance (Serialisable v) => SerialiseImpl (K1 i v) where
    readPrecImpl p = readPrec' p >>= return . K1
    writePrecImpl p (K1 x) = writePrec' p x

-- | __NB:__ I've solved that problem I've been having last week. Turns
-- out, /ScopedTypeVariables/ was the only thing missing.
instance (SerialiseImpl a, Constructor x) => SerialiseImpl (M1 C x a) where
    readPrecImpl p = readParen False $
        do
           pred $ (==) (conName (undefined :: M1 C x a t))
           x <- readPrecImpl 11
           return (M1 x)

    writePrecImpl p c@(M1 x) =
        let x' = writePrecImpl 11 x
            c' = wrt' (conName c)
        in (showParen (p > appPrec) $ c' >> x')
           <|> c'

instance (SerialiseImpl a, Selector s) => SerialiseImpl (M1 S s a) where
    readPrecImpl p = readPrecImpl p >>= return . M1
    writePrecImpl p (M1 x) = writePrecImpl p x

instance (SerialiseImpl a) => SerialiseImpl (M1 D d a) where
    readPrecImpl p = readPrecImpl p >>= return . M1
    writePrecImpl p (M1 x) = writePrecImpl p x

-- | Simple tree.
data Bin a = Leaf | Bin (Bin a) a (Bin a) deriving (Eq, Read, Show, Generic)

instance Serialisable Int where
    readPrec' p = let toInt s = case [x | (x, "") <- readsPrec p s] of
                                    [x] -> return x
                                    _   -> fail "No parse"
                  in rd >>= toInt
    writePrec' p x = wrt x

instance Serialisable (Bin Int)

-- | Our analogue of 'Prelude.read'
read' :: Serialisable a => Serialised -> Maybe a
read' = fst
        . runState (readPrec' 0 :: (Serialisable a) => Serialise a)

-- | Out analogue of 'Prelude.show'
show' :: Serialisable a => a -> Serialised
show' x = snd $ runState (writePrec' 0 x) (Serialised [])
