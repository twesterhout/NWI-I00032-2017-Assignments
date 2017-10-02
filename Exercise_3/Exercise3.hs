{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DefaultSignatures #-}

-- |
-- Module      : Exercise3
-- Description : Generics by Overloading & Clean Native Generics
-- Copyright   : (c) Tom Westerhout, 2017
-- License     : MIT

module Exercise3 (
    Bin(..),
    -- * Kind-Index Generic Serialisation

    -- Our 'Exercise2.Serialise' class in "Exercise2" is indeed only suitable
    -- for serialisation of types of kind @*@. There is, however, an
    -- interesting qoute I've come across:
    --
    -- > In Haskell 98, * is the only inhabited kind, that is, all values
    -- > have types of kind *. GHC introduces another inhabited kind, #, for
    -- > unlifted types.
    --
    -- So what I don't understand is why would one need a @serialise@ function
    -- for kinds other than @*@? Generic mapping, equality etc. -- sure, why
    -- not, but serialisation -- I don't see it... So I'm not going to do this
    -- part as I don't see how it differs from "Exercise2".

    -- * Serialisation using Haskell's native Generics

    -- I got writing working, but not reading. GHC generates @Read@
    -- instances at compile-time, so it should be possible to obtain
    -- contructor names at compile-time... I failed to find a way to do it
    -- using @Generics@.
    SerialiseImpl(readPrecImpl, writePrecImpl),
    Serialise(readPrec', writePrec')

    -- * Rest
    -- The night has passed, it's 6:30 a.m. so I'm afraid I'll stop here.
    ) where

import Prelude.Unicode
import Data.Maybe
import Control.Arrow(first, second)
import Control.Applicative((<|>))
import Control.Monad
import GHC.Generics
import Text.ParserCombinators.ReadP
import Data.Char(isSpace)

import qualified Data.List



type ReadS' α = String → Maybe (α, String)


appPrec ∷ Int
appPrec = 10

step ∷ Int → Int
step p = (p + 1) `mod` 11

readParensIf ∷ Bool → ReadP α → ReadP α
readParensIf p x = if p then mandatory
              else (x +++ mandatory)
    where mandatory = between (char '(') (char ')') x

showParensIf ∷ Bool → String → String
showParensIf p s = if p then "(" ++ s ++ ")"
                   else s

sepAndParen ∷ Bool → [String] → String
sepAndParen p xs =
    case filter (not . null) xs of
        []  -> ""
        [x] -> x
        xs  -> showParensIf p $ Data.List.concat . Data.List.intersperse " " $ xs

-- | Compact @Serialise@ typeclass, i.e. no generic information is included
-- in the serialised data.
class SerialiseImpl α where
    readPrecImpl ∷ Int → ReadP (α χ)
    writePrecImpl ∷ Int → (α χ) → String

class Serialise α where
    readPrec' ∷ Int → ReadP α
    writePrec' ∷ Int → α → String

    default readPrec' ∷ (Generic α, SerialiseImpl (Rep α))
        ⇒ Int → ReadP α
    readPrec' p = readPrecImpl p >>= return . to

    default writePrec' ∷ (Generic α, SerialiseImpl (Rep α))
        ⇒ Int → α → String
    writePrec' p x = writePrecImpl p (from x)

-- |
-- Both functions undefined, because one simply can create no instances of
-- type 'V1'.
instance SerialiseImpl V1 where
    readPrecImpl  = undefined
    writePrecImpl = undefined

-- |
-- Reading always succeeds as there's nothing to read; writing produces an
-- empty string.
instance SerialiseImpl U1 where
    readPrecImpl  _ = return U1
    writePrecImpl _ U1 = ""

instance (SerialiseImpl α, SerialiseImpl β) => SerialiseImpl (α :+: β) where
    readPrecImpl p = ((readPrecImpl p ∷ (SerialiseImpl α) ⇒ ReadP (α χ))
                          >>= return . L1)
                 +++ ((readPrecImpl p ∷ (SerialiseImpl β) ⇒ ReadP (β χ))
                          >>= return . R1)
    writePrecImpl p (L1 x) = writePrecImpl p x
    writePrecImpl p (R1 x) = writePrecImpl p x

instance (SerialiseImpl α, SerialiseImpl β) => SerialiseImpl (α :*: β) where
    readPrecImpl p = readParensIf False $
        do x <- readPrecImpl 11
           skipSpaces
           y <- readPrecImpl 11
           return (x :*: y)

    writePrecImpl p (x :*: y) = sepAndParen False $
        [writePrecImpl 11 x, writePrecImpl 11 y]

instance (Serialise ν) => SerialiseImpl (K1 ι ν) where
    readPrecImpl p = readPrec' p >>= return . K1
    writePrecImpl p (K1 x) = writePrec' p x

instance (SerialiseImpl α, Constructor ξ) => SerialiseImpl (M1 C ξ α) where
    readPrecImpl ∷ Int → ReadP (M1 C ξ α χ)
    readPrecImpl p =
        do string "Leaf" -- This is just to make the code compile. Next
                         -- line is nicer, but doesn't compile...
           -- string $ conName ((M1 undefined)
           --     :: (SerialiseImpl α, Constructor ξ) ⇒ (M1 C ξ α χ))
           skipSpaces
           x <- readPrecImpl 11
           return (M1 x)
    writePrecImpl p c@(M1 x) = sepAndParen (p > appPrec) $
        [conName c, writePrecImpl 11 x]

instance (SerialiseImpl α, Selector σ) => SerialiseImpl (M1 S σ α) where
    readPrecImpl p = readPrecImpl p >>= return . M1
    writePrecImpl p (M1 x) = writePrecImpl p x

instance (SerialiseImpl α) => SerialiseImpl (M1 D δ α) where
    readPrecImpl p = readPrecImpl p >>= return . M1
    writePrecImpl p (M1 x) = writePrecImpl p x

data Bin a = Leaf | Bin (Bin a) a (Bin a) deriving (Eq, Read, Show, Generic)

instance Serialise Int where
    readPrec' p = readS_to_P $ readsPrec p
    writePrec' p x = show x



instance Serialise (Bin Int)

