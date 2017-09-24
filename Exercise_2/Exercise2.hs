{-# LANGUAGE UnicodeSyntax #-}

-- |
-- Module      : Exercise2
-- Description : Assignment 2 of the Advanced Programming Course
-- Copyright   : (c) Tom Westerhout, 2017
-- License     : MIT

module Exercise2 (
    -- * Predefined Types
    UNIT(..),
    EITHER(..),
    PAIR(..),
    CONS(..),
    Bin(..),
    BinG(..),
    ListG(..),
    -- * Review Questions

    -- $answers

    -- * Generic Serialisation

    -- $design-problem
    fromList,
    toList,
    fromBin,
    toBin,

    -- ** With Generic Information
    VSerialise(vreadPrec', vwritePrec'),
    vread',
    vwrite',

    -- ** Without Generic Information
    Serialise(readPrec', writePrec'),
    read',
    write',

    test
    ) where

import Prelude.Unicode
import Data.Maybe
import Control.Arrow
import Control.Applicative
import Control.Monad



-- | Unit
data UNIT = UNIT deriving (Eq, Read, Show)

-- | Sum
data EITHER a b = LEFT a | RIGHT b deriving (Eq, Read, Show)

-- | Product
data PAIR a b = PAIR a b deriving (Eq, Read, Show)

-- | Constructor
data CONS a = CONS String a deriving (Eq, Read, Show)

-- | Binary trees
data Bin a = Leaf | Bin (Bin a) a (Bin a) deriving (Eq, Read, Show)

-- | Generic representation of a Binary tree
type BinG a = EITHER (CONS UNIT)
                     (CONS (PAIR (Bin a)
                           (PAIR a (Bin a))))

-- | Generic representation of a List
type ListG a = EITHER (CONS UNIT)
                      (CONS (PAIR a [a]))


-- $answers
-- 1) We have three implementations of @(==)@ for 'UNIT'. All three give
-- the same results because 'UNIT' only has one state. As for the code, I
-- like third implementation the most.
--
-- 2) The 'String' parameter in 'CONS' constructor should, in principle, by
-- a function of type @a@. So there's no need to compare these names --
-- they are the same by construction.
--
-- 3) Generic representations of both @Leaf@ and @[]@ is 'UNIT'. @Leaf ==
-- []@ gives compile-time error :) No matter how you define it. Type of
-- @(==)@ is @a → a@, so comparing 'Bin' with 'List' is illformed.



-- $design-problem
-- There's a design problem with our definition of 'CONS'. It /tries/ to
-- keep the info about constructor names storing them in a 'String' field.
-- This info, however, in only available at runtime. So if we @read@ a
-- @CONS UNIT@, there's no way to verify that constructor name we read
-- actually makes sense. So 'CONS' is OK with both
--
-- > ["CONS", "[]", "UNIT"]
--
-- and
--
-- > ["CONS", "Abrakadabra", "UNIT"]
--
-- So why is this a problem? According to the exercise, 'toList' function
-- __doesn't__ fail! This means that @LEFT (CONS \"Abrakadabra\" UNIT)@
-- must produce a valid list. OK, suppose we neglect the name parameter and
-- just construct an empty list. Then we'll have problems when no generic
-- information is provided. Consider
--
-- > ["[]"]
--
-- vs.
--
-- > ["(:)", "123", "(", "[]", ")"]
--
-- We can distinguish these two cases by looking at number of arguments,
-- right? But what if out type is
--
-- > data Foo = A Int Int | B Int Int
--
-- Now we really __need__ to take the constructor name into account. This,
-- however, is impossible when reading a 'CONS' object, because constructor
-- name is not part of the type and is hence unknown at compile-time. We
-- thus can only trigger an error when converting to @List@. So it should
-- be
--
-- > toList ∷ ListG a → Maybe [a]
--
-- This, however, is not enough. We need to keep track of /all/ possible
-- ways of parsing input. So our generic @read@ function should be
--
-- > read' ∷ [String] → [(a, [String])]
--
-- And because Haskell has support for 'ReadS', let's make it
--
-- > read' ∷ String → [(a, String)]
--
-- With these modifications we can now implement everything :)
--
-- /P.S./ I think this also answers the \"Reflection\" questions.



-- | Converts a list to its generic representation. This function is
-- inverse of 'toList'. Constructor for unit list is called @"[]"@, for
-- composite list -- @"(:)"@.
fromList ∷ [a] → ListG a
fromList []     = LEFT  . CONS "[]"  $ UNIT
fromList (x:xs) = RIGHT . CONS "(:)" $ PAIR x xs

-- | Converts a generic representation of a list to an actual list. This
-- function is inverse of 'fromList'. It may fail if constructors have
-- wrong names.
toList ∷ ListG a → Maybe [a]
toList (LEFT  (CONS "[]" UNIT))         = Just []
toList (RIGHT (CONS "(:)" (PAIR x xs))) = Just (x:xs)
toList _                                = Nothing

-- | Converts a binary tree to its generic representaion. This function is
-- inverse of 'toBin'. Constructor for unit tree is called @"Leaf"@, for
-- composite tree -- @"Bin"@.
fromBin ∷ Bin a → BinG a
fromBin Leaf        = LEFT  . CONS "Leaf" $ UNIT
fromBin (Bin l x r) = RIGHT . CONS "Bin"  . PAIR l $ PAIR x r

-- | Converts a generic representation of a binary tree back to an actual
-- binary tree. This function is inverse of 'fromBin'. It may fail if
-- constructors have wrong names
toBin ∷ BinG a → Maybe (Bin a)
toBin (LEFT  (CONS "Leaf" UNIT))               = Just Leaf
toBin (RIGHT (CONS "Bin" (PAIR l (PAIR x r)))) = Just (Bin l x r)
toBin _                                        = Nothing

-- | Verbose version of @Serialise@ typeclass, i.e. generic type
-- constructors are included in the serialised data.
class VSerialise α where
    vreadPrec' ∷ Int → ReadS α
    vwritePrec' ∷ Int → α → String

-- | Creates a reades that expects a certain token. Very similar to
-- 'GHC.Read.expectP'.
_expect ∷ String → ReadS String
_expect s text = case lex text of
    [(s, r)] → return (s, r)
    _        → []

-- | Surrounds input 'String' with brackets if condition is 'True'.
_surround ∷ Bool → String → String
_surround p s = if p then "(" ++ s ++ ")" else s

instance VSerialise UNIT where
    vreadPrec' _ s = case lex s of
        [("UNIT", rest)] → return (UNIT, rest)
        _                → []
    vwritePrec' _ _ = "UNIT"

instance (VSerialise α, VSerialise β) ⇒ VSerialise (EITHER α β) where
    -- | Monads + Arrows = Magic :)
    vreadPrec' p = readParen (p > 10) $
            (_expect "LEFT"  >=> (vreadPrec' 11 ∘ snd)
                             >=> return ∘ first LEFT)
        &&& (_expect "RIGHT" >=> (vreadPrec' 11 ∘ snd)
                             >=> return ∘ first RIGHT)
        >>> arr (uncurry (++))

    vwritePrec' p (LEFT x) = _surround (p > 10) ∘ unwords $
                                  ["LEFT", vwritePrec' 11 x]
    vwritePrec' p (RIGHT x) = _surround (p > 10) ∘ unwords $
                                  ["RIGHT", vwritePrec' 11 x]

instance (VSerialise α, VSerialise β) ⇒ VSerialise (PAIR α β) where
    vreadPrec' p = readParen (p > 10) $
        _expect "PAIR" >=> \s → do (a, rest1) ← vreadPrec' 11 (snd s)
                                   (b, rest2) ← vreadPrec' 11 rest1
                                   return (PAIR a b, rest2)

    vwritePrec' p (PAIR a b) = _surround (p > 10) ∘ unwords $
        ["PAIR", vwritePrec' 11 a, vwritePrec' 11 b]

instance (VSerialise α) ⇒ VSerialise (CONS α) where
    vreadPrec' p = readParen (p > 10) $
        _expect "CONS" >=> (readsPrec 11 ∘ snd) >=> \(name, rest) →
            vreadPrec' 11 rest >>= return ∘ first (CONS name)

    vwritePrec' p (CONS name a) = _surround (p > 10) ∘ unwords $
        ["CONS", show name, vwritePrec' 11 a]

-- | Our hand-made verbose analogue of 'Prelude.read'.
vread' ∷ (VSerialise α) ⇒ String → Maybe α
vread' s = case vreadPrec' 0 s of
    [(a,"")] → Just a
    _        → Nothing

-- | Our hand-made verbose analogue of 'Prelude.show'.
vwrite' ∷ (VSerialise α) ⇒ α → String
vwrite' = vwritePrec' 0

instance VSerialise Int where
    vreadPrec' = readsPrec
    vwritePrec' p x = showsPrec p x ""

instance VSerialise Char where
    vreadPrec' = readsPrec
    vwritePrec' p x = showsPrec p x ""

instance (VSerialise α) ⇒ VSerialise [α] where
    vreadPrec' p = vreadPrec' p >=> \(l, rest) →
        case toList l of
            Just xs → return (xs, rest)
            Nothing → []

    vwritePrec' p = vwritePrec' p ∘ fromList

instance (VSerialise α) ⇒ VSerialise (Bin α) where
    vreadPrec' p = vreadPrec' p >=> \(l, rest) →
        case toBin l of
            Just xs → return (xs, rest)
            Nothing → []

    vwritePrec' p = vwritePrec' p ∘ fromBin

-- | Compact @Serialise@ typeclass, i.e. no generic information is included
-- in the serialised data.
class Serialise α where
    readPrec' ∷ Int → ReadS α
    writePrec' ∷ Int → α → String

instance Serialise UNIT where
    readPrec' _ s  = return (UNIT, s)
    writePrec' _ _ = ""

instance (Serialise α, Serialise β) ⇒ Serialise (EITHER α β) where
    readPrec' p =
            (readPrec' p >=> return ∘ first LEFT)
        &&& (readPrec' p >=> return ∘ first RIGHT)
        >>> arr (uncurry (++))

    writePrec' p (LEFT x)  = writePrec' p x
    writePrec' p (RIGHT x) = writePrec' p x

instance (Serialise α) ⇒ Serialise (CONS α) where
    readPrec' p = readParen False $
        readsPrec p >=> \(name, rest) →
            readPrec' 11 rest >>= return ∘ first (CONS name)

    writePrec' p (CONS name a) =
        let a' = writePrec' 10 a
        in if null a' then show name
                      else _surround (p > 10) $ unwords [show name, a']

instance (Serialise α, Serialise β) ⇒ Serialise (PAIR α β) where
    readPrec' p = readParen False $ \s →
        do (a, rest1) ← readPrec' (p + 1 `mod` 11) s
           (b, rest2) ← readPrec' (p + 1 `mod` 11) rest1
           return (PAIR a b, rest2)

    writePrec' p (PAIR a b) =
        let a' = writePrec' (p + 1 `mod` 11) a
            b' = writePrec' (p + 1 `mod` 11) b
            c  = filter (not ∘ null) [a', b']
        in _surround (p > 10 && length c > 1) $ unwords c

instance Serialise Int where
    readPrec' = readsPrec
    writePrec' p x = showsPrec p x ""

instance Serialise Char where
    readPrec' = readsPrec
    writePrec' p x = showsPrec p x ""

instance (Serialise α) ⇒ Serialise [α] where
    readPrec' p = readPrec' p >=> \(l, rest) →
        case toList l of
            Just xs → return (xs, rest)
            Nothing → []

    writePrec' p = writePrec' p ∘ fromList

instance (Serialise α) ⇒ Serialise (Bin α) where
    readPrec' p = readPrec' p >=> \(l, rest) →
        case toBin l of
            Just xs → return (xs, rest)
            Nothing → []

    writePrec' p = writePrec' p ∘ fromBin

-- | Our hand-made analogue of 'Prelude.show'.
read' ∷ (Serialise α) ⇒ String → Maybe α
read' s = case readPrec' 0 s of
    [(a,"")] → Just a
    _        → Nothing

-- | Our hand-made analogue of 'Prelude.show'.
write' ∷ (Serialise α) ⇒ α → String
write' = writePrec' 0

-- | Main
test ∷ IO ()
test = let x1 = []       ∷ [Int]
           x2 = [1,2,3]  ∷ [Int]
           x3 = [1..100] ∷ [Int]
           y1 = Leaf                             ∷ Bin Int
           y2 = Bin Leaf 123 Leaf                ∷ Bin Int
           y3 = Bin (Bin Leaf 432 Leaf) 123 Leaf ∷ Bin Int
       in do putStrLn "[*] Verbose:"
             putStrLn $ unwords ["x1 =", show x1]
             putStrLn $ unwords ["x1 =", vwrite' x1]
             putStrLn $ unwords ["x1 =", show $ fromList x1, "(says GHC)"]
             putStrLn $ unwords ["cycle ->", show $ vread' (vwrite' x1) == Just x1]

             putStrLn $ unwords ["x2 =", show x2]
             putStrLn $ unwords ["x2 =", vwrite' x2]
             putStrLn $ unwords ["x2 =", show $ fromList x2, "(says GHC)"]
             putStrLn $ unwords ["cycle ->", show $ vread' (vwrite' x2) == Just x2]

             putStrLn $ unwords ["x3 =", show x3]
             putStrLn $ unwords ["x3 =", vwrite' x3]
             putStrLn $ unwords ["cycle ->", show $ vread' (vwrite' x3) == Just x3]

             putStrLn $ unwords ["y1 =", show y1]
             putStrLn $ unwords ["y1 =", vwrite' y1]
             putStrLn $ unwords ["y1 =", show $ fromBin y1, "(says GHC)"]
             putStrLn $ unwords ["cycle ->", show $ vread' (vwrite' y1) == Just y1]

             putStrLn $ unwords ["y2 =", show y2]
             putStrLn $ unwords ["y2 =", vwrite' y2]
             putStrLn $ unwords ["y2 =", show $ fromBin y2, "(says GHC)"]
             putStrLn $ unwords ["cycle ->", show $ vread' (vwrite' y2) == Just y2]

             putStrLn $ unwords ["y3 =", show y3]
             putStrLn $ unwords ["y3 =", vwrite' y3]
             putStrLn $ unwords ["y3 =", show $ fromBin y3, "(says GHC)"]
             putStrLn $ unwords ["cycle ->", show $ vread' (vwrite' y3) == Just y3]

             putStrLn "[*] Compact:"
             putStrLn $ unwords ["x1 =", show x1]
             putStrLn $ unwords ["x1 =", write' x1]
             putStrLn $ unwords ["cycle ->", show $ read' (write' x1) == Just x1]

             putStrLn $ unwords ["x2 =", show x2]
             putStrLn $ unwords ["x2 =", write' x2]
             putStrLn $ unwords ["cycle ->", show $ read' (write' x2) == Just x2]

             putStrLn $ unwords ["x3 =", show x3]
             putStrLn $ unwords ["x3 =", write' x3]
             putStrLn $ unwords ["cycle ->", show $ read' (write' x3) == Just x3]

             putStrLn $ unwords ["y1 =", show y1]
             putStrLn $ unwords ["y1 =", write' y1]
             putStrLn $ unwords ["cycle ->", show $ read' (write' y1) == Just y1]

             putStrLn $ unwords ["y2 =", show y2]
             putStrLn $ unwords ["y2 =", write' y2]
             putStrLn $ unwords ["cycle ->", show $ read' (write' y2) == Just y2]

             putStrLn $ unwords ["y3 =", show y3]
             putStrLn $ unwords ["y3 =", write' y3]
             putStrLn $ unwords ["cycle ->", show $ read' (write' y3) == Just y3]

