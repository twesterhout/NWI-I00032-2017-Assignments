{-# LANGUAGE UnicodeSyntax #-}

module Main where

{-|
Module      : Exercise1
Description : Assignment 1 of the Advanced Programming Course
Copyright   : (c) Tom Westerhout, 2017
License     : MIT

1. The serialize typeclass proposed in the exercise is... strange so to say.
How is one supposed to obtain a list of 'String' /before/ parsing? My initial
guess was "by means of 'Prelude.words' function, of course". This, however, has
some strange consequences. For example, how does one handle parantheses? Should
both @Bin (Leaf 4) 5 (Leaf 6)@ and @Bin ( Leaf 4 ) 5 ( Leaf 6 )@ be valid?

So I have decided to specialise the classic 'Show' and 'Read' typeclasses and
provide a general definition of 'read'' and 'write'' functions (apostrophes are
used to not confuse our functions with the ones defined in Prelude).

2.
    * Bool: *
    * Bin: * → *
    * Rose: * → *
    * Bin Int: *
    * Tree: * → * → *
    * T1: (* → *) → * → *
    * T2: (* → *) → (* → *) → *
    * T3: (* → * → *) → * → *
    * T4: (* → *) → (* → *) → *
-}

-- | Rose tree definition from the exercise
data Rose α = Rose α [Rose α]

instance (Show α) => Show (Rose α) where
    show (Rose x xs) = unwords ["Rose", show x, show xs]

instance (Read α) => Read (Rose α) where
    readsPrec d r = do
        -- Purely procedural programming :) and List monad takes care of the
        -- rest.
        -- First, read the word "Rose"
        ("Rose", r1) <- lex r
        -- Next, a value of type 'α'.
        (x, r2)      <- (readsPrec d ∷ (Read α) => ReadS α) r1
        -- And finally, recursively read a list of children.
        (xs, r3)     <- (readList ∷ (Read α) => ReadS [Rose α]) r2
        -- At this point it seems like a good idea to use the State monad
        -- instead...
        return (Rose x xs, r3)


-- | Binary tree definition from the exercise
data Bin α = Leaf | Bin (Bin α) α (Bin α)

instance (Show α) => Show (Bin α) where
    show (Leaf) = "Leaf"
    show (Bin l x r) = unwords [ "Bin"
                               , showWithPars l
                               , showWithPars x, showWithPars r
                               ]
        -- According to Show docs extra parantheses are OK, so always add them.
        where showWithPars s = "(" ++ show s ++ ")"

instance (Read α) => Read (Bin α) where
    readsPrec d r =  readLeaf d r ++ readBin d r
        where readLeaf d r = do
                  ("Leaf", r1) <- lex r
                  return (Leaf, r1)
              readBin d r = do
                  ("Bin", r1) <- lex r
                  (left, r2)  <- readParen False (readsPrec d
                                      ∷ (Read α) => ReadS (Bin α)) r1
                  (x, r3)     <- (readsPrec d
                                      ∷ (Read α) => ReadS α) r2
                  (right, r4) <- readParen False (readsPrec d
                                      ∷ (Read α) => ReadS α) r3
                  return (Bin left x right, r4)


-- | General 'read'' function. First, we concatenate the list into a single
-- 'String', then read 'α', and finally split the rest string into words again.
read' ∷ (Read α) => [String] → Maybe (α, [String])
read' []     = Nothing
read' s = case (reads ∷ Read α => ReadS α) (unwords s) of
    [(x, r)] → Just (x, (words r))
    _        → Nothing

-- | General 'write'' function.
write' ∷ (Show α) => α → [String] → [String]
write' x = (++) (words (show x))


-- | Container interface from the exercise
class Container t where
    cinsert ∷ (Ord α) ⇒ α → t α → t α
    ccontains ∷ (Eq α, Ord α) ⇒ α → t α → Bool
    cshow ∷ (Show α) ⇒ t α → [String]
    cnew ∷ t α


-- | This is just beautiful :) I didn't even expect it to be so short.
instance Container [] where
    -- cinsert a xs = if a `elem` xs then xs else a:xs
    cinsert   = (:)
    ccontains = elem
    cshow     = words . show
    cnew      = []


-- | Implementation for binary trees. /Mind you/, no care is taken to keep the
-- tree balanced.
instance Container Bin where
    cinsert a (Leaf)  = Bin Leaf a Leaf
    cinsert a (Bin l x r)
        -- It's non-obvious from the exercise whether duplicate elements are
        -- allowed. So in my implementation they are.
        --
        -- | a == x      = Bin l x r
        | a <= x       = Bin (cinsert a l) x r
        | otherwise   = Bin l x (cinsert a r)

    ccontains a Leaf  = False
    ccontains a (Bin l x r)
        | a == x      = True
        | a < x       = ccontains a l
        | otherwise   = ccontains a r

    cshow = flip write' []

    cnew  = Leaf


main ∷ IO ()
main = do

    -- Int
    print (read' [] ∷ Maybe (Int, [String]))
    print (read' ["-12345", "Φ"] ∷ Maybe (Int, [String]))
    print (write' (-1487612) ["a", "b", "c"])

    -- List
    print (read' ["[", "True", ",", "False", "]", "asdf"] ∷ Maybe ([Bool], [String]))
    print (read' ["[", "True", "False", "]", "asdf"] ∷ Maybe ([Bool], [String]))
    print (write' [-1, 2, 3] ["⇒≤☑"])

    -- Binary
    print (read' ["Bin (Bin Leaf \"\xe0b0\\x1b[35m Haha!\" Leaf) \"1234\" Leaf"] ∷ Maybe (Bin String, [String]))
    putStrLn $ unwords (write' (Bin Leaf "1234" Leaf) [])
    print (write' (Bin Leaf "1234" Leaf) [])

    -- Rose
    print (read' ["Rose 54321 [Rose 0 []]"] ∷ Maybe (Rose Integer, [String]))
    putStrLn $ unwords (write' (Rose True [Rose False [Rose False [Rose False [Rose False []]]]]) [])

    -- Container
    let fill c = cinsert 5
               . cinsert 3
               . cinsert 2
               . cinsert 8
               . cinsert 8 $ c
        c1 = fill (cnew ∷ Bin Int)
        c2 = fill (cnew ∷ [Int])
    putStrLn . unwords . cshow $ c1
    print $ ccontains 2 c1
    print $ ccontains 10 c1
    putStrLn . unwords . cshow $ c2
    print $ ccontains 2 c2
    print $ ccontains 10 c2
