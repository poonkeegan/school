module DepInj where

-- File format checker for my toy file format:
-- First three characters should be A, L, newline.

toyCheck :: IO Bool
toyCheck = do
    c1 <- getChar
    c2 <- getChar
    c3 <- getChar
    return ([c1, c2, c3] == "AL\n")

-- How do I test this?
-- And/OR how do I restrict it to getChar and nothing funny behind my back?
-- Answer: Dependency Injection (or is it Template Method?)

-- Several ways to do Dependency Injection in Haskell. Here's one.

class Monad f => MonadToyCheck f where
    toyGetChar :: f Char
-- Simplifying assumptions: Enough characters, no failure.
-- A practical version should have a story for EOF and/or other exceptions.

toyCheck2 :: MonadToyCheck f => f Bool
toyCheck2 = do
    c1 <- toyGetChar
    c2 <- toyGetChar
    c3 <- toyGetChar
    return ([c1, c2, c3] == "AL\n")

-- Only things toyCheck2 can do: toyGetChar, purely functional programming.
-- Because USER chooses f.  And toyCheck2 doesn't even know what it is.
-- All it knows is it can call toyGetChar.

-- The real McToy
instance MonadToyCheck IO where
    toyGetChar = getChar

realProgram :: IO Bool
realProgram = toyCheck2

-- Fake news for purely functional testing.

newtype Feeder a = F (String -> (String, a))
-- Again, simplifying assumptions etc.

unF (F sf) = sf

instance Monad Feeder where
    return a = F (\s -> (s, a))
    F sf >>= k = F (\s -> case sf s of
                       (s1, a) -> unF (k a) s1)

instance MonadToyCheck Feeder where
    toyGetChar = F (\(c:s) -> (s, c))

instance Functor Feeder where
    fmap f p = do a <- p
                  return (f a)

instance Applicative Feeder where
    pure a = return a
    pf <*> pa = do
        f <- pf
        a <- pa
        return (f a)

testToyChecker2 :: String -> Bool
testToyChecker2 str = case unF toyCheck2 str of
  (_, b) -> b

-- https://www.slideshare.net/ScottWlaschin/fp-patterns-buildstufflt
-- slide 13
