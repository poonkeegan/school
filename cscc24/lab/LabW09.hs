-- CSCC24 Lab Week 9
-- Due Friday 6PM on Markus.
-- https://markus.utsc.utoronto.ca/cscc24w18/

module LabW09 where

import Control.Applicative
import Data.Char

import ParserLib

natural :: Parser Integer
natural = fmap read (some (satisfy isDigit))

-- All exercises can be done within Applicative and using clever lambdas.  But
-- if you can't think of a way, using do-notation or Monad's >>= is also OK.

-- [4 marks]
-- Read a natural, a plus sign, a second natural.  Return their sum.
-- It is OK to expect no whitespaces.
-- Allow garbage at the end.
--
-- runParser nat2add "42+3" = Just 45
-- runParser nat2add "42+3abc" = Just 45
-- dePsr nat2add "42+3abc" = Just ("abc", 45)
nat2add :: Parser Integer
nat2add = pure (\x _ z -> x + z) <*> natural <*> (char '+') <*> natural

-- [2 marks]
-- Like nat2add but disallow garbage at the end.
--
-- runParser nat2adde "42+3" = Just 45
-- runParser nat2adde "42+3abc" = Nothing
nat2adde :: Parser Integer
nat2adde = pure (\x _ z _ -> x + z) <*> natural <*> (char '+') <*> natural <*> eof

-- [2 marks]
-- Advanced.
--
-- Read a natural, a plus sign or a minus sign, a second natural.  Return their
-- sum or difference according to the plus/minus in the middle.
-- It is OK to expect no whitespaces.
-- Allow garbage at the end.
--
-- runParser nat2op "42+3" = Just 45
-- runParser nat2op "42-3" = Just 39
--
-- Hint: Use <|> for "or".  Have a little parser that reads '+' and returns the
-- (+) function.  And have one for '-' and (-).  Then whichever function is
-- returned, you can simply apply it to the two numbers, you don't even care
-- what it means.
nat2op :: Parser Integer
nat2op = fmap (\x y z -> y x z) natural <*> (parseplus <|> parseminus) <*> natural


parseplus = fmap (\_ -> (+)) $ char ('+')
parseminus = fmap (\_ -> (-)) $ char ('-')
