-- CSCC24 Lab Week 10.
-- Due Friday 6PM on Markus.
-- https://markus.utsc.utoronto.ca/cscc24w18/

module LabW10 where

import Control.Applicative
import Data.Char

import ParserLib

{-
Lisp and Scheme use S-expressions. A basic version of the grammar in EBNF is:

    S ::= identifier
        | "(" S { S } ")"

identifier is as in ParserLib.hs for simplicity.  We are also omitting various
literals like numbers and quotations.

Whitespaces can surround identifiers, "(", and ")".  For example this input is
legal:

    "   (  f  ( g  x1 y1)  (h))  "

and is parsed to

    List [Ident "f", List [Ident "g",Ident "x1",Ident "y1"], List [Ident "h"]]

Implement a parser for this:

* mainParser is responsible for skipping leading whitespaces, calling sexpr
  once, and checking the lack of leftovers after.  Think of this as the
  user-facing entry point.

  runParser mainParser "   ( f x1 )   " = Just (List [Ident "f", Ident "x1"])
  runParser mainParser "   ( f x1 ) y " = Nothing

* sexpr assumes there is no leading whitespaces (because mainParser already took
  care of that).  It is responsible for skipping trailing whitespaces.  It does
  not check for lack of leftovers; it just consumes and parses what it needs and
  leaves the rest unconsumed.  Think of this as a helper parser.

  dePsr sexpr "( f x1 )   " = Just ("",   List [Ident "f", Ident "x1"])
  dePsr sexpr "( f x1 ) y " = Just ("y ", List [Ident "f", Ident "x1"])

4 marks for sexpr, and 1 mark for mainParser.
-}

data SExpr = Ident String | List [SExpr] deriving (Eq, Show)

-- [4 marks]
sexpr :: Parser SExpr
sexpr = fmap Ident (identifier []) <|> do
    terminal "("
    exprs <- some sexpr
    terminal ")"
    return (List exprs)

-- [1 mark]
mainParser :: Parser SExpr
mainParser = whitespaces *> sexpr <* eof
