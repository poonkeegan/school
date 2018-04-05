module A5Term(Term(..), Op2(..), Value(..),
              parseM, parse, parseFileM, parseFile) where

import Control.Applicative
import Data.Char
import Data.Map.Strict (Map)

import ParserLib hiding (integer)

data Term
    = Cond Term Term Term       -- Cond test then-branch else-branch
    | Lambda String Term        -- Lambda var body
    | Let [(String, Term)] Term -- Let [(name, rhs), ...] body
    | Neg Term                  -- unary minus
    | Prim2 Op2 Term Term       -- Prim2 op operand operand
    | App Term Term             -- App func arg
    | Num Integer
    | Bln Bool
    | Var String
    deriving (Eq, Show)

data Op2 = And | Or | Eq | Neq | Lt | Leq | Plus | Minus | Mul | Div | Mod
    deriving (Eq, Show)

data Value = VN Integer
           | VB Bool
           | VClosure (Map String Value) String Term
    deriving (Eq, Show)

parseM :: String -> Maybe Term
parseM = runParser termParser

parse :: String -> Term
parse = myFromJust . parseM
  where
    myFromJust Nothing = error "Syntax error when parsing Term."
    myFromJust (Just a) = a

-- This can help testing by reading from a file so you can test multi-line input
-- and also have little hassle with \
parseFileM :: String -> IO (Maybe Term)
parseFileM filename = parseM <$> readFile filename

parseFile :: String -> IO Term
parseFile filename = parse <$> readFile filename

termParser :: Parser Term
termParser = do
    whitespaces
    t <- block
    eof
    return t

block = cond <|> lambda <|> local <|> binary
-- local is for let-in; binary is for infix.

cond = do
    word "if"
    myTest <- block
    word "then"
    myThen <- block
    word "else"
    myElse <- block
    return (Cond myTest myThen myElse)

lambda = do
    terminal "\\"
    v <- var
    terminal "->"
    b <- block
    return (Lambda v b)

local = do
    word "let"
    eqns <- between (terminal "{") (terminal "}") (many equation)
    word "in"
    body <- block
    return (Let eqns body)

equation = do
    v <- var
    symbol "="
    b <- block
    terminal ";"
    return (v, b)

binary = chainr1 test boolop

boolop = fmap Prim2 (symbol "&&" *> pure And
                     <|> symbol "||" *> pure Or)

test = do
    a <- arith
    (do c <- cmp
        b <- arith
        return (Prim2 c a b))
     <|> return a

cmp = symbol "==" *> pure Eq
      <|> symbol "/=" *> pure Neq
      <|> symbol "<" *> pure Lt
      <|> symbol "<=" *> pure Leq

arith = chainl1 addend addop

addop = fmap Prim2 (symbol "+" *> pure Plus
                    <|> symbol "-" *> pure Minus)

addend = chainl1 factor mulop

mulop = fmap Prim2 (symbol "*" *> pure Mul
                    <|> symbol "/" *> pure Div
                    <|> symbol "%" *> pure Mod)

factor = do
    negs <- many (symbol "-" *> pure Neg)
    a <- app
    return (foldr ($) a negs)

app = fmap (foldl1 App) (some atom)

atom = between (terminal "(") (terminal ")") block
       <|> literal
       <|> fmap Var var

literal = fmap Num natural
          <|> fmap Bln boolean

natural = read <$> some (satisfy isDigit) <* whitespaces

boolean = (do word "True"
              return True)
          <|>
          (do word "False"
              return False)

var = do
    w <- anyWord
    if w `elem` ["if", "then", "else", "let", "in", "True", "False"]
        then empty
        else return w

anyWord = do
    c <- satisfy isAlpha
    cs <- many (satisfy isAlphaNum)
    whitespaces
    return (c:cs)

word wanted = do
    w <- anyWord
    if wanted == w then return w else empty

anySymbol = some (satisfy symChar) <* whitespaces
  where
    symChar c = c `elem` "=/<&|+-*%"

symbol wanted = do
    sym <- anySymbol
    if wanted == sym then return sym else empty
