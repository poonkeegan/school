module A4TermParser where

import Control.Applicative

-- More imports as you need.

import A4Term
import ParserLib

-- This can help testing by reading from a file so you can test multi-line input
-- and also have little hassle with \
parseFile :: String -> IO (Maybe Term)
parseFile filename = do
    inp <- readFile filename
    let ans = runParser termParser inp
    return ans

-- Parsers
termParser :: Parser Term
termParser = error "TODO"

boolPars, litTermParse :: Parser Term
addopPars, mulopPars :: Parser String

addopPars = plusPars <|> minusPars
    where plusPars = parseOperator '+'
          minusPars = parseOperator '-'

mulopPars = mulPars <|> divPars
    where mulPars = parseOperator '*'
          divPars = parseOperator '/'

boolPars = trueParse <|> falseParse
    where trueParse = litPars (Bln True) "True"
          falseParse = litPars (Bln False) "False"

cmpPars = (eqstr <|> neqstr <|> ltstr <|> leqstr) <* whitespaces
    where eqstr = string "=="
          neqstr = string "/="
          ltstr = string "<"
          leqstr = string "<="

facPars = hd
    where ls = some atomPars
          hd = fmap head ls
          tl = fmap tail ls

litPars :: Term -> String -> Parser Term
litPars term strLiteral = (\_ -> term) <$> string strLiteral <* whitespaces
{-
    block
    infix
    boolop
    test
    cmp
    arith
    addop D
    addend
    mulop D
    factor
    atom T
    cond
    lambda
    let
    equation T
    literal D
    boolean D
-}
litTermParse = (\x -> Num x) <$> integer <|> boolPars
keyWords :: [String]
keyWords = ["if", "then", "else", "let", "in", "True", "False"]

varPars :: Parser Term
varPars = (\x -> Var x) <$> (identifier keyWords)

eqnPars = (\var op block  _ -> Prim2 op var block) <$> varPars <*> parseOperator '=' <* whitespaces <*> blockPars <*> (char ';') <* whitespaces


blockPars = error "TODO"
infixPars = error "TODO"

parseOperator :: Char -> Parser String
parseOperator c = ((\x -> [x]) <$> char c) <* whitespaces

atomPars = bracBlockPars <|> litTermParse <|> varPars

bracBlockPars = parseOperator '(' *> blockPars <* parseOperator ')' 
