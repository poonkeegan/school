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

boolOpPars = (andParse <|> orParse) <* whitespaces
    where andParse = string "&&"
          orParse = string "||"

cmpPars = (eqstr <|> neqstr <|> ltstr <|> leqstr) <* whitespaces
    where eqstr = string "=="
          neqstr = string "/="
          ltstr = string "<"
          leqstr = string "<="

facPars = parsFoldl App hd tl
    where ls = some atomPars
          hd = fmap head ls
          tl = fmap tail ls

addendPars = parsFoldl binop hd tl
    where facOpPars = (\x y -> Prim2 y x) <$> facPars <*> mulopPars
          factors = many facOpPars
          hd = fmap head factors
          tl = fmap tail factors
          binop a b = (\x -> a (b x))

infix = parsFoldl binop hd tl
    where facOpPars = (\x y -> Prim2 y x) <$> facPars <*> mulopPars
          factors = many facOpPars
          hd = fmap head factors
          tl = fmap tail factors
          binop a b = (\x -> a (b x))
parsFoldl op parseHd parseTl = ((foldl op) <$> parseHd) <*> parseTl

litPars :: Term -> String -> Parser Term
litPars term strLiteral = (\_ -> term) <$> string strLiteral <* whitespaces
{-
    block
    infix
    boolop D
    test
    cmp D
    arith
    addop D
    addend T
    mulop D
    factor T
    atom T
    cond T
    lambda T
    let
    equation T
    literal D
    boolean D
-}
litTermParse = (\x -> Num x) <$> integer <|> boolPars
keyWords :: [String]
keyWords = ["if", "then", "else", "let", "in", "True", "False"]

condPars :: Parser Term
condPars = pure (\x y z -> Cond x y z) <* (string "if") <*> blockPars <* (string "then") <*> blockPars <* (string "else") <*> blockPars

varPars :: Parser Term
varPars = (\x -> Var x) <$> (identifier keyWords)

lambdaPars :: Parser Term
lambdaPars = pure (\x y -> Lambda x y) <* (parseOperator '\\' ) <*> (identifier keyWords) <* (string "->") <*> blockPars 

eqnPars = (\var op block  _ -> Prim2 op var block) <$> varPars <*> parseOperator '=' <* whitespaces <*> blockPars <*> (char ';') <* whitespaces

blockPars :: Parser Term
blockPars = infix
infixPars = error "TODO"

parseOperator :: Char -> Parser String
parseOperator c = ((\x -> [x]) <$> char c) <* whitespaces

atomPars :: Parser Term
atomPars = bracBlockPars <|> litTermParse <|> varPars

bracBlockPars = parseOperator '(' *> blockPars <* parseOperator ')' 
