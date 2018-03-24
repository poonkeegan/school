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
ter :: String -> Parser String
ter = terminal

ters :: [String] -> Parser String
ters [] = pure ""
ters [a] = ter a
ters (hd:ls) = ter hd *> ters ls

betwter :: String -> String -> Parser a -> Parser a
betwter str1 str2 = between (ter str1) (ter str2)

keyWords :: [String]
keyWords = ["if", "then", "else", "let", "in", "True", "False"]

termParser :: Parser Term
termParser = whitespaces *> blockPars <* eof

blockPars, infixPars, testPars, arithPars, addendPars, facPars, atomPars :: Parser Term
bracBlockPars, condPars, lambdaPars, letPars, boolPars, litTermParse, varPars :: Parser Term
boolOpPars, addopPars, mulopPars :: Parser (Term -> Term -> Term)
cmpPars :: Parser String
eqnPars :: Parser (String, Term)
arthopPars :: Parser String -> Parser (Term -> Term -> Term)

blockPars = condPars <|> lambdaPars <|> letPars <|> infixPars

infixPars = chainr1 testPars boolOpPars

boolOpPars = arthopPars (ter "&&" <|> ter "||")

testPars = full <|> arithPars
    where full = (\a op b -> Prim2 op a b) <$> arithPars <*> cmpPars <*> arithPars 

cmpPars = ter "==" <|> ter "/=" <|> ter "<" <|> ter "<=" 

arithPars = chainl1 addendPars addopPars

arthopPars operators = (\x a b -> Prim2 x a b) <$> operators

addopPars = arthopPars (ter "+" <|> ter "-")

addendPars = chainl1 facPars mulopPars

mulopPars = arthopPars (ter "*" <|> ter "/")

facPars = (pure (fold App)) <*> (some atomPars)
    where fold op ls = foldl op (head ls) (tail ls)

atomPars = bracBlockPars <|> litTermParse <|> varPars

bracBlockPars = betwter  "(" ")" blockPars

condPars = do
   cond <- betwter "if" "then" blockPars
   suc <- blockPars
   ter "else"
   fail <- blockPars
   return (Cond cond suc fail)

lambdaPars = do
    name <- betwter "\\" "->" (identifier keyWords)
    func <- blockPars
    return (Lambda name func)

letPars = do
    eqn <- betwter "let" "in" (betwter "{" "}" (many eqnPars))
    block <- blockPars
    return (Let eqn block)

eqnPars = do
    var <- identifier keyWords 
    block <- betwter "=" ";" blockPars
    return (var,block)


litTermParse = (\x -> Num x) <$> integer <|> boolPars

boolPars = (\x -> Bln (str2bl x)) <$> (ter "True" <|> ter "False")
    where str2bl "True" = True
          str2bl "False" = False

varPars = (\x -> Var x) <$> (identifier keyWords)

