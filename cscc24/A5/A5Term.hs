module A5Term(Term(..), Op2(..), Value(..)) where

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

{- Parsers will be back on Wednesday -}
