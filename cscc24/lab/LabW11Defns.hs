module LabW11Defns where

import Data.Map.Strict (Map)

data Expr = Num Integer
          | Var String
          | Prim2 Op2 Expr Expr
    deriving (Eq, Show)

data Op2 = Plus | Minus | Mul | Div | Mod
    deriving (Eq, Show)
-- Use Haskell's div and mod for Div and Mod, but check the divisor for 0 first.

data Error = VarNotFound String  -- the String is the variable name
           | DivByZero
    deriving (Eq, Show)
