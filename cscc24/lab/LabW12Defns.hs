module LabW12Defns where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

data Expr = N Integer
          | Var String
          | Op2 BinOp Expr Expr
          | IfZero Expr Expr Expr
          | App Expr Expr
          | Rec [(String, String, Expr)] Expr
    deriving (Eq, Show)

{-
Rec is [mutually] recursive function binding and use.

    letrec f x = expr1
           g x = expr2
           h x = expr3
    in body

is represented by

    Rec [ ("f", "x", expr1),
          ("g", "x", expr2),
          ("h", "x", expr3) ]
        body

All of expr1, expr2, expr3 may call all of f, g, h.
-}

data BinOp = Plus | Minus | Times deriving (Eq, Show)

-- The type of possible answers from the interpreter.
data Value = VN Integer
           | VClosure (Map String Value) String Expr
    deriving (Eq, Show)
