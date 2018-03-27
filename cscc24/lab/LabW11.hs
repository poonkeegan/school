-- CSCC24 Lab Week 11.
-- Due Friday 6PM on Markus.
-- https://markus.utsc.utoronto.ca/cscc24w18/

module LabW11 where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import LabW11Defns

{-
In the lecture and the assignment, if an interpreter encounters errors such as
variable not found or division by zero, it just crashes.

In this lab, write a better interpreter that handles these errors more
gracefully.  Do not crash; use the Either type to report errors

Examples:

interp Map.empty (Var "x") = Left VarNotFound

interp (Map.fromList [("x", 0)]) (Prim2 Div (Num 5) (Var "x")) = Left DivByZero

Use the fact that Either is a Monad.  You should never need to code up "is it
Right or Left?".
-}
interp :: Map String Integer -> Expr -> Either Error Integer
interp map (Var name) = case Map.lookup name map of
                Nothing -> Left (VarNotFound name)
                Just val -> Right val
interp _ (Num int) = Right int

interp map (Prim2 op exp1 exp2) = do
    x <- interp map exp1
    y <- interp map exp2
    if (op == Mod || op == Div) && (y == 0) then Left DivByZero else Right ((ops op) x y)

ops :: Op2 -> Integer -> Integer -> Integer
ops Plus = (+)
ops Minus =(-)
ops Mul = (*)
ops Div = div
ops Mod = mod

