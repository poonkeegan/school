{- CSCC24 Lab Week 12.
   Due Friday 6PM on Markus.
   https://markus.utsc.utoronto.ca/cscc24w18/

Recall NumRec.hs from lecture, which supports defining one self-recursive
function at a time.

This lab extends it to support defining any number of self- and/or mutually-
recursive functions together.

See the comment on Rec below (or in LabW12Defns.hs).  There is also an example
usage at the end.
-}

module LabW12 where

import LabW12Defns

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

mainInterp = interp Map.empty

binop Plus = (+)
binop Minus = (-)
binop Times = (*)

interp :: Map String Value -> Expr -> Value
interp env (N i) = VN i
interp env (Var v) = case Map.lookup v env of
  Just val -> val
  Nothing -> error (v ++ " not found")
interp env (Op2 op e1 e2) = case (interp env e1, interp env e2) of
  (VN i, VN j) -> VN (binop op i j)
  _ -> error "wrong type in Op2"
interp env (IfZero e e0 e1) = case interp env e of
  VN 0 -> interp env e0
  VN _ -> interp env e1
  _ -> error "wrong type in IsZero"
interp env (App f e) = case interp env f of
  VClosure fEnv v body ->
      let eVal = interp env e
          bEnv = Map.insert v eVal fEnv
      in interp bEnv body
  _ -> error "wrong type in App"

--interp env (Rec defs body) = error "TODO"

interp env (Rec defs body) = interp newenv body
    where newenv = foldr (envirsetup newenv) env defs 
          envirsetup new_env (f, v, expr) env = Map.insert f (VClosure new_env v expr) env

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

All of expr2, expr2, expr3 may call all of f, g, h.
-}

{-
https://en.wikipedia.org/wiki/Hofstadter_sequence#Hofstadter_Female_and_Male_sequences

F n = if n==0 then 1 else n - M (F (n-1))
M n = if n==0 then 0 else n - F (M (n-1))
-}

fmdef = [ ("F", "n",
           IfZero (Var "n")
             (N 1)
             (Op2 Minus (Var "n")
                        (App (Var "M")
                             (App (Var "F") (Op2 Minus (Var "n") (N 1))))))
        , ("M", "n",
           IfZero (Var "n")
             (N 0)
             (Op2 Minus (Var "n")
                        (App (Var "F")
                             (App (Var "M") (Op2 Minus (Var "n") (N 1))))))
        ]

f n = mainInterp (Rec fmdef (App (Var "F") (N n)))

m n = mainInterp (Rec fmdef (App (Var "M") (N n)))
