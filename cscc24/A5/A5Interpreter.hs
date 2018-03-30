module A5Interpreter where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
-- You can add more imports.

import           A5Term

mainInterp :: Term -> Value
mainInterp = interp Map.empty

interp :: (Map String Value) -> Term -> Value
interp env (Num x) = VN x
interp env (Bln x) = VB x
interp env (Var v) = case Map.lookup v env of
    Just val -> val
    Nothing -> error (v ++ " not defined")
interp env (Neg x) = case interp env x of
    VN x -> VN (-x)
    VB x -> VB (not x)
    _ -> error "Taking negative of non-number"
interp env (Cond cond t f) = case interp env cond of
    VB False -> interp env t
    VB True -> interp env f
    otherwise -> error "Condition does not resolve to Bool"
interp env (Prim2 op t1 t2) = (evOp op) evt1 evt2
    where evt1 = interp env t1
          evt2 = interp env t2
interp env (Lambda name func) = VClosure env name func
interp env (Let [] term) = interp env term
interp env (Let ((str,ter):xs) term) = interp (Map.insert str val env) (Let xs term)
    where val = interp env ter
-- TODO App

evOp :: Op2 -> (Value -> Value -> Value)
evOp op = case op of
    And -> boolOp (&&)
    Or -> boolOp (||)
    Lt -> arithCond (<)
    Leq -> arithCond (<=)
    Eq -> \x y -> VB (x == y)
    Neq -> \x y -> VB (x /= y)
    Plus -> arithOp (+)
    Minus -> arithOp (-)
    Mul -> arithOp (*)
    Div -> arithOp div
    Mod -> arithOp mod 
arithCond :: (Integer -> Integer -> Bool) -> (Value -> Value -> Value)
arithCond op v1 v2 = case v1 of
    VN arg1 -> case v2 of VN arg2 -> VB (op arg1 arg2)
                          _ -> errmsg "2nd" "cond"
    _ -> errmsg "1st" "cond"

arithOp :: (Integer -> Integer -> Integer ) -> (Value -> Value -> Value)
arithOp op v1 v2 = case v1 of
    VN arg1 -> case v2 of VN arg2 -> VN (op arg1 arg2)
                          _ -> errmsg "2nd" "arith"
    _ -> errmsg "1st" "arith"

errmsg nth optype = error ("Wrong type in " ++ nth ++ " param of " ++ optype ++ " op")

boolOp :: (Bool -> Bool -> Bool) -> (Value -> Value -> Value)
boolOp op v1 v2 = case v1 of
    VB bool -> VB (op bool evalv2) 
        where evalv2 = case v2 of VB bl -> bl
                                  _ -> errmsg "2nd" "bool"
    _ -> errmsg "1st" "bool"
        

