module A5Interpreter where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
-- You can add more imports.

import           A5Term

mainInterp :: Term -> Value
mainInterp term = case term of
    Cond _ _ _ -> interpCond term
    Num _ -> interpVal term
    Bln _ -> interpVal term

interpCond :: Term -> Value 
interpCond (Cond cond t f) = case mainInterp cond of
    VB False -> mainInterp t
    VB True -> mainInterp f
    otherwise -> error "Condition does not resolve to Bool"



interpVal :: Term -> Value 
interpVal (Num x) = VN x
interpVal (Bln x) = VB x

newtype Evalr a = PsrOf{
    deEvalr :: Value -> Either Error (Value, Value)}

data Error = Err

