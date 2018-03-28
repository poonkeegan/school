module A5Interpreter where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
-- You can add more imports.

import           A5Term

mainInterp :: Term -> Value
mainInterp = error "TODO"

op :: Op2 -> (Integer -> Integer -> b)
op And = (&&)
op Or = (||)
op Eq = (==)
op Neq = (\=)
op Lt = (<)
op Leq = (<=)
op Plus = (+)
op Minus = (-)
op Mul = (*)
op Div = div
op Mod = mod
