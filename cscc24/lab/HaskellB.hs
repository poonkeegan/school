-- CSCC24 Lab Week 6: Haskell exercises part B.
--
-- The marks in each question include style marks (30%).
-- 
-- Due Friday 6PM on MarkUs: https://markus.utsc.utoronto.ca/cscc24w18/
module HaskellB where

-- [1 mark]
-- You have seen nested lists in Python and Scheme.
-- In Haskell, the list type alone can't do this. (Why?)
-- Your mission, should you accept it, is to make nested lists possible by
-- combining list with a recursive data type to achieve it.
--
-- Define the recursive data type for one item in a nested list.
-- (Not for the whole list.  The whole list is [NestedListItem a].)
data NestedListItem a = List [NestedListItem a] | Item a
    deriving Show

-- such that these functions should work, for example.
flatten :: [NestedListItem a] -> [a]
flatten lst = concat (map itemFlat lst)

itemFlat :: NestedListItem a -> [a]
itemFlat (Item a) = [a]
itemFlat (List lst) = flatten lst


-- [6 marks]
-- I need a number type that supports "positive infinity" for use in
-- weighted-graph algorithms such as Prim's, Kruskal's, Dijkstra's.
-- Let's say I just need integers and +infinity.

data Weight = Fin Integer | Inf
    deriving (Eq, Ord, Show)

-- With those "deriving"s, I'm almost set. Except:
-- Help me make Weight an instance of Num so I can do basic arithmetic.
-- 1 mark per method.
-- No need to do (-), default implementation uses (+) and negate.
instance Num Weight where
    -- (+)
    -- Corner case: anything + Inf = Inf.  The other way round too.
    (+) Inf _ = Inf
    (+) _ Inf = Inf
    (+) (Fin a) (Fin b) = Fin (a + b)
    -- negate
    -- Corner case done for you:
    -- negate Fin a = -a
    negate Fin a = Fin (-a)
    negate Inf = error "negative infinity not supported"

    -- (*)
    -- Corner case: anything * Inf = Inf.  The other way round too.
    (*) (Fin a) (Fin b) = Fin (a * b)
    (*) Inf _           = Inf
    (*) _ Inf           = Inf
    -- abs
    abs (Fin a) = Fin (abs a)
    abs Inf     = Inf
    -- signum
    -- Corner case: signum Inf is positive one. But mind how to code it.
    signum (Fin a) = Fin (signum a)
    signum Inf = Fin 1
    -- fromInteger
    fromInteger a = Fin a
