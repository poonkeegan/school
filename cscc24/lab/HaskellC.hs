-- CSCC24 Lab Week 7: Haskell exercises part C.
--
-- The marks in each question include style marks (30%).
-- 
-- Due Friday 6PM on MarkUs: https://markus.utsc.utoronto.ca/cscc24w18/
module HaskellC where

newtype IntState a = IntStateOf{deIntState :: Int -> (Int, a)}

-- Analogous to C's i++.
postInc :: IntState Int
postInc = IntStateOf (\i -> (i+1, i))

-- Run an IntState program with an initial state.  Get just its return value.
run :: IntState a -> Int -> a
run prog i0 = case deIntState prog i0 of (_, a) -> a

-- Before writing the Monad instance, here is an example program to show what
-- can happen.

-- Using >>= directly:
albertProg1 :: IntState (Int, Int)
albertProg1 =
    postInc >>= (\i -> postInc >>= (\j -> return (i, j)))
    -- Most parentheses here can be omitted.

-- Using do-notation:
albertProg2 :: IntState (Int, Int)
albertProg2 = do
    i <- postInc
    j <- postInc
    return (i, j)

-- Experiment and observe:
--    run albertProg1 42 = (42, 43)
--    run albertProg2 8 = (8, 9)
--    etc
-- Get the feeling that postInc increments an internal counter and returns the
-- counter value before the increment.
-- Initial counter value is from run's 2nd parameter.


instance Monad IntState where
    return a = IntStateOf (\i -> (i, a))
    prog1 >>= k = IntStateOf (\i0 -> case deIntState prog1 i0 of
                                       (i1, a) -> deIntState (k a) i1)
    -- Equivalently:
    -- IntStateOf t >>= k = IntStateOf (\i0 -> case t i0 of
    --                                           (i1, a) -> deIntState (k a) i1)

instance Functor IntState where
    -- Not required, but good exercise to implement it directly.
    fmap f (IntStateOf t) = IntStateOf (\i0 -> case t i0 of
                                                 (i1, a) -> (i1, f a))

instance Applicative IntState where
    pure = return
    -- Not required, but good exercise to implement it directly.
    IntStateOf tf <*> IntStateOf ta = IntStateOf (\i0 -> case tf i0 of
                                                           (_, f) -> case ta i0 of
                                                                       (i1, a) -> (i1, f a))


-- Now the question for you.  A binary tree type (elements in internal nodes) is
-- defined:

data BT v = Null | Node (BT v) v (BT v) deriving Eq

-- This notation goes like: (left-subtree element right-subtree)
instance Show v => Show (BT v) where
    showsPrec _ t = flatsParen t
      where
        flats Null = id
        flats (Node left v right) =
            (case left of Null -> id; t -> flatsParen t . showChar ' ')
            . shows v
            . (case right of Null -> id; t -> showChar ' ' . flatsParen t)
        flatsParen t = showChar '(' . flats t . showChar ')'

albertTree :: BT String
albertTree = Node (Node (Node Null "ll" Null)
                        "l"
                        (Node Null "lr" Null))
                  "root"
                  (Node Null
                        "r"
                        (Node Null "rr" Null))

-- Use IntState (and postInc, and Monad operations (do-notation OK, probably
-- better)) to write a program that takes a binary tree, returns a new binary
-- tree that has the same shape, but the elements are Int's, and the numbers go
-- in the order 0, 1, 2,... in the tree's in-order.

number :: BT v -> BT Int
number t = run (numberHelper t) 0

numberHelper :: BT v -> IntState (BT Int)
numberHelper Null = IntStateOf (\state -> (state, Null))
numberHelper (Node left val right) = do
    l <- numberHelper left
    state <- postInc
    r <- numberHelper right
    return (Node l state r)

-- Example: number albertTree = albertTreeNumbered, where:

albertTreeNumbered :: BT Int
albertTreeNumbered = Node (Node (Node Null 0 Null)
                                1
                                (Node Null 2 Null))
                          3
                          (Node Null
                                4
                                (Node Null 5 Null))

-- Not required, but good exercise: Re-implement numberHelper without Monad
-- operations.  I.e., it can be done with just postInc, Functor, and
-- Applicative.

-- Not required, but good exercise: Think up a problem similar to this, but with
-- a twist, and the twist forces you to use Monad operations.  And solve it.
