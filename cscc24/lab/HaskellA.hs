-- CSCC24 Lab Week 5: Haskell exercises part A.
--
-- The marks in each question include style marks (30%).
-- 
-- Due Friday 6PM on MarkUs: https://markus.utsc.utoronto.ca/cscc24w18/

module HaskellA where

-- [1 mark]
-- Take two list of integers.  Precondition: Each is already sorted (non-decreasing).
-- Perform the "merge" of "mergesort".  Linear time.
-- Example:
-- merge [2, 3, 5] [1, 3, 4] = [1, 2, 3, 3, 4, 5]
merge :: [Integer] -> [Integer] -> [Integer]
merge lst1 lst2
    |or (null lst1, null lst2) = lst1 ++ lst2
    |lst1 > lst2 = head lst1 : (merge (tail lst1) lst2) 
    |otherwise = head lst2 : (merge lst1 (tail lst2))


-- [1 mark]
-- The standard library has "reverse" for reversing a list.
-- Example in uninspiring notation: reverse [1, 2, 3] = [3, 2, 1]
--
-- In more inspiring notation:
-- reverse (1 : (2 : (3 : []))) = 3 : (2 : (1 : []))
--
-- This is "obviously" a linear-time foldl.  Do it.
--
-- Hint: Don't just look at left-vs-right.  Inner-vs-outer is more important.
myreverse :: [a] -> [a]
myreverse xs = foldl revop [] xs
revop x y = y : x


-- [1 mark]
-- Kind-of cartesian product of two lists and apply a function.  Example
-- to clarify:
--
--   listCross f [10, 20, 30] [1, 2]
-- = [f 10 1, f 10 2, f 20 1, f 20 2, f 30 1, f 30 2]
--
-- If any list is empty, the whole answer is empty too.
-- (What else could you do anyway?)
--
-- How to do this?  Recognize that you're looking at:
--
--   listCross f (10 : [20, 30]) [1, 2]
-- = [f 10 1, f 10 2] ++ the rest
--
-- ("++" concatenates two lists.)
--
-- Do you see what to do now?
listCross :: (a -> b -> c) -> [a] -> [b] -> [c]
listCross f lst1 lst2 = foldl binop [] lst1
    where binop x y = x ++ (map (f y) lst2) 



-- [2 marks]
-- foldl can be expressed as foldr too!
-- It is not "obvious", but it's a bit easier if you change the parameter
-- order to put the "accumulator" as the last parameter:
--
-- myfoldl f [] a = a
-- myfoldl f (x : xs) a = myfoldl f xs (f a x)
--
-- Hint: Think of "foo x y = ..." as "foo x = \y -> ..."
myfoldl :: (b -> a -> b) -> [a] -> b -> b
myfoldl f xs = foldr (\param func x -> func (f x param))
                     (\a -> a)
                     xs
