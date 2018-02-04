module A2Trie where

import           Data.Char (ord, chr)
import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

-- It's OK to import stuff from the standard library. Put your import lines here.

-- The following two utility functions will help you.  They treat IntMap as a
-- map from Char.
charMapLookup :: Char -> IntMap a -> Maybe a
charMapLookup c dict = IntMap.lookup (ord c) dict

charMapInsert :: Char -> a -> IntMap a -> IntMap a
charMapInsert c v dict = IntMap.insert (ord c) v dict

-- Question 1.
data Trie a = TrieOf (Maybe a) (IntMap (Trie a))
    deriving Show

trieLookup :: [Char] -> Trie a -> Maybe a
trieLookup (str)  (TrieOf val branches)
        | null (str) = val
        | otherwise = case mapElmt of
                        Nothing -> Nothing
                        Just x -> trieLookup (tail str) x
    where mapElmt = charMapLookup (head str) branches

emptyMap = IntMap.fromList []
emptyTrie = TrieOf Nothing emptyMap

absTrie :: Maybe (Trie a) -> Trie a
absTrie trie = case trie of
                Nothing -> emptyTrie
                Just x -> x

trieInsert :: [Char] -> a -> Trie a -> Trie a
trieInsert (str) newval (TrieOf val branches)
        | null (str) = TrieOf (Just newval) branches
        | otherwise = TrieOf val newbranches
    where newbranches = (charMapInsert (head str) 
                                       (trieInsert (tail str) 
                                                    newval
                                                   (absTrie (charMapLookup (head str) branches)))
                                        branches)

initMap char var = charMapInsert char var emptyMap
t = TrieOf (Just 9) (emptyMap)
i = TrieOf (Just 1) (initMap 't' t) 
p = TrieOf Nothing (initMap 'i' i)
p2 = TrieOf (Just 5) (emptyMap)
n = TrieOf (Just 7) (emptyMap)
o = TrieOf Nothing (charMapInsert 'p' p2 (initMap 'n' n))
t2 = TrieOf Nothing (initMap 'o' o)
root = TrieOf Nothing (charMapInsert 'p' p (initMap 't' t2))
buildlist = [("pit", 9), ("pi", 1), ("top", 5), ("tin", 7)]
loadIns tuple = trieInsert (fst tuple) (snd tuple)
root2 = foldr loadIns emptyTrie buildlist

test str = trieLookup str root

testcases = ["p", "pi", "pit", "pot", [], "t", "to", "top", "ton"]
testresult = map (\x -> (x, test x)) testcases
main = do
  show testresult
