module A2DPLL where

-- It's OK to import stuff from the standard library. Put your import lines here.

-- Question 2.

data Atom = Pos String | Neg String deriving Show

atomEq :: Atom -> Atom -> Bool
atomEq (Pos str1) (Pos str2) = str1 == str2
atomEq (Neg str1) (Neg str2) = str1 == str2
atomEq _ _ = False

atomNeq :: Atom -> Atom -> Bool
atomNeq x y = not (atomEq x y)

atomNeg :: Atom -> Atom
atomNeg (Pos str) = Neg str
atomNeg (Neg str) = Pos str

-- True if satisfiable, False if unsatisfiable.
cnfSAT :: [[Atom]] -> Bool
cnfSAT [] = True
cnfSAT ([]:_) = False
cnfSAT lst((x:_):_) = or (map cnfSAT [filtercnf lst x, filtercnf lst (atomNeg x)])

atomInLst :: Atom -> [Atom] -> Bool
atomInLst element lst = or (map (atomEq element) lst)

filtercnf lst x = map (filter (atomNeq nx)) reduced
    where reduced = filter (\y -> not (atomInLst x y)) lst
          nx = atomNeg x

examples = [[Pos "b", Pos "c"],
            [Pos "a", Neg "b", Neg "d"],
            [Neg "a", Pos "b", Neg "c", Pos "d"]]

