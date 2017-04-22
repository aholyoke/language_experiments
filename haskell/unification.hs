{-
Monadic implementation of unification algorithm

Call mgci (Most general common instance) on two terms and it will
attempt to unify them by instantiating the variables


Use the V constructor for variables and the T constructor for function symbols
Constants have arity 0 so they can be represented by T's with an empty list

Examples at the end
-}
data Term = T String [Term] | V String deriving (Eq, Show)
type Substitution = (Term, Term)


mgci :: Term -> Term -> Maybe Term
mgci t1 t2 = unify t1 t2 >>= return . (applySubs t1)


applySub :: Substitution -> Term -> Term
applySub ((T name1 terms), (V name2)) (V name3) = if name2 == name3 then T name1 terms else V name3
applySub sub (T name terms) = T name (map (applySub sub) terms)


applySubs :: Term -> [Substitution] -> Term
applySubs = foldl (flip applySub)


unifySequence :: [Term] -> [Term] -> Maybe [Substitution]
unifySequence [] [] = Just []
unifySequence (t1:t1s) [] = Nothing
unifySequence [] (t2:t2s) = Nothing
unifySequence (t1:t1s) (t2:t2s) = do
	sub <- unify t1 t2
	rest <- unifySequence (map (flip applySubs sub) t1s) (map (flip applySubs sub) t2s)
	return $ sub ++ rest

occursHelp (V name1) (V name2) = name1 == name2
occursHelp (V name1) (T name2 ts) = foldr ((||) . (occursHelp (V name1))) False ts

occurs :: Term -> Term -> Bool
occurs (V name1) (V name2) = False
occurs x y = occursHelp x y

unify :: Term -> Term -> Maybe [Substitution]
unify (T name1 []) (T name2 []) = if name1 == name2 then Just [] else Nothing
unify (T name1 []) (T name2 (t:ts)) = Nothing
unify (T name1 (s:ss)) (T name2 []) = Nothing
unify (V name1) (T name2 terms2) = if occurs (V name1) (T name2 terms2) then Nothing else Just $ [((T name2 terms2), (V name1))]
unify (T name2 terms2) (V name1) = if occurs (V name1) (T name2 terms2) then Nothing else Just $ [((T name2 terms2), (V name1))]
unify (T name1 (t1:t1s)) (T name2 (t2:t2s)) = if name1 == name2 then unifySequence (t1:t1s) (t2:t2s) else Nothing


t1 = T "hello" [T "goodbye" [], V "X"]
t2 = T "hello" [V "Y", T "greetings" []]

t3 = T "R3" [T "R2" [V "X",    T "b" []], T "R1" [V "X"]] -- R3(R2(X, b), R1(X))
t4 = T "R3" [T "R2" [T "a" [], T "b" []], V "Y"]          -- R3(R2(a, b), Y)

t5 = T "R3" [T "R2" [V "X",    T "b" []], T "R1" [V "X"]] -- R3(R2(X, b), R1(X))
t6 = T "R3" [T "R2" [T "a" [], T "b" []], V "X"]          -- R3(R2(a, b), X))

t7 = T "f" [V "X"]
t8 = V "X"
