{-
An implementation of Peter Landin's SECD machine
An abstract virtual machine for implementing functional languages
-}

data Primitive = Succ | IsZero deriving (Show)

data Term = Var String | Abs String Term | App Term Term
          | Prim Primitive | INT Int

data SContents = SContents Term | Closure String Term [EContents]
data EContents = EContents String SContents
data CContents = CContents Term | At
data DContents = DContents [SContents] [EContents] [CContents]
data SECDConfig = SECD ([SContents], [EContents], [CContents], [DContents])

true = Abs "x" (Abs "y" (Var "x"))
false = Abs "x" (Abs "y" (Var "y"))
lnot = Abs "b" (App (App (Var "b") false) true)
car = Abs "l" (App (Var "l") true)
cdr = Abs "l" (App (Var "l") false)
cons = Abs "h" (Abs "t" (Abs "s" (App (App (Var "s") (Var "h")) (Var "t"))))
nil = Abs "s" true

instance Show Term where
	show (Var v) = v
	show (Abs v (Abs x e)) = "(\\" ++ v ++ ".\\" ++ x ++ "." ++ show e ++ ")"
	show (Abs v e) = "(\\" ++ v ++ "." ++ show e ++ ")"
	show (App a (App b c)) = show a ++ " (" ++ show b ++ " " ++ show c ++ ")"
	show (App a b) = show a ++ " " ++ show b
	show (Prim p) = show p
	show (INT i) = show i

instance Show SContents where
	show (SContents t) = show t
	show (Closure s t _) = show s ++ ": " ++ show t

instance Show EContents where
	show (EContents s sc) = "<" ++ s ++ ", " ++ (show sc) ++ ">"

instance Show CContents where
	show (CContents t) = show t
	show At = "@"

instance Show DContents where
 	show (DContents s e d) = "dump"

instance Show SECDConfig where
	show (SECD (s, e, c, d)) = "S = " ++ (show s) ++ "\nE = " ++ (show e) ++ "\nC = " ++ (show c) ++ "\nD = " ++ (show d) ++ "\n"

lookUp :: String -> [EContents] -> SContents
lookUp s [] = undefined -- assume no free variables so lookUp should always succeed
lookUp s ((EContents x term):es) = if s == x then term else lookUp s es

applyPrim :: Primitive -> Term -> Term
applyPrim Succ (INT i) = INT (i + 1)
applyPrim IsZero (INT i) = if i == 0 then true else false

secdOneStep :: SECDConfig -> SECDConfig
secdOneStep (SECD (s, e, (CContents (Var x)):c, d))                   = SECD ((lookUp x e):s, e, c, d)
secdOneStep (SECD (s, e, (CContents (Abs x m)):c, d))                 = SECD ((Closure x m e):s, e, c, d)
secdOneStep (SECD (s, e, (CContents (App m n)):c, d))                 = SECD (s, e, (CContents n):(CContents m):At:c, d)
secdOneStep (SECD (s, e, (CContents (Prim p)):c, d))                  = SECD ((SContents (Prim p)):s, e, c, d)
secdOneStep (SECD (s, e, (CContents (INT i)):c, d))                   = SECD ((SContents (INT i)):s, e, c, d)
secdOneStep (SECD ((SContents (Prim p)):(SContents n):s, e, At:c, d)) = SECD (s, e, (CContents (applyPrim p n)):c, d)
secdOneStep (SECD ((Closure x m e1):n:s, e2, At:c, d))                = SECD ([], (EContents x n):e1, [(CContents m)], (DContents s e2 c):d)
secdOneStep (SECD ([m], e1, [], (DContents s e2 c):d))                = SECD (m:s, e2, c, d)

closureToTerm :: SContents -> Term
closureToTerm (Closure x m e) = Abs x (makeSubs e m)
closureToTerm _ = undefined -- Should be unreachable

makeSubs :: [EContents] -> Term -> Term
makeSubs [] m = m
makeSubs ((EContents y (SContents t)):es) m = subTerm m t y
makeSubs ((EContents y (Closure s t econt)):es) m = subTerm m (closureToTerm (Closure s t econt)) y


subTerm :: Term -> Term -> String -> Term
subTerm (Var y) e x = if y == x then e else (Var y)
subTerm (App m n) e x = App (subTerm m e x) (subTerm n e x)
subTerm (Abs y p) e x = if x == y then (Abs x p) else (Abs y (subTerm p e x))
subTerm (Prim prim) e x = Prim prim
subTerm (INT i) e x = INT i

secdRepeat :: SECDConfig -> Term
secdRepeat (SECD ([Closure x m e], e2, [], [])) = closureToTerm (Closure x m e)
secdRepeat (SECD ([SContents m], e, [], [])) = m
secdRepeat conf = secdRepeat $ secdOneStep conf

secdInit :: Term -> SECDConfig
secdInit term = SECD ([], [], [CContents term], [])

reduce :: Term -> Term
reduce t = secdRepeat $ secdInit t


-- Examples usages
answer1 = reduce (App (Abs "x" (Var "x")) (Abs "y" (Var "y")))
answer2 = reduce $ (App (App (App (Prim IsZero) (INT 0)) false) true)

t1 = App (App (Abs "x" (Var "x")) (Abs "y" (Var "y"))) (Var "z")

t2 = App (App (Var "x") (Abs "y" (Var "y"))) (Var "z")

t3 = App (App (Abs "x" (Var "x")) (Var "y")) (Var "z")

t4 = App (Abs "x" (Var "x")) (Abs "y" (Var "y"))

t5 = App
	(App
		(App
			(Var "x")
			(Var "y"))
		(App
			(Var "z")
			(Var "w")))
	(App (Var "a") (Var "b"))

t6 = App (Abs "x" (App (Var "x") (App (Var "x") (Var "x")))) (Var "y")
