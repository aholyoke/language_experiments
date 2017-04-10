import Control.Category
import Control.Arrow


data State s a = ST (s -> (a, s))

instance Monad (State s) where
	return x = ST (\s -> (x, s))
	(>>=) (ST f) g = ST (\s0 ->
		let
			(a, s1) = f s0
			(ST h) = g a
			(b, s2) = h s1
		in (b, s2))

instance Category State where
	-- id :: State a a
	id = ST (\s -> (s, s))
	-- (.) :: State b c -> State a b -> State a c
	(.) (ST s1) (ST s2) = ST (\a1 ->
		let
			(b1, a2) = s2 a1
			(c1, b2) = s1 b1
		in
			-- perhaps? (c1 . b1, a1))
			(c1, a2))

instance Arrow State where
	-- arr :: (a -> b) -> State a b
	arr f = ST (\s -> (f s, s))
	-- first :: State a b -> State (a, c) (b, c)
	-- first :: (a -> (b, a)) -> ((a, c) -> ((b, c), (a, c)))
	first (ST s1) = ST(\(a, c) ->
		let
			(b1, a1) = s1 a
		in
			((b1, c), (a1, c)))


data Tree a = Empty | Node a (Tree a) (Tree a) deriving Show


inc :: State Int ()
inc = ST (\s -> ((), s + 1))

numNodes :: Tree a -> State Int ()
numNodes Empty = return ()
numNodes (Node _ left right) = do
	inc
	numNodes left
	numNodes right


type MyState a = ((a -> Maybe Int), Int)

-- t -> ST (t -> Maybe Int, Int) -> (Int, (t -> Maybe Int, Int))
numberNode :: Eq t => t -> State (MyState t) Int
numberNode x = ST (
	-- f :: t -> Maybe Int
	-- n :: Int
	-- x :: t
	\ (f, n) ->
		if f x == Nothing then -- new node
			(n, (\y -> if y == x then Just n else f y, n + 1))
			-- f :: Maybe Int
			-- :: (Int, t -> Maybe Int, Int)
		else
			let Just m = f x -- m :: Int
			in (m, (f, n)) -- :: (Int, (t -> Maybe Int, Int)
	)


-- numberNode a :: m Int
numberTree :: Eq t => Tree t -> State (MyState t) (Tree Int)
numberTree Empty = return Empty
numberTree (Node a left right) = do
	n <- numberNode a
	nt1 <- numberTree left
	nt2 <- numberTree right
	return (Node n nt1 nt2)


numTree :: Eq t => Tree t -> Tree Int
numTree Empty = Empty
numTree x = let
		(ST f) = numberTree x
		(new, _) = f (\ _ -> Nothing, 0)
	in
		new


t = (Node 'a'
		(Node 'b' Empty Empty)
		(Node 'c'
			(Node 'a' Empty Empty)
			Empty))


type Mapping t = (t -> Maybe t)
data LC t = Var t | Abs t (LC t) | App (LC t) (LC t)
newtype MyString = MS String deriving Eq


instance Show MyString where
	show x = let (MS y) = x in y


instance (Show t) => Show (LC t) where
	show (Var v) = show v
	show (Abs v (Abs x e)) = "(\\" ++ show v ++ ".\\" ++ show x ++ "." ++ show e ++ ")"
	show (Abs v e) = "(\\" ++ show v ++ "." ++ show e ++ ")"
	show (App a (App b c)) = show a ++ " (" ++ show b ++ " " ++ show c ++ ")"
	show (App a b) = show a ++ " " ++ show b


instance Eq t => Eq (LC t) where
	(==) e1 e2 = let
			-- return true if variable could be renamed successfully (Or didnt need to be renamed)
			-- and update the mapping if necessary
			renameVar :: Eq t => t -> t -> State (Mapping t) Bool
			renameVar v1 v2 = ST (
				\f -> case (f v2) of
					-- if v2 not in mapping then succeed, if v1 != v2 then add v2->v1 to mapping
					Nothing  -> (True, if v1 == v2 then f else (\y -> if y == v2 then Just v1 else f y))
					-- if v2 in mapping then succeed iff result value is equal to v1
					(Just m) -> (m == v1, f)
				)
			alphaEq :: Eq t => LC t -> LC t -> State (Mapping t) Bool
			alphaEq (Var v1) (Var v2) = renameVar v1 v2 -- if v1 != v2 and v2 not in mapping then add v2->v1 to mapping
			alphaEq (Abs v1 e1) (Abs v2 e2) = do
				succ <- renameVar v1 v2 -- if v1 != v2 and v2 not in mapping then add v2->v1 to mapping
				if succ then alphaEq e1 e2 else return False
			--alphaEq (App e1 e2) (App e3 e4) = do
			--	succ1 <- alphaEq e1 e3
			--	succ2 <- alphaEq e2 e4
			--	return (succ1 && succ2)
			alphaEq (App e1 e2) (App e3 e4) = do
				(b1, b2) <- (alphaEq e1 e3) &&& (alphaEq e2 e4)
				return (b1 && b2)
			alphaEq _ _ = return False -- expressions have different structure
			(ST f) = alphaEq e1 e2
			(b, map) = f (\_ -> Nothing)
		in
			b

-- State (Mapping t) Bool -> State (Mapping t) Bool -> State (Mapping t) (Bool, Bool)

instance Functor LC where
	fmap f (Var t) = Var (f t)
	fmap f (Abs t e) = Abs (f t) (fmap f e)
	fmap f (App a b) = App (fmap f a) (fmap f b)


main = do
	putStrLn $ show $ (==) -- True
		(Abs "x" (Abs "y" (App (Var "x") (Var "y"))))
		(Abs "y" (Abs "x" (App (Var "y") (Var "x"))))
	putStrLn $ show $ (==) -- False
		(Abs "x" (Abs "y" (App (Var "x") (Var "y"))))
		(Abs "y" (Abs "x" (App (Var "y") (Var "y"))))
	putStrLn $ show $ (==) -- True
		(Abs "x" (Abs "x" (App (Var "x") (Var "x"))))
		(Abs "y" (Abs "y" (App (Var "y") (Var "y"))))
	putStrLn $ show $ (==) -- False
		(Abs "x" (Abs "y" (App (Var "y") (Var "y"))))
		(Abs "y" (Abs "x" (App (Var "y") (Var "y"))))
	putStrLn $ show $ (==) -- True
		(App (Abs "y" (Var "y")) (Abs "x" (Var "x")))
		(App (Abs "x" (Var "x")) (Abs "x" (Var "x")))
