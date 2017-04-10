import Control.Category
import Control.Arrow
import Control.Monad.State.Lazy

type MyMap = (String -> Maybe String)
data Term = Var String | Abs String Term | App Term Term deriving Show


--renameVar :: (Eq a, MonadState (a -> Maybe a) m) => a -> a -> m Bool
renameVar v1 v2 = state (
	\f -> case (f v2) of
		Nothing  -> (True, if v1 == v2 then f else (\y -> if y == v2 then Just v1 else f y))
		(Just m) -> (m == v1, f)
	)


--alphaEq :: Term -> Term -> State MyMap Bool
--        :: (Arrow a, MonadState (String -> Maybe String) (a b)) => Term -> Term -> a b Bool
alphaEq (Var v1) (Var v2) = renameVar v1 v2
alphaEq (Abs v1 e1) (Abs v2 e2) = do
	succ <- renameVar v1 v2
	if succ then alphaEq e1 e2 else return False
alphaEq (App e1 e2) (App e3 e4) = do
	(b1, b2) <- (alphaEq e1 e3) &&& (alphaEq e2 e4)
	return (b1 && b2)
alphaEq _ _ = return False


-- runState :: State s a -> s -> (a, s)
-- runState :: State b Bool -> b -> (Bool, b)

-- runState (alphaEq e1 e2) (\_ -> Nothing)
-- b = t -> Maybe a


--alphaEqual :: Eq t => Term t -> Term t -> Bool
--alphaEqual e1 e2 = evalState (\_ -> Nothing) (alphaEq e1 e2) 

t1 = Abs "x" (Var "x")
t2 = Abs "y" (Var "y")

main = do
	print "Hello"