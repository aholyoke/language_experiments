{-
Maybe
List
IO
State
Writer

Reader
Continuation
Error
-}


import Data.Monoid
import qualified Data.ByteString as B

newtype Writer l a = Writer { runWriter :: (a, l) }

instance (Monoid l) => Monad (Writer l) where
	return x = Writer (x, mempty)
	(>>=) (Writer (a, l1)) g = Writer (
		let
			(b, l2) = runWriter (g a)
		in
			(b, l1 `mappend` l2)
		)


addNum :: Int -> Writer [String] Int
addNum n = Writer (n, ["Added: " ++ show n])

getProd :: [Int] -> Writer [String] Int
getProd [] = return 1
getProd (l:ls) = do
	n <- addNum l
	m <- getProd ls
	return (n * m)

answer = runWriter (getProd [3, 4, 5])
