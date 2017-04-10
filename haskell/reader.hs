
newtype Reader e a = Reader { runReader :: (e -> a) }
 
instance Monad (Reader e) where 
    return a = Reader (\e1 -> a)
    (>>=) (Reader r) f = Reader (\e1 -> runReader (f (r e1)) e1)



env = Reader (\x -> if x == 'a' then 1 else if x == 'b' then 3 else 0)
