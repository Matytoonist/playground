suma :: Int -> Int -> Int
suma x y = x + y

succ = suma 1

twice :: (a -> a) -> (a -> a)
twice f = \x -> f (f x)

map' :: (a->b)->([a]->[b])
map' f [] = []
map' f (x:xs) = f x : map f xs

sum :: [Int] -> Int
sum [] = 0
sum (x:xs) = x + sum xs

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f z [] = z
foldr f z (x:xs) = f x (foldr f z xs)

partition :: (a -> bool) -> [a] -> ([a],[a])
partition p [] = ([],[])
partition p (x:xs) = let (bs,ms) = partition p xs
			in if p x
			then (x:bs,ms)
			else (bs,x:ms)
--ayudaaaaa

--scanr:: 

data ExpA = Cte Int | Suma ExpA ExpA | Prod ExpA ExpA

foldEpxA :: (Int -> b) -> (b -> b -> b) -> (b -> b -> b) -> ExpA -> b
foldExpA c s p (Cte n)      = c n
foldExpA c s p (Suma e1 e2) = s (foldExpA c s p e1) (foldExpA c s p e2)
foldExpA c s p (Prod e1 e2) = p (foldExpA c s p e1) (foldExpA c s p e2)

recExpA::(Int -> b) -> (b->b->b) -> (b->b->b) -> ExpA -> b
recExpA c s p (Cte n)      = c n
recExpA c s p (Suma e1 e2) = s e1 (recExpA c s p e1) e2 (recExpA c s p e1)
recExpA c s p (Prod e1 e2) = p e1 (recExpA c s p e1) e2 (recExpA c s p e1)

showExpA :: ExpA -> String
showExpA = foldExpA (\n -> "(Const " ++ show n ++ ")")
                    (\s1 s2 -> "(Suma " ++ s1 ++ " " ++ s2 ++ ")")
                    (\s1 s2 -> "(Prod " ++ s1 ++ " " ++ s2 ++ ")")

cantidadSumaCero :: ExpA -> Int
cantidadSumaCero = recExpA (\n -> 0)
                           (\e1 n1 e2 n2 -> n1 + n2 + unoSi (esCero e1 || esCero e2)
                           (\e1 n1 e2 n2 -> n1 + n2)
