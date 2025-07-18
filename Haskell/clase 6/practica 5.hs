suma :: Int -> Int -> Int
suma x y = x + y

succ = suma 1

twice :: (a -> a) -> (a -> a)
twice f = \x -> f (f x)

map' :: (a->b)->([a]->[b])
map' f [] = []
map' f (x:xs) = f x : map f xs

sum' :: [Int] -> Int
sum' [] = 0
sum' (x:xs) = x + sum' xs

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f z [] = z
foldr' f z (x:xs) = f x (foldr' f z xs)

partition :: (a -> Bool) -> [a] -> ([a],[a])
partition p [] = ([],[])
partition p (x:xs) = let (bs,ms) = (partition p xs) in if p x then (x:bs,ms) else (bs,x:ms)

--partition' p = foldr (\x -> if p x then (x:bs,ms) else (bs,x:ms)) ([],[])

elem :: Eq a => a -> [a] -> Bool
elem x = any (\y -> y == x)

{-
elem' :: Eq a => a -> [a] -> Bool
elem' x [] = False
elem' x (y:ys) = x == y || elem' x ys
-}

-- poderEntrenador :: Entrenador -> Int

