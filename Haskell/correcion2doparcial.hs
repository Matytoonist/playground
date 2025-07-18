data BST2_3 k v = Cero
                | Dos Int (BST2_3 k v) (k,v) (BST2_3 k v)
                | Tres Int (BST2_3 k v) (k,v) (BST2_3 k v) (k,v) (BST2_3 k v)
                deriving Show

bstej = Dos 3 (Dos 2 (Dos 1 Cero (1,"c") Cero) (2,"b") 
                     (Tres 1 Cero (3,"d") Cero (4,"e")  Cero)) (5,"a") 
              (Dos 2 (Tres 1 Cero (6,"f") Cero (7,"g") Cero) (8,"e") (Tres 1 Cero (9,"h") Cero (10,"i") Cero))

--         5
--   2           8
-- 1   3 4   6 7   9 10
fold2_3 :: b -> (Int -> b -> (k,v) -> b -> b) -> (Int -> b -> (k,v) -> b -> (k,v) -> b -> b) -> BST2_3 k v -> b
fold2_3 c d t Cero = c
fold2_3 c d t (Dos h a1 x a2) = d h (fold2_3 c d t a1) x (fold2_3 c d t a2)
fold2_3 c d t (Tres h a1 x a2 y a3) = t h (fold2_3 c d t a1) x 
                                          (fold2_3 c d t a2) y
                                          (fold2_3 c d t a3)

size :: BST2_3 k v -> Int
size Cero                  = 0
size (Dos h a1 x a2)       = 1 + size a1 + size a2
size (Tres h a1 x a2 y a3) = 2 + size a1 + size a2 + size a3

keysInOrder :: BST2_3 k v -> [k]
keysInOrder Cero                              = []
keysInOrder (Dos h a1 (k,v) a2)               = keysInOrder a1 ++ [k] ++ keysInOrder a2
keysInOrder (Tres h a1 (k1,v1) a2 (k2,v2) a3) = keysInOrder a1 ++ [k1] ++ keysInOrder a2 ++ [k2] ++ keysInOrder a3

height :: BST2_3 k v -> Int
height Cero                  = 0
height (Dos h a1 x a2)       = 1 + max (height a1) (height a2)
height (Tres h a1 x a2 y a3) = 1 + max (height a1) (max (height a2) (height a3))

claveMenor :: BST2_3 k v -> k
--Precondicion: el arbol debe ser válido y no vacío
claveMenor Cero = error "arbol vacío"
claveMenor (Dos n a1 (k,v) a2) = case n of
				      1 -> k
				      _ -> claveMenor a1
claveMenor (Tres n a1 (k1,v1) a2 (k2,v2) a3) = case n of
						    1 -> k1
						    _ -> claveMenor a1

claveMayor :: BST2_3 k v -> k
claveMayor Cero = error "arbol vacío"
claveMayor (Dos n a1 (k,v) a2) = case n of
				      1 -> k
				      _ -> claveMayor a2
claveMayor (Tres n a1 (k1,v1) a2 (k2,v2) a3) = case n of
						    1 -> k2
						    _ -> claveMenor a3


search :: Ord k => BST2_3 k v -> k -> Maybe v
search Cero = Nothing
search (Dos n a1 (k,v) a2) sk = if sk == s then Just v
				 	   else if sk < k
						then search a1 sk
						else search a2 sk
search (Tres n a1 (k1,v1) a2 (k2,v2) a3) sk = if sk == k1 then Just v1
							  else if sk < k1
							       then search a1 sk
						    	       else if sk == k2 then Just v2
                                                                                else if sk < k2
                                                                                        then search a2 sk
                                                                                        else search a3 sk
