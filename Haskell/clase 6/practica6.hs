data ExpA  = Cte Int
           | Suma ExpA ExpA
           | Prod ExpA ExpA
{-
foldExpA :: (Int -> b) -> (Int -> b -> b) -> (Int -> b -> b) -> ExpA -> b
foldExpA c s p (Cte n)          = c n
foldExpA c s p (Suma exp1 exp2) = s (foldEpxA c s p exp1) (foldEpxA c s p exp2)
foldExpA c s p (Prod exp1 exp2) = p (foldEpxA c s p exp1) (foldEpxA c s p exp2)
-}
{-
-- un caso por cada constructor de ExpA
foldExpA ::                      -> ExpA -> b
foldExpA ... (Cte num)          = ... num ...
foldExpA ... (Suma exp1 exp2)   = ... exp1 ... exp2 ...
foldExpA ... (Prod exp1 exp2)   = ... exp1 ... exp2 ...

-- paso cada "espacio" como parametro
un caso por cada constructor de ExpA
foldExpA ::                      -> ExpA -> b
foldExpA c s p (Cte num)          = c num
foldExpA c s p (Suma exp1 exp2)   = s (foldExpA c s p exp1) (foldExpA c s p exp2) -- caso recursivo
foldExpA c s p (Prod exp1 exp2)   = p (foldExpA c s p exp1) (foldExpA c s p exp2)

-- completo el tipo de cada parámetro. c = funcion que toma Int y devuelve B. s = funcion que toma 2 b (resultado de la recursión) y devuelve b. p = ídem s

foldExpA ::(Int -> b) -> (b -> b -> b) -> (b -> b -> b) -> ExpA -> b
foldExpA c s p (Cte num)          = c num
foldExpA c s p (Suma exp1 exp2)   = s (foldExpA c s p exp1) (foldExpA c s p exp2)
foldExpA c s p (Prod exp1 exp2)   = p (foldExpA c s p exp1) (foldExpA c s p exp2)
-}

foldExpA ::(Int -> b) -> (b -> b -> b) -> (b -> b -> b) -> ExpA -> b
foldExpA c s p (Cte num)          = c num
foldExpA c s p (Suma exp1 exp2)   = s (foldExpA c s p exp1) (foldExpA c s p exp2)
foldExpA c s p (Prod exp1 exp2)   = p (foldExpA c s p exp1) (foldExpA c s p exp2)


cantidadDeCeros :: ExpA -> Int
cantidadDeCeros = foldExpA c s p
                    where c x = unoSiCeroSiNo (x == 0)
                          s exp1 exp2 = exp1 + exp2 
                          p exp1 exp2 = exp1 + exp2

cantidadDeCeros' ::ExpA -> Int
cantidadDeCeros' = foldExpA (\x -> unoSiCeroSiNo (x == 0)) (+) (+)

unoSiCeroSiNo :: Bool -> Int
unoSiCeroSiNo condicion = if condicion then 1 else 0

noTieneNegativosExplicitos :: ExpA -> Bool
noTieneNegativosExplicitos = foldExpA c s p
                        where c x = x >= 0
                              s exp1 exp2 = exp1 && exp2 
                              p exp1 exp2 = exp1 && exp2

noTieneNegativosExplicitos' :: ExpA -> Bool
noTieneNegativosExplicitos' = foldExpA (\x -> x >= 0) (&&) (&&)

simplificarExpA :: ExpA -> ExpA
simplificarExpA = foldExpA Cte armarSuma armarProd

armarSuma :: ExpA -> ExpA -> ExpA
armarSuma (Cte 0) e2 = e2
armarSuma e1 (Cte 0) = e1
armarSuma e1 e2 = Suma e1 e2

armarProd :: ExpA -> ExpA -> ExpA
armarProd (Cte 1) e2 = e2
armarProd e1 (Cte 1) = e1
armarProd e1 e2 = Prod e1 e2

evalExpA :: ExpA -> Int
evalExpA = foldExpA c s p
                  where c x = x
                        s n1 n2 = n1 + n2
                        p n1 n2 = n1 * n2

evalExpA' :: ExpA -> Int
evalExpA' = foldExpA (id) (+) (*)

showExpA :: ExpA -> String
showExpA = foldExpA c s p
                  where c x = "Cte " ++ show x
                        s exp1 exp2 = "Suma (" ++ exp1 ++ ") (" ++ exp2 ++ ")"
                        p exp1 exp2 = "Prod (" ++ exp1 ++ ") (" ++ exp2 ++ ")"

showExpA' :: ExpA -> String
showExpA' = foldExpA (\x -> "Cte " ++ show x) 
                     (\s1 s2 -> "Suma (" ++ s1 ++ ") (" ++ s2 ++ ")") 
                     (\s1 s2 -> "Prod (" ++ s1 ++ ") (" ++ s2 ++ ")")


recExpA ::(Int -> b) -> (ExpA -> b -> ExpA -> b -> b) -> (ExpA -> b -> ExpA -> b -> b) -> ExpA -> b
recExpA c s p (Cte num)          = c num
recExpA c s p (Suma exp1 exp2)   = s  exp1 (recExpA c s p exp1) exp2 (recExpA c s p exp2)
recExpA c s p (Prod exp1 exp2)   = p  exp1 (recExpA c s p exp1) exp2 (recExpA c s p exp2)

{-cantSumaCeros :: ExpA -> Int
cantSumaCeros = recExpA (\c -> 0) 
                        (\s exp1 e1 exp2 e2 -> (unoSiCeroSiNo (esCte 0 exp1 || esCte 0 exp2)) + e1 + e2) 
                        (\p exp1 e1 exp2 e2 -> e1 + e2)

--cantProdUnos :: ExpA -> Int
cantProdUnos :: ExpA -> Int
cantProdUnos = recExpA  (\c -> 0) 
                        (\s exp1 e1 exp2 e2 -> e1 + e2)
                        (\p exp1 e1 exp2 e2 -> unoSiCeroSiNo (esCte 1 exp1 
                                                           || esCte 1 exp2) + e1 + e2) 
-}
esCte :: Int -> ExpA -> Bool
esCte n (Cte x) = x == n
esCte n exp = False

-- parte 2

data EA = Const Int | BOp BinOp EA EA
data BinOp = Sum | Mul

foldEA :: (Int -> b) -> (BinOp -> b -> b -> b) -> EA -> b
foldEA c bp (Const num)       = c num
foldEA c bp (BOp op e1 e2)    = bp op (foldEA c bp e1) (foldEA c bp e2)

noTieneNegativosExplicitosEA :: EA -> Bool
noTieneNegativosExplicitosEA = foldEA (\x -> x >= 0) (\op ex1 ex2 -> ex1 && ex2)

--simplificarEA (no la puedo pensar ahora)

{- (preguntar como era la sintaxis para usar case-of en este caso)
evalEA :: EA -> Int
evalEA = foldEA (\x -> x) (case op of Sum ex1 ex2 = ex1 + ex2)
-}

--todo el resto de la 6 medio requiere que sepa esto asi que uhhh

--7 tengo que preguntar lo del recr
recEA :: (Int -> b) -> (BinOp -> EA -> b -> EA -> b -> b) -> EA -> b
recEA c bp (Const num)       = c num
recEA c bp (BOp op e1 e2)    = bp op e1 (recEA c bp e1) e1 (recEA c bp e2)

--8 A tambien

--8b requiere recExp

data Tree a = ET | NT a (Tree a) (Tree a) deriving Show

tej = NT 1  (NT 2 (NT 4 (NT 8 ET ET) (NT 9 (NT 18 ET ET) ET)) (NT 5 (NT 10 ET ET) ET)) (NT 3 (NT 6 ET (NT 13 ET ET)) (NT 7 (NT 14 ET ET) (NT 15 ET ET)))

btej = NT 5 (NT 3 (NT 2 (NT 1 ET ET) ET) (NT 4 ET ET)) (NT 7 (NT 6 ET ET) (NT 9 (NT 8 ET ET) (NT 10 ET ET)))

foldT :: b -> (a -> b -> b -> b) -> Tree a -> b
foldT g f ET           = g
foldT g f (NT x t1 t2) = f x (foldT g f t1) (foldT g f t2)

mapT :: (a -> b) -> Tree a -> Tree b
mapT f = foldT ET (\x t1 t2 -> NT (f x) t1 t2)

sumT :: Tree Int -> Int
sumT = foldT 0 (\n t1 t2 -> n + t1 + t2)

sizeT :: Tree a -> Int
sizeT = foldT 0 (\x t1 t2 -> 1 + t1 + t2)

heightT :: Tree a -> Int
heightT = foldT 0 (\x t1 t2 -> 1 + max t1 t2)

preOrder :: Tree a -> [a]
preOrder = foldT [] (\x t1 t2 -> [x] ++ t1 ++ t2)

inOrder :: Tree a -> [a]
inOrder = foldT [] (\x t1 t2 -> t1 ++ [x] ++ t2)

postOrder :: Tree a -> [a]
postOrder = foldT [] (\x t1 t2 -> t1 ++ t2 ++ [x])

mirror :: Tree a -> Tree a
mirror = foldT ET (\x t1 t2 -> NT x t2 t1)

countByT :: (a -> Bool) -> Tree a -> Int
countByT prec = foldT 0 (\x t1 t2 -> if prec x then 1 + t1 + t2 else t1 + t2)
{-
partitionT :: (a -> Bool) ->Tree a -> ([a],[a])
partitionT prec = foldT [] (\x t1 t2 -> if prec x then ([x],[]))

zipWithT

caminoMasLargo

todosLosCaminos

todosLosNiveles

nivelN
-}

recT :: b -> (a -> Tree a -> b -> Tree a -> b -> b) -> Tree a -> b
recT g f ET           = g
recT g f (NT x t1 t2) = f x t1 (recT g f t1)
                            t2 (recT g f t2)

{-
insertT :: a -> Tree a -> Tree a
insertT elem = recT ET (\x t1 e1 t2 e2 -> if elem > e1 then NT elem t1 else t2)

caminoHasta
-}

data Dep a  = SinSalida
            | Almacen [a]
            | Siga (Dep a)
            | Bif (Dep a) (Dep a)
            | Hub (Dep a) (Dep a) (Dep a) (Dep a)

type Guia = [Indicacion]
data Indicacion = Seguir | IrAIzq | IrADer | UsarPuerta Int

dej = Hub (Bif (Siga (Almacen [2,3])) (Bif (Siga SinSalida) (Almacen [17]))) (Siga (Siga (Siga SinSalida))) (Siga (Bif (SinSalida) (Almacen [99,8]))) (Bif (Hub SinSalida (Almacen [42]) (Siga SinSalida) (Siga (Almacen []))) (Siga SinSalida))

foldD :: (a -> b) -> ([a]->b) -> (b -> b) -> (b -> b -> b) -> (b -> b -> b -> b -> b) -> Dep a -> b
foldD ss a s b h SinSalida                  = ss
foldD ss a s b h (Almacen ls)               = a ls
foldD ss a s b h (Siga dep)                 = s (foldD ss a s b h dep)
foldD ss a s b h (Bif dep1 dep2)            = b (foldD ss a s b h dep1) 
                                                (foldD ss a s b h dep2)
foldD ss a s b h (Hub dep1 dep2 dep3 dep4)  = h (foldD ss a s b h dep1) 
                                                (foldD ss a s b h dep2) 
                                                (foldD ss a s b h dep3) 
                                                (foldD ss a s b h dep4)

recD :: (a -> b) -> ([a]->b) -> (b -> Dep a -> b) -> (b -> Dep a -> b -> Dep a -> b) -> (b -> Dep a -> b -> Dep a-> b -> Dep a -> b -> Dep a -> b) -> Dep a -> b
recD ss a s b h SinSalida                  = ss
recD ss a s b h (Almacen ls)               = a ls
recD ss a s b h (Siga dep)                 = s  (recD ss a s b h dep) dep
recD ss a s b h (Bif dep1 dep2)            = b  (recD ss a s b h dep1) dep1
                                                (recD ss a s b h dep2) dep2
foldD ss a s b h (Hub dep1 dep2 dep3 dep4) = h  (recD ss a s b h dep1) dep1
                                                (recD ss a s b h dep2) dep2
                                                (recD ss a s b h dep3) dep3
                                                (recD ss a s b h dep4) dep4