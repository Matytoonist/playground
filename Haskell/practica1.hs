--Dado un número devuelve su sucesor
sucesor :: Int -> Int
sucesor n = n+1

--Dados dos números devuelve su suma utilizando la operación +
sumar :: Int -> Int -> Int
sumar x y = x + y

{- Dado dos números, devuelve un par donde la primera componente es la división del
primero por el segundo, y la segunda componente es el resto de dicha división. Nota:
para obtener el resto de la división utilizar la función mod :: Int -> Int -> Int,
provista por Haskell.-}
divisionYResto :: Int -> Int -> (Int, Int)
divisionYResto x y = (div x y,mod x y)

maxDePar :: (Int,Int) -> Int
maxDePar (n,m) =
	if (n > m)
	 then n
	 else m

data DiaDeSemana = Lunes | Martes | Miercoles | Jueves | Viernes | Sábado | Domingo
    deriving Show

siempreSiete :: x -> Int
siempreSiete _ = 7

swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)

estaVacia :: [a] -> Bool
estaVacia [] = True
estaVacia _ = False

elPrimero :: [a] -> a
elPrimero (x:_) = x

sinElPrimero :: [a] -> [a]
sinElPrimero (_:xs) = xs
