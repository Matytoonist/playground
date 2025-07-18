doble :: Int -> Int
doble x = x+x

cuadruple :: Int -> Int
cuadruple x = doble (doble x)

multiplicar :: Int -> Int -> Int
multiplicar x y = x*y
