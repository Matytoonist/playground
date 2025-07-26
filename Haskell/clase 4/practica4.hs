succ' = \x -> x + 1
doble = \x -> x + x
cuadruple = \x -> x * 4
sumaDos =  \x -> x + 2
const = \x -> \y -> x
flip = \f -> \x -> \y -> f y x
(f . g) x = \f -> (\g -> x)
