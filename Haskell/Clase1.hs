doble :: Int -> Int
doble n = n*2

{- Gobstones
function sumar(n,m) { 
  return (n+m) 
}
-}
-- Haskell
sumar :: Int -> Int -> Int
sumar n m = n+m

{- Gobstones
function dos() {
	return (2)
}
-}
-- Haskell
dos :: Int
dos = 2

signo :: Int -> Int 
signo n = if (n==0)     then 0
          else if (n>0) then 1
          else              -1

divisionEntera :: Int -> Int -> Int
-- PRECOND: m no es 0
divisionEntera n m = div n m

divisionEntera' :: Int -> Int -> Int
-- PRECOND: m no es 0
divisionEntera' n m = if (m==0) 
                      then error "No puedo dividir por 0"
                      else div n m

data Dir = Norte | Este | Sur | Oeste
     deriving Show

{- Gobstones
function siguienteDir(d) {
  return(choose Este  when (d==Norte)
                Sur   when (d==Este)
                Oeste when (d==Sur)
                Norte when (d==Oeste)
                boom("Inválido") otherwise)
}  

function siguienteDir(d) {
  return(matching (d) select
           Este  on Norte
           Sur   on Este
           Oeste on Sur
           Norte on Oeste
           boom("Inválido") otherwise)
}  
-}

siguienteDir :: Dir -> Dir
siguienteDir Norte = Este
siguienteDir Este  = Sur
siguienteDir Sur   = Oeste
siguienteDir Oeste = Norte
--siguienteDir d     = error "NO PUEDE PASAR: ¿metiste mano al tipo Dir?"
     
siguienteDir' :: Dir -> Dir
siguienteDir' d = 
   case d of
    Norte     -> Este
    Este      -> Sur
    Sur       -> Oeste
    Oeste     -> Norte
    -- otherwise -> error "NO DEBERÍA PASAR"

esEste :: Dir -> Bool
esEste Este = True
esEste d    = False

esEsteMal :: Dir -> Bool
esEsteMal d    = False
esEsteMal Este = True



data Persona = P String Int  String 
              -- Nombre Edad DNI    
     deriving Show

nombre :: Persona -> String
nombre (P n _ _) = n

edad :: Persona -> Int
edad (P _ e _) = e

esMayorDeEdad' :: Persona -> Bool
esMayorDeEdad' (P _ e _) = e >= 18

esMayorDeEdad :: Persona -> Bool
esMayorDeEdad p = edad p >= 18

fidel = P "Fidel" 53 "20XXXXXX" 

--      nombre fidel
--  ->  nombre (P "Fidel" 53 "20XXXXXX")
--    --nombre (P n       e  d)
--  ->  "Fidel"
--    --n

data Gusto = Chocolate | Sambayon | DDL | Frutilla
     deriving Show
data Helado = Vasito Gusto | Cucurucho Gusto Gusto
            | Pote Gusto Gusto Gusto
     deriving Show

{- Gobstones
type Gusto is variant { 
  case Chocolate {} 
  case Sambayón  {}
  case DDL       {}
  case Frutilla  {}
}

type Helado is variant {
  case Vasito    { field gusto // Gusto
                 }
  case Cucurucho { field gusto1 // Gusto
                   field gusto2 // Gusto
                 }
  case Pote      { field gusto1 // Gusto
                   field gusto2 // Gusto
                   field gusto3 // Gusto
                 }
}
-}

esHeladoSerio :: Helado -> Bool
esHeladoSerio (Vasito g)        = esGustoSerio g
esHeladoSerio (Cucurucho g1 g2) = esGustoSerio g1 
                               && esGustoSerio g2
esHeladoSerio (Pote g1 g2 g3)   = esGustoSerio g1 
                               && esGustoSerio g2
                               && esGustoSerio g3

esGustoSerio :: Gusto -> Bool
esGustoSerio Frutilla = False
esGustoSerio _        = True

tieneChocolate :: Helado -> Bool
tieneChocolate (Vasito g)        = esChocolate g
tieneChocolate (Cucurucho g1 g2) = esChocolate g1
                                || esChocolate g2
tieneChocolate (Pote g1 g2 g3)   = esChocolate g1
                                || esChocolate g2
                                || esChocolate g3

esChocolate :: Gusto -> Bool
esChocolate Chocolate = True
esChocolate _         = False

-- ¡¡NO USA SUBTAREAS!! BUUUUUUUUU
tieneChocolateFEO :: Helado -> Bool
tieneChocolateFEO (Vasito Chocolate)      = True
tieneChocolateFEO (Cucurucho Chocolate _) = True
tieneChocolateFEO (Cucurucho _ Chocolate) = True
tieneChocolateFEO (Pote Chocolate _ _)    = True
tieneChocolateFEO (Pote _ Chocolate _)    = True
tieneChocolateFEO (Pote _ _ Chocolate)    = True
tieneChocolateFEO _                       = False

sinFrutilla :: Helado -> Helado
sinFrutilla (Vasito g)        = Vasito (cambiarFrutilla g)
sinFrutilla (Cucurucho g1 g2) = Cucurucho 
                                       (cambiarFrutilla g1)
                                       (cambiarFrutilla g2)
sinFrutilla (Pote g1 g2 g3)   = Pote   (cambiarFrutilla g1) 
                                       (cambiarFrutilla g2)                             
                                       (cambiarFrutilla g3)

cambiarFrutilla :: Gusto -> Gusto
cambiarFrutilla Frutilla = Chocolate
cambiarFrutilla g        = g


data MiPar = MP Int Bool
data MiParPoly a b = MPP a b

fstMiPar :: MiPar -> Int
fstMiPar (MP n b) = n


-- data [a]    = []  | a : [a]
data MiLista a = Nil | Cons a (MiLista a)

soyNil' :: MiLista a -> Bool
soyNil' Nil        = True
soyNil' (Cons _ _) = False

soyNil :: MiLista a -> Bool
soyNil Nil = True
soyNil _   = False

soyVacia' :: [a] -> Bool
soyVacia' []    = True
soyVacia' (_:_) = False

soyVacia :: [a] -> Bool
soyVacia [] = True
soyVacia _  = False

-- (++) :: [a] -> [a] -> [a]
-- (:)  ::  a  -> [a] -> [a]

{- Gobstones:
-- Este es el (:) de Gobstones
function cons(x,xs) {
  return([x]++xs)
}
-}
