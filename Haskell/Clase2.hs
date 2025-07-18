xs1  = []
xs2  = 17:[]
xs2' = [17]

ej1  = null xs1
ej2  = null xs2
ej2' = null xs2'

esSingular :: [a] -> Bool
esSingular (_:[]) = True
esSingular _      = False

esSingular' :: [a] -> Bool
esSingular' [_] = True
esSingular' _   = False


sumarTodos :: [Int] -> Int
sumarTodos []     = 0
sumarTodos (n:ns) = n + sumarTodos ns

{- ¿Por qué usar el neutro de la suma?

   sumarTodos [17]
 =
   sumarTodos (17:[])  -- (n:ns)
 =
   17 + sumarTodos []
 =
   17 + 0              -- ¡Tiene que dar 17!
 =
   17
-}

oBien :: Bool -> Bool -> Bool
oBien True _ = True
oBien _    b = b

hayAlMenosUnCinco :: [Int] -> Bool
  -- hayAlMenosUnCinco [10,20,30] = False
  -- hayAlMenosUnCinco [10,5,30] = True
hayAlMenosUnCinco []     = False
hayAlMenosUnCinco (n:ns) = n == 5 || hayAlMenosUnCinco ns
-- hayAlMenosUnCinco (5:ns) = True
-- hayAlMenosUnCinco (_:ns) = hayAlMenosUnCinco ns

-- hayAlMenosUn :: Eq a => a -> [a] -> Bool
hayAlMenosUn :: Int -> [Int] -> Bool
  -- Similar, pero toma el número por parámetro
hayAlMenosUn k []     = False
hayAlMenosUn k (n:ns) = n == k || hayAlMenosUn k ns

soloLosMayoresQue :: Int -> [Int] -> [Int]
  -- soloLosMayoresQue 25 [10,30,20,40] = [30,40]
soloLosMayoresQue k []     = []
soloLosMayoresQue k (n:ns) = if n>k 
                              then n : soloLosMayoresQue k ns
                              else soloLosMayoresQue k ns

iniciales :: [Dir] -> [Char]
  -- iniciales [Norte,Sur] = ['N','S']
iniciales []     = []
iniciales (d:ds) = inicial d : iniciales ds

data Dir = Norte | Este | Sur | Oeste
     deriving Show

inicial :: Dir -> Char
inicial Norte = 'N'
inicial Este  = 'E' 
inicial Sur   = 'S' 
inicial Oeste = 'O'

{-    iniciales [Norte, Sur, Norte, Este]
      /                                 \
   Norte                     iniciales [Sur, Norte, Este]
      \                                  |
    inicial                         ['S','N','E']
        \             (:)               /
               'N' : ['S','N','E']
-}

miZip :: [a] -> [b] -> [(a,b)]
miZip []     _      = [] 
miZip _      []     = [] 
miZip (x:xs) (y:ys) = (x,y) : miZip xs ys

{-
zipFeo :: [a] -> [b] -> [(a,b)]
zipFeo []     []     = []
zipFeo (x:xs) []     = ??
zipFeo []     (y:ys) = ??
zipFeo (x:xs) (y:ys) = ... x ... y ... zipFeo xs ys ...
-}

miZip3 :: [a] -> [b] -> [c] -> [(a,b,c)]
miZip3 []     _      _      = []
miZip3 _      []     _      = []
miZip3 _      _      []     = []
miZip3 (x:xs) (y:ys) (z:zs) = (x,y,z) : miZip3 xs ys zs

{-
zip3Feo :: [a] -> [b] -> [c] -> [(a,b,c)]
zip3Feo []     []     []     = []
zip3Feo []     (y:ys) []     = ??
zip3Feo []     []     (z:zs) = ??
zip3Feo []     (y:ys) (z:zs) = ??
zip3Feo (x:xs) []     []     = ??
zip3Feo (x:xs) []     (z:zs) = ??
zip3Feo (x:xs) (y:ys) []     = ??
zip3Feo (x:xs) (y:ys) (z:zs) = ... x ... y ... z ... zip3Feo xs ys zs ... 
-}

promedioFeo :: [Int] -> Int
promedioFeo []     = error "No puedo dividir por 0"
promedioFeo (n:[]) = n
promedioFeo (n:ns) = 
    div (n + (promedioFeo ns)*(length ns)) 
        (length (n:ns))

replicar :: Int -> a -> [a]    
-- PRECOND: el número es mayor o igual que cero
     -- replicar 2 'K' = ['K','K']
replicar 0 _ = []
replicar n x = x : replicar (n-1) x

cuentaRegresivaDesde :: Int -> [Int]
	 -- cuentaRegresivaDesde 5 = [5,4,3,2,1,0]
cuentaRegresivaDesde 0 = [0]
cuentaRegresivaDesde n = n : cuentaRegresivaDesde (n-1)

losPrimerosN :: Int -> [a] -> [a] 
     -- losPrimerosN 7 [10..50] = [10,11,12,13,14,15,16]
losPrimerosN 0 _      = []
losPrimerosN _ []     = []
losPrimerosN n (x:xs) = x : losPrimerosN (n-1) xs
