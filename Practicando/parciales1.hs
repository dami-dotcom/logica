-- 20251009 Matutino
type N = Int

esDivisible :: N -> N -> Bool
esDivisible x y
  | y == 0        = error "Divisor cero"
  | (x - y) < 0   = False
  | (x - y) == 0  = True
  | otherwise     = esDivisible (x - y) y

esPar :: N -> Bool
esPar x
    | x < 0 = False
    | x == 0 = True
    | otherwise = esPar (x - 2)

sumaP :: (N -> Bool) -> N -> N -> N
sumaP p m n
    | m > n         = 0
    | p m == True   = m + sumaP p (m + 1) n 
    | otherwise     = sumaP p (m + 1) n

esPerfecto :: N -> Bool
esPerfecto x = sumaP (esDivisible x) 1 (x - 1) == x

-- 20250513-ParNoc-LyMD

productoria_f_mn :: N -> N -> (N-> N) -> N
productoria_f_mn m n f 
    | m > n = 1
    | otherwise = (f m) * productoria_f_mn (m + 1) n f

factAsc :: N -> N -> N
factAsc m n 
    |  m > (n + 1) = 1
    |  otherwise = m * ( factAsc (m + 1) n)

factAsc' :: N -> N -> N
factAsc' m n = productoria_f_mn m (m + n -1) (*1)

-- 20250512-ParMat-LyMD.pdf

{-- 
1. Definir la funcion cantP :: N -> (N -> Bool) -> N , que recibe un natural n y un predicado p, 
    y computa la cantidad de naturales entre 1 y n que cumplen con p .
--}
cantP :: N -> (N -> Bool) -> N 
cantP n p
    | n == 0 = 0
    | p n == True = 1 + cantP (n - 1) p
    | otherwise = cantP (n - 1) p

{--
2. Definir sin hacer recursion la funcion exceptoUno :: N -> (N -> Bool) -> Bool, 
    que recibe un natural n y un predicado p, y retorna True si existe solamente un 
    unico natural entre 1 y n que NO cumple con p.
--}
exceptoUno :: N -> (N -> Bool) -> Bool
exceptoUno n p = (cantP n p) == 1

{--
3. Definir la funcion sumaCuad :: N -> N -> N, que dados dos naturales m y n, 
    devuelve la suma de los cuadrados de todos los numeros 
    pertenecientes al intervalo cerrado [m,n].
--}
sumaCuad :: N -> N -> N
sumaCuad m n 
    |  m > n = error "Exception: intervalo invalido"
    | m == n =  (m * m) 
    | otherwise = (m * m) + sumaCuad ( m + 1) n 

-- 20241014-Par-Noc-LyMD.pdf    
{-- 
1. Definir la funcion hay1 :: (N -> Bool) -> N -> N -> Bool, que dado un predicado p, 
    y dos naturales m y n, retorna True si y solo si, existe al menos un numero 
    en el intervalo [m..n] que cumpla el predicado p.
hay1 par 5 11 = True
hay1 (>10) 3 8 = False
--}

hay1 :: (N -> Bool) -> N -> N -> Bool
hay1 p m n 
    | m > n = False
    | p m = True
    | otherwise = hay1 p ( m + 1) n

{--
2. Definir haciendo uso de la funcion anterior, la funcion hay2 :: (N -> Bool) -> N -> Bool, 
que dado un predicado p y un natural n, retorna True si y solo si, existen al menos 2 
numeros en el intervalo [0..n] para los cuales se cumpla el predicado p.
Ejemplos:
hay2 par 6 = True
hay2 (>10) 11 = False
--}

{--
hay2 :: (N -> Bool) -> N -> Bool
hay2 p m 
    | m < 0 = False
    | hay1 p (m -1) m =  
--}

-- LogMD 2021-10 p1m.pdf    

{--
1. Definir la funcion sumaP :: (Int->Bool) ->Int -> Int, que recibe un predicado p y un natural n 
y computa la suma de los numeros entre 0 y n que cumplen con p .
Ejemplo: sumaP esPar 6 = 0 + 2 + 4 + 6 = 12
--}

sumaP2 :: (Int->Bool) ->Int -> Int
sumaP2 p n 
    | n == 0 = 0
    | p n = n + sumaP2 p ( n - 1 )
    | otherwise = sumaP2 p (n - 1)

{--
2. Definir la funcion sumaCre :: Int -> Int -> Int, que recibe una un natural n y un numero x , 
    y computa la suma de n numeros consecutivos a partir de x.
Ejemplo: sumaCre 3 5 = 5 + 6 + 7 = 18
--}

sumaCre :: Int -> Int -> Int
sumaCre n x
  | n == 0 = 0
  | n > 0  = x + sumaCre (n - 1) (x + 1)

-- 20241014-Par-Mat-LyMD.pdf
-- 14/10/2024 Matutino

{--
1. Hacer la tabla de verdad y programar en Haskell la siguiente conectiva ternaria:
ter :: Bool -> Bool -> Bool -> Bool, que dados x, y, z de tipo Bool, retorna True si al menos 2 de sus
argumentos son False o cuando z es False.
--}

ter :: Bool -> Bool -> Bool -> Bool
ter False False True    = True
ter _     _     False   = True
ter _     _     _       = False


{--
1.  Definir la funcion negar :: Bool -> N -> Bool, que dado un booleano x y un natural n, 
    aplica n veces la funcion not al parametro x.
Ejemplo:
negar True 3 = not(not(not(True))) = False
negar True 0 = True
--}

negar :: Bool -> N -> Bool
negar a x
    | x == 0 = a
    | otherwise = negar (not a) (x-1)

{--
2.  Definir la funcion noCumple :: (N -> Bool) -> N -> N -> Bool, que dado un predicado p 
    y dos naturales m y n, devuelve True si y solo si existe un natural i 
    perteneciente al intervalo cerrado [m,n] que no cumple con el predicado p.

Ejemplos:
noCumple (<4) 2 4 = True
noCumple (>0) 2 4 = False
--}

noCumple :: (N -> Bool) -> N -> N -> Bool
noCumple p m n
    | m > n = error "No tiene sentido el rango"
    | m == n = not (p m)
    | otherwise =  (noCumple p (m + 1) n) || not (p m)

-- 20241014-Par-Noc-LyMD (1).pdf
-- 14/10/2024
{--
Problema 1. Dada la siguiente tabla de verdad:
p q r ϕ(p,q,r)
T T T T
T T F T
T F T F
T F F T
F T T T
F T F F
F F T T
F F F T

1. Programar sin hacer uso de funciones auxiliares la funcion phi :: Bool -> Bool -> Bool -> Bool
representada por la tabla de verdad definida previamente.
2. Reprogramar la funcion phi :: Bool -> Bool -> Bool -> Bool en dos lıneas, haciendo uso unicamente 
de la funcion implica (>>) :: Bool -> Bool -> Bool.
--}

phi :: Bool -> Bool -> Bool -> Bool -- implica not x or y
phi True True True = True       -- (not true or True) = True // not True or True = True 
phi True True False = True      -- (not true or True) = True // not True or False = True
phi True False True = False     -- (not true or False) = False // not False or True = False  
phi True False False = True     -- (not true or False) = False // not False or False
phi False True True = True
phi False True False = False
phi False False True = True
phi False False False = True

implica :: Bool -> Bool -> Bool
implica x y = not x || y

phi' :: Bool -> Bool -> Bool -> Bool
phi' x y z = implica x (implica y z)

-- LyMD_Agosto2023Noc.pdf
{--
1. Definir la funcion fact :: Int -> Int, que dado un natural n, retorna el factorial de n, definido como
    n! = n*(n-1)*(n-2)*...*1.
Ejemplo: fact 6 = 6*5*4*3*2*1.

--}
fact :: Int -> Int
fact x
    | x == 0 = 1
    | otherwise = x * fact (x - 1)

{--
2. Definir la funcion prodEntre :: Int -> Int -> Int, que dados dos naturales m y n, 
retorna el producto de
todos los numeros que se encuentren entre m y n. Puede asumir que m ≤ n.
Ejemplo: prodEntre 3 6 = 3*4*5*6.
--}

prodEntre :: Int -> Int -> Int
prodEntre m n
    | m > n = 1
    | otherwise = m * prodEntre (m +1 ) n