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