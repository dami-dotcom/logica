type N = Int

esPar :: N -> Bool
esPar n 
    | n == 0 = True 
    | n == 1 = False
    | otherwise = esPar (n - 2)

{--
Prueba: Parcial 1 - Nocturno, 09/10/2025 (archivo: 20251009ParNocLyMD.pdf, página 2)
Problema 3 (completo, las tres partes son encadenadas)

1. Definir la función modulo :: N -> N -> N, que dados dos naturales x e y, devuelve el resto de la división x entre y. 
La definición debe realizarse utilizando recursión con la resta (-) y operaciones comparativas.
Ejemplos:

   modulo 12 3 = 0
   modulo 14 3 = 2
   modulo 7 0 = error "El divisor no puede ser cero"
--}

modulo :: N -> N -> N
modulo x y
    | y == 0 = error "El divisor no puede ser cero"
    | x < y = x
    | otherwise = modulo (x - y) y

{--
2. Definir la función productoDivisibles :: N -> N -> N -> N, tal que productoDivisibles m n k devuelve el producto de 
    todos los números del intervalo cerrado [m, n] que son divisibles por k. 
    La definición debe realizarse mediante recursión, reutilizando modulo para verificar la divisibilidad. 
    Si m > n, devuelve 1. Si k = 0, debe reportar error.
Ejemplos:

   productoDivisibles 1 10 3 = 162   (3 * 6 * 9)
   productoDivisibles 5 20 5 = 18750 (5 * 10 * 15 * 20)
   productoDivisibles 10 1 7 = 1
--}

productoDivisibles :: N -> N -> N -> N
productoDivisibles m n k
    | k == 0 = error "k no puede ser cero"
    | m > n = 1
    | modulo m k == 0 = m * productoDivisibles (m + 1) n k
    | otherwise = productoDivisibles (m + 1) n k

{--
3. Definir la función productoPares :: N -> N -> N en una sola línea, reutilizando únicamente la función del ítem (2). 
    Debe devolver el producto de todos los números pares en el intervalo [m, n]. Si m > n, debe invertir los parámetros.
Ejemplos:

   productoPares 1 10 = 3840  (2 * 4 * 6 * 8 * 10)
   productoPares 10 1 = 3840
--}

productoPares :: N -> N -> N 
productoPares m n = if m > n then productoDivisibles n m 2 else productoDivisibles m n 2

-- Prueba: Parcial 1 - Matutino, 12/05/2025 (archivo: 20250512ParMatLyMD.pdf, página 2)
{--
 1. Definir la función cantP :: N -> (N -> Bool) -> N, que recibe un natural n y un predicado p, y computa la cantidad de naturales 
    entre 1 y n que cumplen con p.
Ejemplos:

   cantP 8 esPar = 4
   cantP 0 esPar = 0
   cantP 8 esPositivo = 8
   cantP 0 esPositivo = 0
--}
cantP :: N -> (N -> Bool) -> N
cantP n p
    | n == 0 = 0
    | p n = 1 + cantP (n - 1) p 
    | otherwise = cantP (n - 1) p 

{--
2.  Definir sin hacer recursión la función exceptoUno :: N -> (N -> Bool) -> Bool, que recibe un natural n y un predicado p, 
    y retorna True si existe solamente un único natural entre 1 y n que NO cumple con p.
Ejemplos:

   exceptoUno 8 (< 8)  = True
   exceptoUno 8 (< 10) = False
   exceptoUno 8 (> 5)  = False
--}

exceptoUno :: N -> (N -> Bool) -> Bool
exceptoUno n p 
    | n == 1 && p n = True
    | otherwise = n - (cantP n p) == 1  -- Entendi mal la letra, es 1 que NOOOOO cumple con p. exceptoUno n p = n - cantP n p == 1, oneliner
{--
3.  Definir la función sumaCuad :: N -> N -> N, que dados dos naturales m y n, 
    devuelve la suma de los cuadrados de todos los números pertenecientes al intervalo cerrado [m, n].
Ejemplos:

   sumaCuad 2 5 = 2² + 3² + 4² + 5² = 4 + 9 + 16 + 25 = 54
   sumaCuad 2 2 = 2² = 4
   sumaCuad 5 2 = *** Exception: intervalo inválido.
--}
sumaCuad :: N -> N -> N
sumaCuad m n 
    | m > n = error "intervalo inválido"
    | m == n = m * m
    | otherwise = (m * m) + sumaCuad (m + 1) n

-- Prueba: Parcial 1 - Matutino, 09/10/2025 (archivo: 20251009ParMatLyMD.pdf, página 2)

{--
1. Definir la función esDivisible :: N -> N -> Bool, que dados dos naturales n y x, devuelve True si n es divisible entre x.
Ejemplos:

   esDivisible 5 5 = True
   esDivisible 3 5 = False
   esDivisible 17 5 = False
   esDivisible 15 5 = True
   esDivisible 7 0 = error "x no puede ser cero"
--}

esDivisible :: N -> N -> Bool
esDivisible n x 
    | x == 0 = error "x no puede ser cero"
    | (n - x) == 0 = True
    | n > x = esDivisible (n - x) x 
    | otherwise = False

{-- 
2.  Definir la función sumaP :: (N -> Bool) -> N -> N -> N, que dado un predicado p y dos naturales m y n, 
    computa la suma de los números x tal que m ≤ x ≤ n y cumplen con el predicado p.
Ejemplos:

   sumaP esPar 4 9 = 4 + 6 + 8 = 18
   sumaP (> 6) 4 9 = 7 + 8 + 9 = 24
   sumaP esPar 9 4 = 0
--}

sumaP :: (N -> Bool) -> N -> N -> N
sumaP p m n 
    | m > n = 0
    | p m = m + sumaP p (m + 1) n 
    | otherwise = sumaP p (m + 1) n 
{--
3. Definir en una línea (sin usar recursión de forma directa) la función esPerfecto :: N -> Bool, que dado un natural n, 
    se dice que n es un número perfecto si y sólo si la suma de sus divisores menores que él dan como resultado él mismo. 
    Por ejemplo, 28 es un número perfecto ya que 28 = 1 + 2 + 4 + 7 + 14.
--}
esPerfecto :: N -> Bool
esPerfecto n = sumaP (esDivisible n) 1 (n - 1) == n

-- Prueba: Parcial 1 - Nocturno, 14/10/2024 (archivo: 20241014ParNocLyMD.pdf, página 2)
{--Problema 3 (las dos partes son encadenadas)

1.  Definir la función hay1 :: (N -> Bool) -> N -> N -> Bool, que dado un predicado p, y dos naturales m y n, 
    retorna True si y solo si existe al menos un número en el intervalo [m..n] que cumpla el predicado p.
Ejemplos:

   hay1 par 5 11 = True
   hay1 (>10) 3 8 = False
--}

hay1 :: (N -> Bool) -> N -> N -> Bool
hay1 p m n
    | m > n = False
    | p m = True 
    | otherwise = hay1 p (m + 1) n
{-- 

2.  Definir haciendo uso de la función anterior, la función hay2 :: (N -> Bool) -> N -> Bool, que dado un predicado p y un natural n, 
    retorna True si y solo si existen al menos 2 números en el intervalo [0..n] para los cuales se cumpla el predicado p.
Ejemplos:
   hay2 par 6 = True
   hay2 (>10) 11 = False
--}

hay2 :: (N -> Bool) -> N -> Bool
hay2 p n 
    | n == 0 = False
    | p n = hay1 p 0 (n - 1)
    | otherwise = hay2 p (n - 1)

-- Prueba: Examen, 28/07/2025 (archivo: 20250728ExmLyMD.pdf, página 2)
{--

Problema 3 (las dos partes son encadenadas)

1. Definir la función esDivisible :: N -> N -> Bool, que dados dos naturales n y x, devuelve True si n es divisible entre x.
Ejemplos:

   esDivisible 5 5  = True
   esDivisible 3 5  = False
   esDivisible 17 5 = False
   esDivisible 15 5 = True
   esDivisible 7 0  = error "x no puede ser cero"
--}
esDivisible' :: N -> N -> Bool
esDivisible' n x 
    | x == 0 = error "x no puede ser cero"
    | n - x == 0 = True
    | n < x = False 
    | otherwise = esDivisible' (n - x) x 

{--
2.  Definir la función sumarDivisibles :: N -> N -> N -> N, que dados tres naturales m, n y k, 
    devuelve la suma de todos los números en el intervalo [m, n] que son divisibles por k.
Ejemplos:

   sumarDivisibles 1 10 3 = 3 + 6 + 9 = 18
   sumarDivisibles 10 1 3 = 0
--}

sumarDivisibles :: N -> N -> N -> N
sumarDivisibles m n k
    | m > n = 0
    | esDivisible' m k = m + sumarDivisibles (m + 1) n k
    | otherwise = sumarDivisibles (m + 1) n k

-- Prueba: Parcial 1 - Nocturno, 12/10/2023 (archivo: LyMD_Agosto2023Noc.pdf, página 1)

{--Problema 1 — parte 1 (solo la parte de programar)

Dada la siguiente conectiva ternaria fun (información de contexto, ya está dada):

fun :: Bool -> Bool -> Bool -> Bool
fun True q r  = not q && not r
fun False q r = True

Definir sin hacer uso de funciones auxiliares, la conectiva binaria (//) :: Bool -> Bool -> Bool, 
que dados dos booleanos p y q, retorna True cuando p y q son iguales, o cuando p es False.

--}
(//) :: Bool -> Bool -> Bool
(//) False _ = True
(//) True True = True
(//) _ _ = False

-- Prueba: Parcial 1 - Nocturno, 12/10/2023 (archivo: LyMD_Agosto2023Noc.pdf, página 1)
{--

Problema 3 (las dos partes son encadenadas)

1.  Definir la función fact :: Int -> Int, que dado un natural n, retorna el factorial de n, 
    definido como n! = n*(n-1)*(n-2)*...*1.
Ejemplo: fact 6 = 6*5*4*3*2*1.

--}

fact' :: Int -> Int
fact' n 
    | n == 0 = 1
    | otherwise = n * fact' (n - 1)

{--
2.  Definir la función prodEntre :: Int -> Int -> Int, que dados dos naturales m y n, 
    retorna el producto de todos los números que se encuentren entre m y n. Puede asumir que m ≤ n.
Ejemplo: prodEntre 3 6 = 3*4*5*6.
--}

prodEntre :: Int -> Int -> Int
prodEntre m n 
    | m > n = 1
    | otherwise = m * prodEntre (m + 1) n

-- Prueba: Parcial 1 - Matutino, 12/10/2022 (archivo: LyMD_202210p1mat.pdf, página 2)
{--
Problema 3 (dos partes independientes pero relacionadas)

Definir la función prod :: Int -> Int, que dado un natural n, multiplica todos los números entre 1 y n.
Ejemplo: prod 4 = 1*2*3*4 = 24
--}
prod :: Int -> Int
prod n
    | n == 1 = 1
    | otherwise = n * prod (n - 1)

{--
Definir la función sumaPares :: Int -> Int, que dado un natural n, suma los números pares entre 0 y n.
Ejemplo: sumaPares 4 = 0+2+4 = 6
--}
sumaPares :: Int -> Int
sumaPares n 
    | n == 0 = 0
    | esPar n = n + sumaPares (n - 1)
    | otherwise = sumaPares (n - 1)