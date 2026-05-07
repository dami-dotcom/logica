{--
2. Definir la función sumaP :: (N -> Bool) -> N -> N -> N, que dado un predicado p y dos naturales m y n,
computa la suma de los números x, tal que m ≤ x ≤ n y cumplen con el predicado p.

Ejemplos:

sumaP esPar 4 9 = 4 + 6 + 8 = 18
sumaP (> 6) 4 9 = 7 + 8 + 9 = 24
sumaP esPar 9 4 = 0
--}

type N = Integer

sumaP :: (N -> Bool) -> N -> N -> N
sumaP p m n
  | m > n = 0
  | p m = m + sumaP p (m+1) n
  | otherwise = sumaP p (m+1) n

{-

1. Definir la función productoria f mn :: N -> N -> (N -> N) -> N, que dados dos naturales m y n, y una

función f, calcula la productoria entre m y n aplicando f a cada factor.

Ejemplos:

productoria mn 1 4 (+1) = 2 * 3 * 4 * 5
productoria mn 2 4 (*3) = 6 * 9 * 12

-}

productoriaFMN :: N -> N -> (N -> N) -> N
productoriaFMN m n f
  | m > n = 1
  | otherwise = f m * productoriaFMN (m+1) n f

{-
2. Definir la función factAsc :: N -> N -> N, que dados dos naturales m y n,
calcula el factorial ascendente de m con n lugares hacia adelante.

Ejemplos:

factAsc 4 6 = 4 * 5 * 6 * 7 * 8 * 9
factAsc 7 3 = 7 * 8 * 9
factAsc 1 5 = 1 * 2 * 3 * 4 * 5
factAsc 6 1 = 6
factAsc 6 0 = 1
-}

factAsc :: N -> N -> N
factAsc m n
  | n == 0 = 1
  | otherwise = m * factAsc (m+1) (n-1)
{-
3. Redefinir en una línea la función factAsc haciendo uso de la función productoria f mn
-}

factAsc2 :: N -> N -> N
factAsc2 m n = productoriaFMN m (m + n-1) (*1)