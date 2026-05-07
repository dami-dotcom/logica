{-

1. Defina sin usar guardas la función menor :: N -> N -> Bool, que recibe 2 naturales m,
n y devuelve True si m es menor estricto que n. Puede utilizar solamente la
operación de la resta (-).
-}

type N = Integer

menor:: N -> N -> Bool
menor m 0 = False
menor 0 n = True
menor m n = menor (m-1) (n-1)


{-
2. Defina sin usar guardas la función min2 :: N -> N -> N, que recibe 2 naturales m, n y
retorna el menor de los dos. Puede utilizar solamente la operación de la resta (-).
-}



min2 :: N -> N -> N
min2 m n = min2Aux m m n n

min2Aux :: N -> N -> N -> N -> N
min2Aux prevM 0 prevN n2 = prevM
min2Aux prevM m2 prevN 0 = prevN
min2Aux prevM m2 prevN n2 = min2Aux prevM (m2-1) prevN (n2-1)

-- Restamos m2 y n2

min22 :: N -> N -> N
min22 0 n = 0
min22 m 0 = 0
min22 m n = min22 (m-1) (n-1) -(-1)

{-

3. Definir la función posmax :: (N -> Bool) -> N -> N tal que posmax p n devuelve el
mayor número positivo menor o igual a n que satisfaga p. Si no existe ningún
positivo menor o igual a n que satisfaga p, debe devolver 0.

Ejemplos:
Sea la función par :: N -> Bool tal que par x = True si y sólo si x es par
posmax par 3 = 2
posmax impar 3 = 3
-}

posmax :: (N -> Bool) -> N -> N
posmax p n
  | n == 0 = 0
  | p n = n
  | otherwise = posmax p (n-1)

par :: N -> Bool
par n = n `mod` 2 == 0

{-
4. Definir la función prodp :: (N -> Bool)-> N -> N que recibe un predicado p y un
natural n y calcula el producto de los números menores o iguales a n para los cuales
se cumple p.
-}

prodp :: (N -> Bool) -> N -> N
prodp p n
  | n == 0 && p n = 0
  | n == 0 && not(p n) = 1
  | p n = n * prodp p (n-1)
  | not(p n) = prodp p (n-1)

{-
5. Definir la función fib :: N -> N , considerando que la secuencia de Fibonacci
comienza con los números 0 y 1, mientras que los números siguientes se definen
recursivamente como la suma de sus dos predecesores inmediatos (ej. 0+1 = 1,
1+1=2, 2+1=3, 3+2= 5, 5+3=8, ...)
-}
fib :: N -> N
fib 0 = 0
fib 1 = 1
fib n = fib(n-1) + fib(n-2)
{-
6. Definir la función cifras :: N -> N tal que cifras n devuelve la cantidad de dígitos que
contiene la representación decimal de n. Asumir que el número n es un número
entero positivo (n >= 0). Ejemplos: cifras 3 = 1 y cifras 3012 = 4
-}
cifras :: N -> N
cifras n
| n >= 10 = 1 + cifras(n `div` 10)
| otherwise = 1
{-
7. Definir la función factorialCre :: N -> N -> N tal que factorialCre a n devuelve la
multiplicación de n números consecutivos a partir de a.

Ejemplos:

factorialCre 4 3 = 4 * 5 * 6 = 120
factorialCre 8 5 = 8 * 9 * 10 * 11 * 12 = 95040
-}

factorialCre :: N -> N -> N
factorialCre m 0 = 1
factorialCre 0 n = 0
factorialCre m n = m * factorialCre (m+1) (n-1)