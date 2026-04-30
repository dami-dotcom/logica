{-#LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Tarea2 where

---------------------------------
--Nombre 1	: Damián González
--Nro. 1	: 164.553
---------------------------------

type N = Integer

--------------
--PROBLEMA 1--
--------------

suma_entre :: N -> N -> N
suma_entre m n
    | m > n = 0
	| otherwise = m + (suma_entre (m+1) n)

--1.
--suma_entre 2 5 = 14
{--
suma_entre 2 5
= 2 + (suma_entre 3 5)
= 2 + (3 + (suma_entre 4 5))
= 2 + (3 + (4 + (suma_entre 5 5)))
= 2 + (3 + (4 + (5 + (suma_entre 6 5))))
= 2 + (3 + (4 + 5))
= 2 + (3 + 9)
= 2 + 12
= 14

--}

--2.
{--
Es bien fundada porque la distancia entre los parámetros m y n disminuye 
en una unidad en cada recursión, asegurando que en algún momento m superará
en una unidad a n (dado a que estamos en el conjunto de los naturales),
activándose la guarda del caso base, pudiendo comenzar a volver en la recursión.

El tamaño del problema que decrece es la distancia entre ambos parámetros en 
cada recursión.
--}

--3.
suma_entre' :: N -> N -> N
suma_entre' m n = undefined

--4.
suma_entre_f :: (N -> N) -> N -> N -> N
suma_entre_f f m n = undefined

--5.
suma_i :: N -> N
suma_i n = undefined

--------------
--PROBLEMA 2--
--------------

--1.
es_divisor :: N -> N -> Bool
es_divisor n k = undefined

--2.
primer_divisor :: N -> N
primer_divisor n = undefined

--3.
es_primo :: N -> Bool
es_primo n = undefined

--------------
--PROBLEMA 3--
--------------

minimo_acotado :: (N -> Bool) -> N -> N -> N
minimo_acotado p m n
	| m > n = m
	| m <= n && p m = m
	| m <= n && not (p m) = minimo_acotado p (m+1) n

--1.
{--
Si ningun valor en el intervalo considerado cumple el predicado, entonces...
--}

--2.
primer_divisor' :: N -> N
primer_divisor' n = undefined

--3.
maximo_acotado :: (N -> Bool) -> N -> N -> N
maximo_acotado p m n = undefined

--4.
minimo_p :: (N -> Bool) -> N -> N
minimo_p p n
	| p n = n
	| not (p n) = minimo_p p (n+1)

{--
Esta función termina si...
--}

--------------
--PROBLEMA 4--
--------------

--1.
cantidad_p :: (N -> Bool) -> N -> N -> N
cantidad_p p m n = undefined

--2.
suma_p :: (N -> Bool) -> N -> N -> N
suma_p p m n = undefined

--3.
suma2_p :: (N -> Bool) -> N -> N -> N
suma2_p p m n = undefined

--4.
sumaf_p :: (N -> Bool) -> (N -> N) -> N -> N -> N
sumaf_p p f m n = undefined

--5.
todos_p :: (N -> Bool) -> N -> N -> Bool
todos_p p m n = undefined

--5.
existe_p :: (N -> Bool) -> N -> N -> Bool
existe_p p m n = undefined

-------
--FIN--
-------