module Tarea3 where

---------------------------------
--Nombre 1	: 
--Nro. 1	: 
---------------------------------
--Nombre 2 	: 
--Nro. 2 	: 
---------------------------------

--------------------
----Ejercicio 1:----
--------------------

--1)
prefijo :: (a -> Bool) -> [a] -> [a]
prefijo p [] = []
prefijo p (x:xs)
    | p x       = x : prefijo p xs
    | otherwise = []

--2)
sufijo :: (a -> Bool) -> [a] -> [a]
sufijo p [] = []
sufijo p (x:xs)
    | p x       = sufijo p xs
    | otherwise = x:xs

--3)
{--
(∀ xs::[a]) (∀p::(a− > Bool)) ((prefijo p xs) ++ (sufijo p xs) = xs)

Caso base:  

Tesis: (∀p) prefijo p [] ++ sufijo p [] = []

prefijo p [] ++ sufijo p []
=
  [] ++ sufijo p []           (por caso base de prefijo)
=
  sufijo p []                 (por caso base de ++)
=
  []                          (por caso base de sufijo)


Paso inductivo:

Hipotesis: prefijo p xs ++ sufijo p xs = xs.   

Tesis: prefijo p (x:xs) ++ sufijo p (x:xs) = (x:xs)


1. Caso p x:

prefijo p (x:xs) ++ sufijo p (x:xs)
=
(x : prefijo p xs) ++ sufijo p (x:xs)   (por prefijo, vale p x)
=
(x : prefijo p xs) ++ sufijo p xs       (por sufijo, vale p x)
=
x : (prefijo p xs ++ sufijo p xs)       (por caso recursivo de ++)
=
x : xs                                  (por hipótesis de inducción)

2. Caso ¬(p x): (yo lo hice con otherwise en el codigo)

prefijo p (x:xs) ++ sufijo p (x:xs)
=
[] ++ sufijo p (x:xs)       (por prefijo, no vale p x)
=
sufijo p (x:xs)             (por caso base de ++)
=
x : xs                      (por sufijo, no vale p x)

En ambos casos llegue a x : xs , por lo que queda demostrado.

--}

-}

--------------------
----Ejercicio 2:----
--------------------

--1)
incluido :: Eq a => [a] -> [a] -> Bool
incluido [] ys = True
incluido xs [] = False
incluido (x:xs) ys = elem x ys && incluido xs ys

--2)
interseccion :: Eq a => [a] -> [a] -> [a]
interseccion [] ys = []
interseccion xs [] = []
interseccion (x:xs) ys
    | elem x xs = interseccion xs ys
    | elem x ys = x : interseccion xs ys
    | otherwise = interseccion xs ys

--3)
{--
(∀ l1,l2::[a]) incluido (interseccion l1 l2) l2 = True

-}

--------------------
----Ejercicio 3:----
--------------------

data Tree = L Int | U Int Tree | B Tree Int Tree
 deriving Show
--1)
t :: Tree
t = undefined

--2)
listarElems :: Tree -> [Int]
listarElems t = undefined 

--3)
esBinario :: Tree -> Bool
esBinario t = undefined

--4)
espejo :: Tree -> Tree
espejo t = undefined

--5) 
convertirEnBinario :: Tree -> Tree
convertirEnBinario t = undefined

--6)
{--
(∀t::Tree) esBinario (convertirEnBinario t) = True

--}
