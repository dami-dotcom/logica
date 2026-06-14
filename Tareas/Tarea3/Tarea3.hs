module Tarea3 where

---------------------------------
--Nombre 1	: Damián González
--Nro. 1	: 164553
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

Hipotesis: Sea x ∈ a tal que (∀p)  prefijo p xs ++ sufijo p xs = xs.   

Tesis: (∀p) prefijo p (x:xs) ++ sufijo p (x:xs) = (x:xs)


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

Propiedad: (∀ l1, l2 :: [a]) incluido (interseccion l1 l2) l2 = True

Inducción en l1.  P l1 ≡ (∀ l2) incluido (interseccion l1 l2) l2 = True

Caso base:

incluido (interseccion [] l2) l2
=
  incluido [] l2     (por 1ra ec. de interseccion: interseccion [] ys = [])
=
  True               (por 1ra ec. de incluido: incluido [] ys = True)


Paso inductivo (l1 = x:xs):
Hipotesis: Sea x ∈ a (∀ l2) incluido (interseccion xs l2) l2 = True   
Tesis: (∀ l2) incluido (interseccion (x:xs) l2) l2 = True

Tomamos l2 en casos según la def. de interseccion.

Caso (l2 = []):

incluido (interseccion (x:xs) []) []
=
incluido [] []     (por 2da ec. de interseccion: interseccion xs [] = [])
=
True               (por 1ra ec. de incluido)

CASO B (l2 no vacía): entra la 3ra ec. de interseccion (tres guardas).

Bsub1 (vale elem x xs):

incluido (interseccion (x:xs) l2) l2
=
incluido (interseccion xs l2) l2   (por interseccion, guarda elem x xs)
=
True                               (por hipótesis de inducción)

Bsub2 (no vale elem x xs, vale elem x l2):

incluido (interseccion (x:xs) l2) l2
=
incluido (x : interseccion xs l2) l2            (por interseccion, guarda elem x ys)
=
elem x l2 && incluido (interseccion xs l2) l2   (por 3ra ec. de incluido; l2 ≠ [])
=
True && incluido (interseccion xs l2) l2        (porque en este subcaso vale elem x l2)
=
incluido (interseccion xs l2) l2                (por def. de &&: True && b = b)
=
True                                            (por hipótesis de inducción)

Bsub3 (no vale elem x xs, no vale elem x l2 -> otherwise):

incluido (interseccion (x:xs) l2) l2
=
incluido (interseccion xs l2) l2   (por interseccion, guarda otherwise)
=
True                               (por hipotesis de indcción)

En los cuatro casos se llega a True, por lo que queda demostrado

--}

--------------------
----Ejercicio 3:----
--------------------

data Tree = L Int | U Int Tree | B Tree Int Tree
 deriving Show
--1)
t :: Tree
t = B (L 4) 8 (U 16 (B (L 9) 14 (L 18)))

--2)
listarElems :: Tree -> [Int]
listarElems (L n)       = [n]
listarElems (U n t)     = [n] ++ listarElems t
listarElems (B ti n td) = [n] ++ listarElems ti ++ listarElems td

--3)
esBinario :: Tree -> Bool
esBinario (L n)       = True
esBinario (U n t)     = False
esBinario (B ti n td) = esBinario ti && esBinario td

--4)
espejo :: Tree -> Tree
espejo (L n)       = L n
espejo (U n t)     = U n (espejo t)
espejo (B ti n td) = B (espejo td) n (espejo ti)

--5) 
convertirEnBinario :: Tree -> Tree
convertirEnBinario (L n)       = L n
convertirEnBinario (U n t)     = B (convertirEnBinario t) n (espejo (convertirEnBinario t))
convertirEnBinario (B ti n td) = B (convertirEnBinario ti) n (convertirEnBinario td)

--6)
{--
(∀t::Tree) esBinario (convertirEnBinario t) = True


Propiedad: (∀t::Tree) esBinario (convertirEnBinario t) = True
P t ≡ esBinario (convertirEnBinario t) = True

Caso base (t = L n):

esBinario (convertirEnBinario (L n))
=
  esBinario (L n)     (por convertirEnBinario, caso L)
=
  True                (por esBinario, caso L)


Paso inductivo unario (t = U n s):
Hipotesis: esBinario (convertirEnBinario s) = True

esBinario (convertirEnBinario (U n s))
=
esBinario (B (convertirEnBinario s) n (espejo (convertirEnBinario s)))
                    (por convertirEnBinario, caso U)
=
esBinario (convertirEnBinario s) && esBinario (espejo (convertirEnBinario s))
                    (por esBinario, caso B)
=
True && esBinario (espejo (convertirEnBinario s))   (por hipótesis de inducción)
=
esBinario (espejo (convertirEnBinario s))           (por def. de &&: True && b = b)
=
True                (por L1, pues esBinario (convertirEnBinario s) = True por HI)


Paso inductivo binario (t = B ti n td):
Hipotesis: 
    esBinario (convertirEnBinario ti) = True
  y esBinario (convertirEnBinario td) = True.   

esBinario (convertirEnBinario (B ti n td))
=
esBinario (B (convertirEnBinario ti) n (convertirEnBinario td))
                    (por convertirEnBinario, caso B)
=
esBinario (convertirEnBinario ti) && esBinario (convertirEnBinario td)
                    (por esBinario, caso B)
=
True && esBinario (convertirEnBinario td)   (por HI sobre ti)
=
esBinario (convertirEnBinario td)           (por def. de &&: True && b = b)
=
True                                        (por HI sobre td)

En los tres casos se llega a True, por lo que queda demostrado


--}
