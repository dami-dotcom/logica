Ejercicio 1. Funciones polimórficas:


(a) null :: [a] -> Bool, indica si una lista dada está vacía o no.


(b) length :: [a] -> Int, retorna el largo de la lista.


(c) duplicate :: [a] -> [a], duplica cada elemento de la lista de forma que para una lista

[a,b,c] retorne [a,a,b,b,c,c].


(d) (++) :: [a] -> [a] -> [a], concatena dos listas, la segunda a continuación de la primera

de forma que [a,b]++[c,d,e] = [a,b,c,d,e].


(e) append :: a -> [a] -> [a], inserta un elemento al final de la lista.


(f) reverse :: [a] -> [a], invierte el orden de los elementos de la lista.


(g) lensum :: [[a]] -> Int, suma los largos de todas las listas.


(h) concat :: [[a]] -> [a], devuelve una lista con todos los elementos de todas las listas en

su respectivo orden de forma que concat [[a,b],[a,c],[b,d]] = [a,b,a,c,b,d].


-}



null :: [a] -> Bool

null [] = True

null _ = False



length :: [a] -> Int

length [] = 0

length (x:xs) = 1 + length(xs)



duplicate :: [a] -> [a]

duplicate [] = []

duplicate (x:xs) = x:(x:(duplicate (xs)))



(++) :: [a] -> [a] -> [a] -- [a,b] ++ [c,d,e] = [a,b,c,d,e]

(++) [] ys = ys

(++) (x:xs) ys = x:((++) xs (ys))



-- b:[c,d,e] --> [b,c,d,e]

-- a:[b,c,d,e] --> [a,b,c,d,e]



append :: a -> [a] -> [a] -- append c [a,b] --> [a,b,c]

append elem [] = [elem] -- append a [] --> [a]

append elem (x:xs) = x:(append elem xs)



reverse :: [a] -> [a] -- reverse [1,2,3] --> [3,2,1]

reverse [] = []

reverse (x:xs) = reverse(xs) ++ [x]



lensum :: [[a]] -> Int -- lensum [ [1,2,3], [4,5], [6,7] ] --> 5

lensum [] = 0

lensum (x:xs) = length x + lensum(xs) -- x es [1,2,3] y xs [[4,5], [6,7]]



concat :: [[a]] -> [a] -- [[a,b],[a,c],[b,d]] = [a,b,a,c,b,d]

concat [] = []

concat (x:xs) = x ++ concat(xs)





{-

Ejercicio 2. Funciones no polimórficas:

(a) sum :: [Int] -> Int, suma todos los elementos de una lista de enteros.

(b) prod :: [Int] -> Int, multiplica todos los elementos de una lista de enteros.

(c) and :: [Bool] -> Bool, hace la conjunción (&&) entre todos los elementos de una lista

de booleanos.

(d) or :: [Bool] -> Bool , hace la disyunción (||) entre todos los elementos de una lista de

booleanos.

-}



sum:: [Int] -> Int

sum [] = 0

sum (x:xs) = x + sum(xs)



prod:: [Int] -> Int

prod [] = 1

prod (x:xs) = x * prod(xs)



and:: [Bool] -> Bool

and [] = True

and (x:xs) = x && and (xs)



or:: [Bool] -> Bool

or [] = False

or (x:xs) = x || or (xs)



{-

Ejercicio 3. Funciones de alto orden:

(a) any :: (a -> Bool) -> [a] -> Bool, dado un predicado p y una lista l, retorna True,

si algún elemento de l cumple p.

(b) all :: (a -> Bool) -> [a] -> Bool, dado un predicado p y una lista l, retorna True,

si todos los elementos de l cumplen p.

(c) count :: (a -> Bool) -> [a] -> Int, dado un predicado p y una lista l, retorna la

cantidad de elementos de l que cumplen p.

(d) map :: (a -> b) -> [a] -> [b], dada una función f y una lista l, le aplica f a todos

los elementos de l.

1

(e) filter :: (a -> Bool) -> [a] -> [a], dado un predicado p y una lista l, retorna una

nueva lista con los elementos de l cumplen p.

(f) elem :: Eq a => a -> [a] -> Bool, dado un elemento e y una lista l, retorna True, si

e pertenece a l

-}



any:: (a -> Bool) -> [a] -> Bool

any p [] = False

any p (x:xs) = p x || any p (xs)



any2:: (a -> Bool) -> [a] -> Bool

any2 p [] = False

any2 p (x:xs)

  | p x = True

  | otherwise = any2 p (xs)

 

all :: (a -> Bool) -> [a] -> Bool

all p [] = True

all p (x:xs) = p x && all p xs



count :: (a -> Bool) -> [a] -> Int

count p [] = 0

count p (x:xs)

  | p x = 1 + count p xs

  | otherwise = count p xs



map :: (a -> b) -> [a] -> [b]

map f [] = []

map f (x:xs) = f x : map f xs



filter :: (a -> Bool) -> [a] -> [a]

filter p [] = []

filter p (x:xs)

  | p x = x:(filter p xs)

  | otherwise = filter p xs



elem :: Eq a => a -> [a] -> Bool

elem a [] = False

elem a (x:xs)

  | a == x = True

  | otherwise = elem a xs



{-



Ejercicio 4. Funciones parciales (no están definidas para todo el dominio/pueden fallar para

alguna entrada): Para definir estas funciones deberá hacer uso de la función error :: String

-> a, que corta la ejecución de un programa.



-- Mirar clase teorico



(a) head :: [a] -> a, retorna el primer elemento de la lista.

(b) tail :: [a] -> [a], retorna la lista sin el primer elemento (la cola de la lista).

(c) last :: [a] -> a, retorna el último elemento de la lista.

(d) init :: [a] -> [a], retorna todos los elementos de la lista menos el último.



-- Mirar clase teorico





(e) (!!) :: [a] -> Int -> a, dada una lista l y un entero n, retorna el elemento de l que

se encuentra en la posición n empezando a contar desde 0.

(f) firstp :: (a -> Bool) -> [a] -> a, dado un predicado p y una lista l, retorna el

primer elemento de l que cumpla p.

(g) lastp :: (a -> Bool) -> [a] -> a, dado un predicado p y una lista l, retorna el último

elemento de l que cumpla p.



-}



(!!) :: [a] -> Int -> a

(!!) [] number = error "La lista tiene que tener elementos"

(!!) (x:xs) number

  | number == 0 = x -- !! [6,9,4] 0 --> 6

  | otherwise = (!!) xs (number - 1)

