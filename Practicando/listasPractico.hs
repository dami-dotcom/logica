import Prelude hiding (null,length,duplicate,(++),append,reverse,lensum,concat,sum,prod,and,or,any,all,count,map,filter,elem,head,tail,last,init,(!!),firstp,lastp)

miLista, miLista2, miLista3 :: [Int]
miLista = [1,2,3]                            
miLista2 = [4,5]                           
miLista3 = [6,7,8,9]                           
miListaDeListas = [miLista, miLista2, miLista3]

miListaTrue, miListaFalse, miListaMixta, miListaTrue2 :: [Bool]
miListaTrue = [True, True, True]
miListaFalse = [False, False, False]
miListaMixta = [True, False, False]
miListaTrue2 = [True, True, True]
-- Ejercicio 1

null :: [a] -> Bool
null [] = True
null _ = False

length :: [a] -> Int
length [] = 0
length (x:xs) = 1 + length xs

duplicate :: [a] -> [a]
duplicate [] = []
duplicate (x:xs) = x : x : duplicate xs

(++) :: [a] -> [a] -> [a]
(++) [] ys = ys
(++) (x:xs) ys = x : (++) xs ys

append :: a -> [a] -> [a] -- inserta un elemento al final de la lista
append a [] = [a]
append a (x:xs) = x : append a xs

reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]

lensum :: [[a]] -> Int -- suma los largos de todas las listas.
lensum [] = 0
lensum (x:xs) = length x + lensum xs

concat :: [[a]] -> [a]  -- devuelve una lista con todos los elementos combinados
concat [] = []
concat (x:xs) = x ++ concat xs

-- Ejercicio 2 Funciones no polimorficas

sum :: [Int] -> Int -- suma todos los elementos de una lista de enteros.
sum [] = 0
sum (x:xs) = x + sum xs

prod :: [Int] -> Int -- multiplica todos los elementos de una lista de enteros.
prod [] = 1
prod (x:xs) = x * prod xs

and :: [Bool] -> Bool -- hace la conjuncion (&&) entre todos los elementos de una lista de booleanos.
and [] = True
and (x:xs) = x && and xs

or :: [Bool] -> Bool  -- hace la disyuncion (||) entre todos los elementos de una lista de booleanos.
or [] = False
or (x:xs) = x || or xs

-- Ejercicio 3. Funciones de alto orden

any :: (a -> Bool) -> [a] -> Bool --  dado un predicado p y una lista l, retorna True, si algun elemento de l cumple p
any p [] = False
any p (x:xs) 
    | p x = True
    | otherwise = any p xs

all :: (a -> Bool) -> [a] -> Bool -- dado un predicado p y una lista l, retorna True si todos los elementos de l cumplen p.
all p [] = True
all p (x:xs) 
    | not (p x) = False
    | otherwise = all p xs

count :: (a -> Bool) -> [a] -> Int -- dado un predicado p y una lista l, retorna la cantidad de elementos de l que cumplen p.
count p [] = 0
count p (x:xs) 
    | p x = 1 + count p xs
    | otherwise = count p xs

map :: (a -> b) -> [a] -> [b] -- dada una funcion f y una lista l, le aplica f a todos los elementos de l
map f [] = []
map f (x:xs) = f(x) : map f xs

filter :: (a -> Bool) -> [a] -> [a] -- dado un predicado p y una lista l, retorna una nueva lista con los elementos de l cumplen p
filter p [] = []
filter p (x:xs)
    | p x = x : filter p xs
    | otherwise = filter p xs

elem :: Eq a => a -> [a] -> Bool -- dado un elemento e y una lista l, retorna True, si e pertenece a l.
elem e [] = False
elem e (x:xs)
    | e == x = True
    | otherwise = elem e xs

{--
    Ejercicio 4. Funciones parciales (no estan definidas para todo el dominio/pueden fallar para
    alguna entrada): Para definir estas funciones debera hacer uso de la funcion 
    error :: String -> a, que corta la ejecucion de un programa.
--}

head :: [a] -> a -- retorna el primer elemento de la lista.
head [] = error "lista vacia"
head (x:xs) = x

tail :: [a] -> [a] -- retorna la lista sin el primer elemento (la cola de la lista).
tail [] = error "lista vacia"
tail (x:xs) = xs

last :: [a] -> a -- retorna el ultimo elemento de la lista.
last [] = error "lista vacia"
last [x] = x
last (_:xs) = last xs

init :: [a] -> [a] -- retorna todos los elementos de la lista menos el ultimo.
init [] = error "lista vacia"
init [x] = []
init (x:xs) = x : init xs

(!!) :: [a] -> Int -> a -- dada una lista l y un entero n, retorna el elemento de l que se encuentra en la posicion n empezando a contar desde 0.
(!!) [] _ = error "lista vacia"
(!!) (x:xs) 0 = x 
(!!) (x:xs) n
    | n < 0 = error "no se encontro el elemento"
    | otherwise = (!!) xs (n - 1)

firstp :: (a -> Bool) -> [a] -> a -- dado un predicado p y una lista l, retorna el primer elemento de l que cumpla p.
firstp p [] = error "lista vacia"
firstp p (x:xs)
    | p x = x
    | otherwise = firstp p xs

lastp :: (a -> Bool) -> [a] -> a -- dado un predicado p y una lista l, retorna el ultimo elemento de l que cumpla p.
lastp p l = firstp p (reverse l)