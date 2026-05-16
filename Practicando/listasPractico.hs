import Prelude hiding (null,length,duplicate,(++),append,reverse,lensum,concat,sum,prod,and,or,any,all,count,map,filter,elem,head,tail,last,init,(!!),firstp,lastp)

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

-- Ejercicio 2
