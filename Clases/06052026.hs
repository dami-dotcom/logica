fromto :: Integer -> Integer -> [Integer]
fromto m n
    | m > n = []
    | otherwise = m : fromto ( m + 1) n

-- Definicion de filter, viene con las de Haskell

filter' :: (a -> Bool) -> [a] -> [a]
filter' p [] = []
filter' p (x:xs)
    | p x = x : filter' p xs
    | otherwise = filter' p xs

divisores :: Integer -> [Integer]
divisores n = filter (\x -> n `mod` x == 0) [1..n]

primo :: Integer -> Bool
primo n = (n > 1) && (divisores n == [1..n])

primosMenores :: Integer -> [Integer]
primosMenores n = filter (primo) (fromto 1 n)

-- 17 ejercicios de listas

-- Ejercicio 1.1) Nos dice que usemos 50 lineas de pattern matching Parcial 20250513

tabla :: Bool -> Bool -> Bool -> Bool
tabla False False False = False
tabla True False True = False
tabla True True False = False
tabla a b c = True

-- Ejercicio 1.2) Repgrogramar lo anterior haciendo conectivas booleanas que hay que programar
or' :: Bool -> Bool
or' True _ = True
or' _ True = True
or' _ _ = False

and' :: Bool -> Bool
and' True True = True
and' _ _ = False

iff' :: Bool -> Bool
iff' True True = True
iff' False False = True
iff' _ _ = False

not' :: Bool -> Bool
not' True = False
not' False = True

-- Ejercicio 1.2)
tabla' :: Bool -> Bool -> Bool -> Bool 
tabla' a b c = (a `and'` (b `iff'` c)) `or'` ((not a) `and'` (b `or'` c))
tabla'' :: Bool -> Bool -> Bool -> Bool 
tabla'' a b c = (((((a `and'` b) `and'` c) `or'` ((a `and'` not' b) `and'` not' c)) `or'` ((not a `and'` b) `and'` c)) `or'` ((not a `and'` b) `and'` not c)) `or'` ((not a `and'` not b) `and'` c)