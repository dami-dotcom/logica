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