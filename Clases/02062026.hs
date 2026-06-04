type Tira = [Int]

fromto :: Int -> Int -> [Int]
fromto m n
    |   m > n      = []
    |   otherwise  = m : fromto (m + 1) n

agregar01 :: Tira -> [Tira]
agregar01 t = [0 : t, 1 : t]

agregarBit :: [Tira] -> [Tira]
agregarBit [] = []
agregarBit (t:ts) = agregar01 t ++ agregarBit ts

tirasNBits :: Int -> [Tira]
tirasNBits n
    | n <=0     = [[]]
    | otherwise = agregarBit (tirasNBits (n - 1))

tirasNBits' :: Int -> [Tira]
tirasNBits' 0   = [[]]
tirasNBits' n   = agregarBit (tirasNBits' (n - 1))

agregarDigitos :: [Int] -> Tira -> [Tira]
agregarDigitos [] t = []
agregarDigitos (d:ds) t = (d:t):(agregarDigitos ds t)

agregar06 :: Tira -> [Tira]
agregar06 t = agregarDigitos (fromto 0 6) t

agregarDigito :: [Tira] -> [Tira]
agregarDigito [] = []
agregarDigito (t:ts) = agregar06 t ++ agregarDigito ts

tirasNDigitos :: Int -> [Tira]
tirasNDigitos 0 = [[]]
tirasNDigitos n = agregarDigito (tirasNDigitos (n - 1))

-- Esto genera permutaciones con repeticion
{--
    PR(n,r) = n^r
    n = cantidad de objetos
    r = largo
--}