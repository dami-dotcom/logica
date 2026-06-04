type Tira = [Int]

-- 5 de Oro
-- type Bolilla = Int
-- type Jugada = [Bolilla]

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