type Tira = [Int]

-- 5 de Oro (pero importa el orden, trucheli) (Importa el orden, repeticion no permitida)
type Bolilla = Int
type Jugada = [Bolilla]

elem' :: Eq a => a -> [a] -> Bool
elem' e [] = False
elem' e (x:xs)
    | e == x = True
    | e /= x = elem' e xs

fromto :: Int -> Int -> [Int]
fromto m n
    |   m > n      = []
    |   otherwise  = m : fromto (m + 1) n

agregarBolillas :: [Bolilla] -> Jugada -> [Jugada]
agregarBolillas [] j = []
agregarBolillas (b:bs) j
    | elem' b j         = agregarBolillas bs j
    | not (elem b j)    = (b:j):(agregarBolillas bs j)

agregarBolillasEntre :: Int -> Int -> Jugada -> [Jugada]
agregarBolillasEntre m n j 
    |   m > n       = []
    |   elem' m j   = agregarBolillasEntre (m + 1) n j 
    |   otherwise   = (m : j) : (agregarBolillasEntre (m + 1) n j) 

-- Mas simple, como lo hizo el profe en clase, el anterior es mio
agregarBolillasEntre' :: Int -> Int -> Jugada -> [Jugada]
agregarBolillasEntre' m n j = agregarBolillas (fromto m n) j -- agregarBolillas ya plantea la repeticion

-- 8
agregarBolilla :: [Jugada] -> [Jugada]
agregarBolilla []     = []
agregarBolilla (j:js)   = agregarBolillasEntre' 1 48 j ++ agregarBolilla js

-- llegue hasta el minuto 48 de la clase https://vimeo.com/showcase/12141663?video=1198301066