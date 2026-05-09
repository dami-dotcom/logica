-- 20251009 Matutino
type N = Int

esDivisible :: N -> N -> Bool
esDivisible x y
  | y == 0        = error "Divisor cero"
  | (x - y) < 0   = False
  | (x - y) == 0  = True
  | otherwise     = esDivisible (x - y) y

esPar :: N -> Bool
esPar x
    | x < 0 = False
    | x == 0 = True
    | otherwise = esPar (x - 2)

sumaP :: (N -> Bool) -> N -> N -> N
sumaP p m n
    | m > n         = 0
    | p m == True   = m + sumaP p (m + 1) n 
    | otherwise     = sumaP p (m + 1) n

esPerfecto :: N -> Bool
esPerfecto x = sumaP (esDivisible x) 1 (x - 1) == x