modulo :: Int -> Int -> Int
modulo _ 0 = error "El divisor no puede ser cero"
modulo x y
  | x < y    = x
  | otherwise = modulo (x - y) y

productoDivisibles :: Int -> Int -> Int -> Int
productoDivisibles m n k
  | k == 0    = error "k no puede ser cero"
  | m > n     = 1
  | modulo n k == 0 = n * productoDivisibles m (n - 1) k
  | otherwise       = productoDivisibles m (n - 1) k

productoPares :: Int -> Int -> Int
productoDivisibles