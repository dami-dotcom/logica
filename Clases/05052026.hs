--buscar :: [Integer] -> Integer -> Bool
--buscar2 :: [Bool] -> Bool -> Bool
--buscar3 :: [String] -> String -> Bool

pertenece :: [a] -> a -> Bool
pertenece [] _ = False
pertenece (x:xs) a
    | a == x = True
    | otherwise = pertenece xs a