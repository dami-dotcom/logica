data BinTree a = Empty | Node a (BinTree a) (BinTree a)

arbolEjemplo :: BinTree Integer
arbolEjemplo = Node 16 (Node 10 (Node 6 Empty Empty) (Node 5 Empty Empty)) (Node 20 Empty Empty)

arbolOrdenado :: BinTree Integer
arbolOrdenado = Node 16 (Node 10 (Node 5 Empty Empty) (Node 6 Empty Empty)) (Node 20 Empty Empty)

arbolOrdenado2 :: BinTree Integer
arbolOrdenado2 = Node 16 (Node 10 (Node 6 Empty Empty) (Node 12 Empty Empty)) (Node 20 Empty Empty)

size :: BinTree a -> Integer
size Empty = 0
size (Node x izq der) = 1 + size (izq) + size (der)

sumTree :: BinTree Integer -> Integer
sumTree Empty = 0
sumTree (Node x izq der) = x + sumTree izq + sumTree der

allTree :: BinTree a -> (a -> Bool) -> Bool 
allTree Empty p = True
allTree (Node x izq der) p = p x && allTree izq p && allTree der p

anyTree :: BinTree a -> (a -> Bool) -> Bool
anyTree Empty p = False 
anyTree (Node x izq der) p = p x || anyTree izq p || anyTree der p

-- Funcion auxiliar
maxInteger :: Integer -> Integer -> Integer
maxInteger x y = if x > y then x else y

height :: BinTree a -> Integer
height Empty = 0
height (Node x izq der) = 1 + maxInteger (height izq) (height der)

memberOrd :: BinTree Integer -> Integer -> Bool 
memberOrd Empty n = False
memberOrd (Node x izq der) n 
    | x == n    = True
    | n < x     = memberOrd izq n
    | otherwise = memberOrd der n

preOrder :: BinTree a -> [a]
preOrder Empty = []
preOrder (Node x izq der) = [x] ++ preOrder (izq) ++ preOrder (der)

inOrder :: BinTree a -> [a]
inOrder Empty = []
inOrder (Node x izq der) = inOrder izq ++ [x] ++ inOrder der

postOrder :: BinTree a -> [a]
postOrder Empty = []
postOrder (Node x izq der) = postOrder izq ++ postOrder der ++ [x]