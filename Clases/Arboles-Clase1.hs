data BinTree a = Empty | Node a (BinTree a) (BinTree a)

arbolEjemplo :: BinTree Integer
arbolEjemplo = Node 16 (Node 10 (Node 6 Empty Empty) (Node 5 Empty Empty)) (Node 20 Empty Empty)

size :: BinTree a -> Integer
size Empty = 0
size (Node x izq der) = 1 + size izq + size der

sumTree :: BinTree Integer -> Integer
sumTree Empty = 0 
sumTree (Node x izq der) = x + sumTree izq + sumTree der

allTree :: BinTree a -> (a -> Bool) -> Bool 
allTree Empty p = True 
allTree (Node x izq der) p = p x && allTree izq p && allTree der p

anyTree :: BinTree a -> (a -> Bool) -> Bool 
anyTree Empty p = False 
anyTree (Node x izq der) p = p x || anyTree izq p || anyTree der p

-- Función auxiliar 
maxInteger :: Integer -> Integer -> Integer 
maxInteger x y 
    | x >= y = x 
    | otherwise = y 

height :: BinTree a -> Integer 
height Empty = 0 
height (Node x izq der) = 1 + maxInteger (height izq) (height der)

memberOrd :: BinTree Integer -> Integer -> Bool 
memberOrd Empty n = False
memberOrd (Node x izq der) n 
    | x == n = True
    | n < x = memberOrd izq n 
    | otherwise = memberOrd der n

-- Ejemplo de arbol ordenado
arbolEjemplo2 :: BinTree Integer
arbolEjemplo2 = Node 16 (Node 10 (Node 6 Empty Empty) (Node 12 Empty Empty)) (Node 20 Empty Empty)

preorder :: BinTree a -> [a]
preorder Empty = []
preorder (Node x izq der) = [x] ++ preorder izq ++ preorder der

inorder :: BinTree a -> [a]
inorder Empty = []
inorder (Node x izq der) = inorder izq ++ [x] ++ inorder der

postorder :: BinTree a -> [a]
postorder Empty = []
postorder (Node x izq der) = postorder izq ++ postorder der ++ [x]