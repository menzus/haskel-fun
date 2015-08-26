module DataStructure.Tree where 

data Tree a = Empty  |
              Leaf a | 
              Branch (Tree a) (Tree a)

size :: Tree a -> Int
size (Empty)      = 0
size (Leaf _)     = 1
size (Branch l r) = size l + size r

add :: Tree a -> Tree a -> Tree a
add Empty t = t
add t Empty = t
add t1 t2   = Branch t1 t2 