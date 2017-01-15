-- Exercise 1: Wholemeal Programming.

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
      | even x = (x - 2) * fun1 xs
      | otherwise = fun1 xs


 -- This function appears to filter out odd numbers and then subtract two
 --from the remaining list of even numbers, and then finally take the product
 --across the resulting list.
fun1' :: [Integer] -> Integer
fun1' = product . map (subtract 2) . filter even

fun1'' :: [Integer] -> Integer
-- This one doesn't work on empty lists! 
fun1'' = foldl (\acc x -> acc * (x-2)) 1

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
   | even n = n + fun2 (n `div` 2)
   | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (>1) . iterate (\x -> if (even x) then (div x 2) else (3*x + 1))

-- Exercise 2: Folding with Trees
data Tree a = Leaf
              | Node Integer (Tree a) a (Tree a)
              deriving (Show, Eq)

insertNode :: a -> Tree a -> Tree a
insertNode x Leaf = Node 0 Leaf x Leaf
insertNode x (Node h Leaf y Leaf) = Node (h+1) (insertNode x Leaf) y Leaf
insertNode x (Node h left y Leaf) = Node h left y (insertNode x Leaf)
insertNode x (Node h Leaf y right) = Node h (insertNode x Leaf) y right
insertNode x (Node h left@(Node leftHeight _ _ _) y right@(Node rightHeight _ _ _))
            | leftHeight > rightHeight = Node h left y (insertNode x right)
            | leftHeight < rightHeight = Node h (insertNode x left) y right
            | otherwise = Node newHeight newTree y right
               where newTree = insertNode x left
                     newHeight = (max (height newTree) rightHeight) + 1

height :: Tree a -> Integer
height (Node h _ _ _) = h

foldTree :: [a] -> Tree a
foldTree = foldr (insertNode) Leaf

isBalanced :: Tree a -> Bool
isBalanced Leaf = True
isBalanced (Node _ Leaf y Leaf) = True
isBalanced (Node h Leaf y right@(Node rightHeight _ _ _)) = (rightHeight == 0)
isBalanced (Node h left@(Node leftHeight _ _ _) y Leaf) = (leftHeight == 0)
isBalanced (Node h left@(Node leftHeight _ _ _) y right@(Node rightHeight _ _ _)) = (abs (leftHeight - rightHeight) <= 1) && (isBalanced left) && (isBalanced right)

-- Exercise 3: More Folds!
xor :: [Bool] -> Bool
xor = foldl (\acc x -> if x == False then not acc else acc) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> (f x): acc) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (flip f) base (reverse xs)

-- Exercise 4: Finding Primes
cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

removeList' n = [i + j + 2*i*j | (i, j) <- cartesian, i <= j, i + j + 2*i*j <= n]
                   where cartesian = cartProd [1..n] [1..n]

removeList :: (Ord a, Num a, Enum a) => a -> [a]
removeList n = filter (\n -> not (elem n [i + j + 2*i*j | (i, j) <- (cartProd [1..n] [1..n]), i <= j, i + j + 2*i*j <= n])) [1..n]

sieveSundaram :: Integer -> [Integer]
sieveSundaram = map (+1) . map (*2) . removeList



