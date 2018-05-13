import Data.List

-- exercise 1
-- original
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

-- idiomatic
fun1' :: [Integer] -> Integer
fun1' a = foldl' (\y x -> (x-2) * y) 1 $ filter even a

-- original 2
fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)

-- idiomatic
fun2' :: Integer -> Integer 
fun2' n = sum . filter even $ takeWhile (>1) $ iterate f n

f :: Integer -> Integer 
f n | even n = n `div` 2 | otherwise = 3*n + 1


-- exercise 2
data Tree a = Leaf | Node Integer (Tree a) a (Tree a)
              deriving(Show, Eq)

fullLevels :: Tree a -> Integer 
fullLevels Leaf = -1
fullLevels (Node _ l _ r) = 1 + min (fullLevels l) (fullLevels r)

treeInsert :: a -> Tree a -> Tree a

treeInsert a Leaf = Node 0 Leaf a Leaf 
treeInsert a (Node i Leaf v r) = Node (i+1) (treeInsert a Leaf) v r 
treeInsert a (Node i l v Leaf) = Node i l v (treeInsert a Leaf)

treeInsert a (Node i l v r) 
  | n < m = Node (m+1) (treeInsert a l) v r
  | n == m = Node (n+2) (treeInsert a l) v r
  | otherwise = Node i l v (treeInsert a r)
  where (n,m) = (fullLevels l, fullLevels r)

foldTree :: [a] -> Tree a
foldTree = foldr treeInsert Leaf 

-- exercise 3
-- part 1
xor :: [Bool] -> Bool 
xor = foldr (/=) False

-- part 2
map' :: (a->b) -> [a] -> [b]
map' f = foldr (\x y -> f x : y) [] 

-- part3
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (\b g x -> g ( f x b)) id xs base

-- exercise 4
cartProd :: Ord a => [a] -> [a] -> [(a,a)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys, x <= y]

fn :: (Integer,Integer) -> Integer
fn (i,j) = i + j + 2*i*j

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = 2: map ((+1) . (*2)) (filterNums 1 n (removeNums n))

filterNums :: Integer -> Integer -> [Integer] -> [Integer]
filterNums s n [] = [s..n]
filterNums s n (b:bs) = filter (< b) [s..(b-1)] ++ filterNums (b+1) n bs

removeNums :: Integer -> [Integer]
removeNums n = sort $ filter (< n) $ map fn $ cartProd [1..n] [1..n]


