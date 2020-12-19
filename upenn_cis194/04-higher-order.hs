import Data.List

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x : xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
  | even n = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)

-- 1

fun1' :: [Integer] -> Integer
fun1' = product . map (subtract 2) . filter even

fun2' :: Integer -> Integer
fun2' =
  sum
    . filter even
    . takeWhile (/= 1)
    . iterate (\n -> if even n then n `div` 2 else 3 * n + 1)

-- 2

data Tree a
  = Leaf
  | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr (insert 0) Leaf
  where
    insert d e tree =
      case tree of
        Leaf -> Node d Leaf e Leaf
        Node d' left _ right -> undefined

-- 3

xor :: [Bool] -> Bool
xor = foldr (/=) False

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x : xs) = foldr (\a b -> f a : b) [f x] xs

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base = foldr (flip f) base . reverse

-- 4

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = 2 : map (\x -> x * 2 + 1) ([1 .. n] \\ remove)
  where
    remove =
      sort $
        nub
          [ t
            | i <- [1 .. n + 1],
              j <- [i .. n],
              let t = i + j + 2 * i * j,
              t <= n
          ]

cardProd :: [a] -> [b] -> [(a, b)]
cardProd xs ys = [(x, y) | x <- xs, y <- ys]
