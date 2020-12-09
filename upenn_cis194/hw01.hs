{- It's a comment -}

import qualified Data.List as List

-- validate credit card numbers

-- ex1
toDigitsRev :: Integer -> [Integer]
toDigitsRev x
  | x <= 0 = []
  | otherwise = x `mod` 10 : toDigitsRev (x `div` 10)

toDigits :: Integer -> [Integer]
toDigits x = reverse (toDigitsRev x)

-- ex2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther =
  reverse . r . reverse
  where
    r xs =
      case xs of
        [] -> []
        [x] -> [x]
        x : y : xs' -> x : (y * 2) : r xs'

-- ex3
sumDigits :: [Integer] -> Integer
sumDigits = sum . List.concatMap toDigits

-- ex4
validate :: Integer -> Bool
validate = (== 0) . (`mod` 10) . sumDigits . doubleEveryOther . toDigits

-- the tower of hanoi

-- ex5

type Peg = String

type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = hanoi (n -1) a c b ++ [(a, b)] ++ hanoi (n -1) c b a

-- ex6
hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 0 _ _ _ _ = []
hanoi4 n a b c d = hanoi4 x a c b d ++ hanoi y a d b ++ [(a,b)] ++ hanoi y d b a ++ hanoi4 x c b a d
  where x = n `div` 2
        y = n - x -1
