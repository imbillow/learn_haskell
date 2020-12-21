{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

-- 1

import Data.List
import Data.Maybe

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n -1) + fib (n -2)

fibs1 :: [Integer]
fibs1 = map fib [0 ..]

-- 2

data Stream a = Cons a (Stream a)
  deriving (Show)

fibs2 :: [Integer]
fibs2 = m
  where
    m = 0 : 1 : [m !! (i -1) + m !! (i -2) | i <- [2 ..]]

fibs2' :: [Integer]
fibs2' = 0 : 1 : zipWith (+) fibs2' (tail fibs2')

-- 3

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

-- 4

streamRepeat :: a -> Stream a
streamRepeat x = Cons x $ streamRepeat x

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) $ streamMap f xs

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x $ streamFromSeed f (f x)

-- 5

nats :: Stream Integer
nats = streamFromSeed succ 0

ruler :: Stream Integer
ruler = interLeaveStreams zeros positives
  where
    positives =
      streamMap (\x -> f' $ x * 2) $
        streamFromSeed succ 1
    zeros = streamRepeat 0
    interLeaveStreams (Cons x xs) (Cons y ys) = Cons x $ Cons y $ interLeaveStreams xs ys
    m = iterate (* 2) 1
    f x y = gcd x y == y
    f' :: Integer -> Integer
    f' x =
      fromJust
        ( ( find (f x) $
              reverse $
                takeWhile (<= x) m
          )
            >>= \y ->
              elemIndex y m
                >>= return . fromIntegral
        )

-- 6

x :: Stream Integer
x = Cons 0 $ Cons 1 $ streamRepeat 0

mul (Cons x xs) s = Cons (x * s) $ mul xs s

instance Num (Stream Integer) where
  fromInteger n = Cons n $ streamRepeat 0
  negate (Cons x xs) = Cons (- x) $ negate xs
  (+) (Cons x xs) (Cons y ys) = Cons (x + y) $ xs + ys
  (*) (Cons x xs') ys@(Cons y ys') = Cons (x * y) $ mul ys' x + xs' * ys

instance Fractional (Stream Integer) where
  (/) (Cons x xs') (Cons y ys') = q
    where
      q = Cons (x `div` y) $ mul (xs' - q * ys') (round $ 1 / fromIntegral y)

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x * x)

-- 7

data Mat2 a = Mat2 a a a a

instance Num (Mat2 Integer) where
  (Mat2 a0 a1 a2 a3) * (Mat2 b0 b1 b2 b3) =
    Mat2
      (a0 * b0 + a1 * b2)
      (a0 * b1 + a1 * b3)
      (a2 * b0 + a3 * b2)
      (a2 * b1 + a3 * b3)

fib4 :: Integer -> Integer
fib4 n = y
  where
    Mat2 _ y _ _ = Mat2 1 1 1 0 ^ n
