{-# LANGUAGE LambdaCase #-}

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
x = undefined

fibs3 :: Stream Integer
fibs3 = undefined

-- 7

fib4 :: Integer -> Integer
fib4 = undefined
