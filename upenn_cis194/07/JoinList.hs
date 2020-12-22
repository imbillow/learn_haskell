module JoinList where

import Sized

data JoinList m a
  = Empty
  | Single m a
  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

-- 1

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
Empty +++ Empty = Empty
Empty +++ x = x
x +++ Empty = x
x +++ y = Append (tag x <> tag y) x y

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

-- 2

getSize' :: Sized a => a -> Int
getSize' = getSize . size

indexJ ::
  (Sized b, Monoid b) =>
  Int ->
  JoinList b a ->
  Maybe a
indexJ _ Empty = Nothing
indexJ i _ | i < 0 = Nothing
indexJ 0 (Single _ x) = Just x
indexJ i (Append s xs ys)
  | i >= sz = Nothing
  | i < sx = indexJ i xs
  | otherwise = indexJ (i - sx) ys
  where
    sz = getSize' s
    sx = getSize' $ tag xs

dropJ ::
  (Sized b, Monoid b) =>
  Int ->
  JoinList b a ->
  JoinList b a
dropJ x zs
  | x <= 0 = zs
  | x >= sz = Empty
  where
    sz = getSize' $ tag zs
dropJ x (Append _ xs ys) = dropJ x xs +++ dropJ (x - sx) ys
  where
    sx = getSize' $ tag xs

takeJ :: (Sized b, Monoid b) =>
  Int -> JoinList b a -> JoinList b a
takeJ x zs
  | x <= 0 = Empty
  | x >= sz = zs
  where
    sz = getSize' $ tag zs
takeJ x (Append _ xs ys) = takeJ x xs +++ takeJ (x-sx) ys
  where
    sx = getSize' $ tag xs

-- 3
