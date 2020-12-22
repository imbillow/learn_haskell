{-# LANGUAGE FlexibleInstances #-}

module JoinList where

import Buffer
import Editor
import Scrabble
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

takeJ ::
  (Sized b, Monoid b) =>
  Int ->
  JoinList b a ->
  JoinList b a
takeJ x zs
  | x <= 0 = Empty
  | x >= sz = zs
  where
    sz = getSize' $ tag zs
takeJ x (Append _ xs ys) = takeJ x xs +++ takeJ (x - sx) ys
  where
    sx = getSize' $ tag xs

-- 3

scoreLine :: String -> JoinList Score String
scoreLine str = Single (scoreString str) str

-- 4

instance Monoid b => Semigroup (JoinList b a) where
  (<>) = (+++)

instance Monoid b => Monoid (JoinList b a) where
  mempty = Empty

instance Buffer (JoinList (Score, Size) String) where
  toString Empty = ""
  toString (Single _ s) = s
  toString (Append _ xs ys) = toString xs ++ toString ys

  fromString = mconcat . map (\l -> Single (scoreString l, Size 1) l) . lines

  line = indexJ

  replaceLine n l b = postfix +++ fromString l +++ suffix
    where
      postfix = takeJ n b
      suffix = dropJ (n + 1) b

  numLines = getSize . size . tag
  value = getScore . score_ . tag

main =
  runEditor editor buffer
  where
    buffer :: JoinList (Score, Size) String
    buffer =
      fromString $
        unlines
          [ "This buffer is for notes you don't want to save, and for",
            "evaluation of steam valve coefficients.",
            "To load a different file, type the character L followed",
            "by the name of the file."
          ]
