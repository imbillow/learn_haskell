{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import qualified Data.List as List
import Log

-- ex01
parseMessage :: String -> LogMessage
parseMessage x =
  case words x of
    "I" : tim : msg -> LogMessage Info (read tim) $ unwords msg
    "W" : tim : msg -> LogMessage Warning (read tim) $ unwords msg
    "E" : sev : tim : msg -> LogMessage (Error $ read sev) (read tim) $ unwords msg
    _ -> Unknown x

parse :: String -> [LogMessage]
parse =
  map parseMessage . lines

-- ex02
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert m@(LogMessage _ t _) tree =
  case tree of
    Leaf -> Node Leaf m Leaf
    Node left m'@(LogMessage _ t' _) right ->
      if t <= t'
        then Node (insert m left) m' right
        else Node left m' (insert m right)
    -- this should never happen
    _ -> undefined

-- ex03
build :: [LogMessage] -> MessageTree
build = List.foldl (flip insert) Leaf

-- ex04
inOrder :: MessageTree -> [LogMessage]
inOrder tree =
  case tree of
    Leaf -> []
    Node left m right ->
      inOrder left ++ [m] ++ inOrder right

-- ex05
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong =
  map (\(LogMessage _ _ msg) -> msg)
    . filter
      ( \case
          LogMessage (Error e) _ _ -> e >= 50
          _ -> False
      )
    . inOrder
    . build

-- ex06
{- maybe pickles? or flange? -}
