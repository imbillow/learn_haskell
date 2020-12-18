module Golf where

import Data.Char
import Data.List

skips :: [a] -> [[a]]
skips xs = [skipsN i | i <- [1 .. length xs]]
  where
    skipsN n = [xs !! (i -1) | i <- [1 .. length xs], i `mod` n == 0]

localMaxima :: [Integer] -> [Integer]
localMaxima = concatMap (\(a : b : c : _) -> if b > a && b > c then [b] else []) . filter ((== 3) . length) . windows 3
  where
    windows n xs = map (take n) $ tails xs

histogram :: [Integer] -> String
histogram = unlines . toGrid . groups
  where
    groups = map (\x -> (head x, length x)) . group . sort
    toGrid xs =
      let m = maximum $ map snd xs
       in reverse $
            transpose
              [ intToDigit (fromIntegral y) : '=' :
                case k of
                  Nothing -> replicate m ' '
                  Just k -> [if x <= k then '*' else ' ' | x <- [1 .. m]]
                | y <- [0 .. 9],
                  let k = lookup y xs
              ]
