module Golf where

skips :: [a] -> [[a]]
skips xs = [skipsN i | i <- [1 .. length xs]]
  where
    skipsN n = [xs !! (i -1) | i <- [1 .. length xs], i `mod` n == 0]
