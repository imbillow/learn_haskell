toDigitsRev :: Integer -> [Integer]
toDigitsRev x
  | x <= 0 = []
  | otherwise = x `mod` 10 : toDigitsRev (x `div` 10)

toDigits :: Integer -> [Integer]
toDigits x = reverse (toDigitsRev x)
