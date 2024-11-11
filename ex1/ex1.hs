toDigits :: Integer -> [Integer]
toDigits n
 | n > 0 = toDigits (n `div` 10) ++ [n `mod` 10]
 | otherwise = []
