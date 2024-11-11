-- Ex 1
-- Converts positive integers to a list of their digits
-- toDigits 1234 == [1,2,3,4]
-- toDigits 0 == []
-- toDigits (-17) == []
toDigits :: Integer -> [Integer]
toDigits n
  | n > 0   = reverse $ toDigitsRev n
  | otherwise = []

-- Converts positive integers to a reversed list of their digits
-- toDigitsRev 1234 == [4,3,2,1]
toDigitsRev :: Integer -> [Integer]
toDigitsRev n
 | n > 0 = n `mod` 10 : toDigitsRev (n `div` 10)
 | otherwise = []

-- Ex2
-- double every other number beginning from the right
-- doubleEveryOther [4,3,2,1] == [8,3,4,1]
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . zipWith (*) oneTwo . reverse where
  oneTwo = 1 : 2 : oneTwo

-- Ex3
-- sum digits to sum of all numbers
-- sumDigits[16,7,12,5] = 22
sumDigits :: [Integer] -> Integer
sumDigits = sum . map (sum . toDigits)

-- Ex4
-- validate the credit card
validate :: Integer -> Bool
validate x = (sumDigits . doubleEveryOther . toDigits) x `mod` 10 == 0

-- Part 2: Tower of Hanoi

type Peg = String
type Move = (Peg, Peg)
hanoi:: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = hanoi (n - 1) a c b ++ -- Move n - 1 disc from a to ac using b as temp
  [(a,b)] ++ -- Move the top disc from a to b
    hanoi (n - 1) c b a -- Move n - 1 disc from c to b using a as temp
