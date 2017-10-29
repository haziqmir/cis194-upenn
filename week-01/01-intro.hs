-- http://www.cis.upenn.edu/~cis194/spring13/hw/01-intro.pdf

toDigits :: Integer -> [Integer]
toDigits 0 = []
toDigits n
  | n < 0     = []
  | otherwise = mod n 10 : toDigits (div n 10)

toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverse (toDigits n)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:[]) = [x]
doubleEveryOther (x:(y:zs)) = x : ((y * 2) : doubleEveryOther(zs))


sumDigits :: [Integer] -> Integer
-- sumDigits [16,7,12,5] = 1 + 6 + 7 + 1 + 2 + 5 = 22
sumDigits [] = 0
sumDigits (x:xs)
  | x > 9     = (mod x 10) + (div x 10) + sumDigits xs
  | otherwise = x + sumDigits xs


validate :: Integer -> Bool
-- Int -> [Int] -> doubleEveryOther -> sumDigits -> checkSum
validate x
  | (sumDigits (doubleEveryOther (toDigits x))) `mod` 10 == 0 = True
  | otherwise       = False


type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move] -- # of discs and names of pegs, output a sequence of moves
-- Example: hanoi 2 "a" "b" "c" == [("a","c"), ("a","b"), ("c","b")]



--Exercise 6 (Optional) What if there are four pegs instead of three?




