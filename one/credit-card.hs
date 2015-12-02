{- EXERCISE 1

   We need to first find the digits of a number. Define the functions

   toDigits :: Integer -> [Integer]
   toDigitsRev :: Integer -> [Integer]

   toDigits should convert positive Integers to a list of digits. (For 0 or
   negative inputs, toDigits should return the empty list.) toDigitsRev
   should do the same, but with the digits reversed.

   Example: toDigits 1234 == [1,2,3,4]
   Example: toDigitsRev 1234 == [4,3,2,1]
   Example: toDigits 0 == []
   Example: toDigits (-17) == [] -}

toDigits :: Integer -> [Integer]
toDigits x
  | x < 1     = []
  | otherwise = toDigits (x `div` 10) ++ [x `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev x = reverse (toDigits x)

{- EXERCISE 2

   Once we have the digits in the proper order, we need to
   double every other one. Define a function
   doubleEveryOther :: [Integer] -> [Integer]
   Remember that doubleEveryOther should double every other number
   beginning from the right, that is, the second-to-last, fourth-to-last,
   ... numbers are doubled.

   Example: doubleEveryOther [8,7,6,5] == [16,7,12,5]
   Example: doubleEveryOther [1,2,3] == [1,4,3] -}

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther (x:xs) = x : (2 * head xs) : doubleEveryOther (tail xs)

{- EXERCISE 3

   The output of doubleEveryOther has a mix of one-digit
   and two-digit numbers. Define the function
   sumDigits :: [Integer] -> Integer
   to calculate the sum of all digits.

   Example: sumDigits [16,7,12,5] = 1 + 6 + 7 + 1 + 2 + 5 = 22 -}

sumDigits :: [Integer] -> Integer
sumDigits xs = sum . concat $ map toDigits xs

{- EXERCISE 4

   Define the function
   validate :: Integer -> Bool
   that indicates whether an Integer could be a valid credit card number.
   This will use all functions defined in the previous exercises.

   Example: validate 4012888888881881 = True
   Example: validate 4012888888881882 = False -}

validate :: Integer -> Bool
validate x = (sumDigits $ reverse $ doubleEveryOther $ toDigitsRev x) `mod` 10 == 0
