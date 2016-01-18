module Golf where

{- EXERCISE 1 -}

-- The first list in the output should be the same as the input list.
-- The second list in the output should contain every second element
-- from the input list, and the nth list in the output should contain
-- every nth element from the input list.
--
-- For example:
--   skips "ABCD"
--   => ["ABCD", "BD", "C", "D"]
--   skips "hello!"
--   => ["hello!", "el!", "l!", "l", "o", "!"]
--   skips [1]
--   => [[1]]
--   skips [True,False]
--   => [[True,False], [False]]
--   skips []
--   => []
-- Note that the output should be the same length as the input.
skips :: [a] -> [[a]]
skips []     = []
skips list   = [everyNth i list | i <- [1..length list]]

-- Helper function to get every `n`th element of a list.
everyNth :: Int -> [a] -> [a]
-- Probably going to get dinged re: style for using !! (partial function)
everyNth n list = [list !! i | i <- [n - 1, n - 1 + n..length list - 1]]

{- EXERCISE 2 -}

-- Finds all local maxima in the input
-- list and returns them in order.
--
-- For example:
--   localMaxima [2,9,5,6,1]
--   => [9,6]
--   localMaxima [2,3,4,1,5]
--   => [4]
--   localMaxima [1,2,3,4,5]
--   => []
localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:rest)
  | x < y && y > z = y: localMaxima (y:z:rest)
  | otherwise      =    localMaxima (y:z:rest)
localMaxima _ = []

{- EXERCISE 3 -}

-- Takes as input a list of Integers
-- between 0 and 9 (inclusive) and
-- outputs a vertical histogram showing
-- how many of each number were in the
-- input list.
--
-- For example:
--   histogram [1,1,1,5]
--   =>
--
--    *
--    *
--    *   *
--   ==========
--   0123456789
histogram :: [Integer] -> String
histogram xs =
  let c   = count xs
      max = maximum c
  in unlines (map (line c) [max + 1, max..1]) ++ "==========\n0123456789\n"

-- Helper to generate a column of *s.
line :: [Int] -> Int -> String
line xs x = [if i >= x then '*' else ' ' | i <- xs]

-- Helper to count occurrences of numbers [0..9] in the input.
count :: [Integer] -> [Int]
count xs = map (\x -> length $ filter (== x) xs) [0..9]
