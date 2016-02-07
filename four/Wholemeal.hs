module Wholemeal where

{- EXERCISE 1 -}

-- Reimplement each of the following functions in a more idiomatic
-- Haskell style. Use wholemeal programming practices, breaking each
-- function into a pipeline of incremental transformations to an entire
-- data structure. Name your functions fun1' and fun2' respectively.
-- 1. fun1 :: [Integer] -> Integer
--    fun1 [] = 1
--    fun1 (x:xs)
--      | even x    = (x - 2) * fun1 xs
--      | otherwise = fun1 xs
-- 2. fun2 :: Integer -> Integer
--    fun2 1 = 0
--    fun2 n | even n    = n + fun2 (n `div` 2)
--           | otherwise = fun2 (3 * n + 1)
-- Hint: For this problem you may wish to use the functions `iterate`
-- and `takeWhile`. Look them up in the Prelude documentation to see
-- what they do.
fun1' :: [Integer] -> Integer
fun1' = product . map (\x -> x - 2) . filter even

-- The Collatz conjecture!
fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (> 1) . iterate (\n -> if even n then n `div` 2 else 3 * n + 1)

{- EXERCISE 2 -}

-- Given the following data structure for a binary tree:
data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)
-- For this exercise, write a function
--   foldTree :: [a] -> Tree a
-- which generates a balanced binary tree
-- from a list of values using `foldr`.
foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf

insert :: a -> Tree a -> Tree a
insert x Leaf = Node 1 Leaf x Leaf
insert x (Node n t1 v t2)
  | h1 < h2   = Node n (insert x t1) v t2
  | h1 > h2   = Node n t1 v (insert x t2)
  | otherwise = Node (h + 1) t1 v (insert x t2)
  where h1 = height t1
        h2 = height t2
        h  = height (insert x t2)

height :: Tree a -> Integer
height Leaf             = 0
height (Node n t1 v t2) = n

{- EXERCISE 3 -}

-- Implement a function:
--   xor :: [Bool] -> Bool
-- which returns True if and only if there are an odd number of True
-- values contained in the input list. It does not matter how many
-- False values the input list contains.
xor :: [Bool] -> Bool
xor xs = odd . length $ filter id xs

-- Implement map as a fold. That is, complete the definition
--   map' :: (a -> b) -> [a] -> [b]
-- in such a way that map' behaves identically to the standard map function.
map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\y ys -> (f y):ys) [] xs

{- EXERCISE 4 -}

-- Implement http://en.wikipedia.org/wiki/Sieve_of_Sundaram
-- Given an integer n, your function should
-- generate all the odd prime numbers up to 2n + 2:
--   sieveSundaram :: Integer -> [Integer]
-- To give you some help, below is a function to compute the Cartesian
-- product of two lists:
cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

-- The general algorithm: for integers 1 to n,
-- remove all numbers of the form i + j + 2ij where:
--   * i, j ∈ ℕ, i <= i <= j
--   * i + j + 2ij <= n
sieveSundaram :: Integer -> [Integer]
sieveSundaram n =
  let sieve = map (\(i, j) -> i + j + 2 * i * j)
              . filter (\(i, j) -> i + j + 2 * i * j <= n)
              $ cartProd [1..n] [1..n]
  in map ((+ 1) . (* 2)) $ [1..n]
