module Fibonacci where

{- EXERCISE 1 -}

-- Translate the definition of Fibonacci numbers directly
-- into a recursive function definition of type
--   fib :: Integer -> Integer
-- so that `fib n` computes the nth Fibonacci number.
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

-- Now use `fib` to define the infinite list of all
-- Fibonacci numbers,
--   fibs1 :: [Integer]
-- (Hint: You can write the list of all positive integers as [0..].)
fibs1 :: [Integer]
fibs1 = map fib [0..]

{- EXERCISE 2 -}

-- Your task for this exercise is to come up with more
-- efficient implementation. Specifically, define the
-- infinite list
--   fibs2 :: [Integer]
-- so that it has the same elements as fibs1, but
-- computing the first n elements of fibs2 requires
-- only O(n) addition operations.
--
-- TBH, this required some Stack Overflow-ing after a couple of days:
-- http://stackoverflow.com/questions/1105765/generating-fibonacci-numbers-in-haskell
fibs2 :: [Integer]
fibs2 = 1:1:zipWith (+) fibs2 (tail fibs2)

-- *Fibonacci> take 10 fibs2
-- [1,1,2,3,5,8,13,21,34,55]

{- EXERCISE 3 -}

-- 1. Define a data type of polymorphic streams, Stream.
data Stream a = Cons a (Stream a)

-- 2. Write a function to convert a Stream to an infinite list,
--   streamToList :: Stream a -> [a]
streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

-- 3. To test your Stream functions in the succeeding exercises,
-- it will be useful to have an instance of Show for Streams.
-- However, if you put `deriving Show` after your definition of
-- Stream, as one usually does, the resulting instance will try
-- to print an entire Stream—which, of course, will never finish.
-- Instead, you should make your own instance of Show for Stream,
--   instance Show a => Show (Stream a) where
--     show ...
-- which works by showing only some prefix of a stream (say, the
-- first 20 elements).
instance Show a => Show (Stream a) where
  show = show . take 20 . streamToList

{- EXERCISE 4 -}

-- Write a function
--   streamRepeat :: a -> Stream a
-- which generates a stream containing infinitely many copies of the
-- given element.
streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

-- Write a function
--   streamMap :: (a -> b) -> Stream a -> Stream b
-- which applies a function to every element of a Stream.
streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) $ streamMap f xs

-- Write a function
--   streamFromSeed :: (a -> a) -> a -> Stream a
-- which generates a Stream from a "seed" of type a, which is the
-- first element of the stream, and an "unfolding rule" of type a -> a
-- which specifies how to transform the seed into a new seed, to be
-- used for generating the rest of the stream.
streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x (streamFromSeed f $ f x)

-- *Fibonacci> streamRepeat 42
-- [42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42]
-- *Fibonacci> streamMap (\x -> x * x) $ streamFromSeed (+ 1) 1
-- [1,4,9,16,25,36,49,64,81,100,121,144,169,196,225,256,289,324,361,400]

{- EXERCISE 5 -}

-- 1. Define the stream
--   nats :: Stream Integer
-- which contains the infinite list of natural numbers 0, 1, 2, ...
nats :: Stream Integer
nats = streamFromSeed (+ 1) 0

-- 2. Define the stream
--   ruler :: Stream Integer
-- which corresponds to the ruler function
--   0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 4, ...
-- where the nth element in the stream (assuming the first element
-- corresponds to n = 1) is the largest power of 2 which evenly
-- divides n. Hint: define a function interleaveStreams which alternates
-- the elements from two streams. Can you use this function to implement
-- ruler in a clever way that does not have to do any divisibility testing?
interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons x xs) ys = Cons x (interleaveStreams ys xs)

mkRuler :: Integer -> Stream Integer
mkRuler n = interleaveStreams (streamRepeat n) $ mkRuler $ n + 1

ruler :: Stream Integer
ruler = mkRuler 0
