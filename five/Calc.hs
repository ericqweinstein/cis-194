{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Calc where

import ExprT (ExprT(Lit), ExprT(Add), ExprT(Mul))
import Parser (parseExp)
import StackVM (StackExp(PushI), StackExp(Add), StackExp(Mul), Program)

{- EXERCISE 1 -}

-- Write Version 1 of the calculator: an
-- evaluator for ExprT, with the signature
--   eval :: ExprT -> Integer
-- For example:
--   eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) == 20.
eval :: ExprT -> Integer
eval (ExprT.Lit x)   = x
eval (ExprT.Add x y) = eval x + eval y
eval (ExprT.Mul x y) = eval x * eval y

{- EXERCISE 2 -}

-- The UI department has sent you the module Parser.hs, which
-- exports parseExp, a parser for arithmetic expressions. If
-- you pass the constructors of ExprT to it as arguments, it
-- will convert Strings representing arithmetic expressions
-- into values of type ExprT. For example:
--   *Calc> parseExp Lit Add Mul "(2+3)*4"
--     Just (Mul (Add (Lit 2) (Lit 3)) (Lit 4))
--   *Calc> parseExp Lit Add Mul "2+3*4"
--     Just (Add (Lit 2) (Mul (Lit 3) (Lit 4)))
--   *Calc> parseExp Lit Add Mul "2+3*"
--     Nothing
-- Leverage the assets of the UI team to implement the
-- value-added function
--   evalStr :: String -> Maybe Integer
-- which evaluates arithmetic expressions given as a String,
-- producing Nothing for inputs which are not well-formed
-- expressions, and Just n for well-formed inputs that
-- evaluate to n.
evalStr :: String -> Maybe Integer
evalStr = fmap eval . parseExp ExprT.Lit ExprT.Add ExprT.Mul

{- EXERCISE 3 -}

-- Create a type class called Expr with three methods
-- called `lit`, `add`, and `mul` which parallel the
-- constructors of ExprT. Make an instance of Expr
-- for the ExprT type, in such a way that
--   mul (add (lit 2) (lit 3)) (lit 4) :: ExprT
--   == Mul (Add (Lit 2) (Lit 3)) (Lit 4)
class Expr n where
  lit :: Integer -> n
  add :: n -> n -> n
  mul :: n -> n -> n

instance Expr ExprT where
  lit = ExprT.Lit
  add = ExprT.Add
  mul = ExprT.Mul

{- EXERCISE 4 -}

-- Make instances of Expr for each of the following types:
--   * Integer — works like the original calculator
--   * Bool — every literal value less than or equal to 0
--     is interpreted as False, and all positive Integers
--     are interpreted as True; "addition" is logical or,
--     "multiplication" is logical and
--   * MinMax — "addition" is taken to be the max function,
--     while "multiplication" is the min function
--   * Mod7 — all values should be in the ranage 0...6, and
--     all arithmetic is done modulo 7; for example, 5 + 3 = 1.
instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit x
    | x > 0     = True
    | otherwise = False
  add           = (||) -- Sum type
  mul           = (&&) -- Product type

-- The last two variants work with Integers internally,
-- but in order to provide different instances, we wrap
-- those Integers in `newtype` wrappers.
newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where
  lit = MinMax
  add (MinMax x) (MinMax y) = MinMax (max x y)
  mul (MinMax x) (MinMax y) = MinMax (min x y)

instance Expr Mod7 where
  lit x = Mod7 (x `mod` 7)
  add (Mod7 x) (Mod7 y) = Mod7 ((x + y) `mod` 7)
  mul (Mod7 x) (Mod7 y) = Mod7 ((x * y) `mod` 7)

-- Once done, the following code should
-- demonstrate our family of calculators:
testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool    = testExp :: Maybe Bool
testMM      = testExp :: Maybe MinMax
testSat     = testExp :: Maybe Mod7

-- *Calc> print testExp
--   Just (-7)
-- *Calc> print testInteger
--   Just (-7)
-- *Calc> print testBool
--   Just True
-- *Calc> print testMM
--   Just (MinMax 5)
-- *Calc> print testSat
--   Just (Mod7 0)

{- EXERCISE 5 -}

-- Implement a compiler for arithmetic expressions. Simply create
-- an instance of the Expr type class for Program, so that arithmetic
-- expressions can be interpreted as compiled programs. For any
-- arithmetic expression exp :: Expr a => a it should be the case that
--   stackVM exp == Right [IVal exp]
-- Finally, put together the pieces you have to create a function
--   compile :: String -> Maybe Program
-- which takes Strings representing arithmetic expressions and
-- compiles them into programs that can be run on the custom CPU.
instance Expr Program where
  lit x = [PushI x]
  add x y = x ++ y ++ [StackVM.Add]
  mul x y = x ++ y ++ [StackVM.Mul]

compile :: String -> Maybe Program
compile = parseExp lit add mul
