{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Surreal where

import Data.List (intercalate)
import qualified Data.Set as Set

-- Simplified Surreal representation that only tracks essential information
data Surreal = 
    Zero
  | One
  | NegOne
  | Succ Surreal
  | Pred Surreal
  | Sum Surreal Surreal
  | Prod Surreal Surreal
  deriving (Eq)

-- Basic constructors
surZero :: Surreal
surZero = Zero

surOne :: Surreal
surOne = One

surNOne :: Surreal
surNOne = NegOne

-- Simplified ordering based on construction
instance Ord Surreal where
    compare Zero Zero = EQ
    compare One One = EQ
    compare NegOne NegOne = EQ
    compare (Succ x) (Succ y) = compare x y
    compare (Pred x) (Pred y) = compare x y
    compare Zero One = LT
    compare One Zero = GT
    compare Zero NegOne = GT
    compare NegOne Zero = LT
    compare x y = 
        case (normalize x, normalize y) of
            (nx, ny) | nx == ny -> EQ
                     | otherwise -> compare (evalToInt nx) (evalToInt ny)

-- Normalize surreal numbers to simplest form
normalize :: Surreal -> Surreal
normalize Zero = Zero
normalize One = One
normalize NegOne = NegOne
normalize (Succ Zero) = One
normalize (Pred Zero) = NegOne
normalize (Sum x y) = 
    case (normalize x, normalize y) of
        (Zero, y') -> y'
        (x', Zero) -> x'
        (x', y') -> addNormalized x' y'
normalize (Prod x y) = 
    case (normalize x, normalize y) of
        (Zero, _) -> Zero
        (_, Zero) -> Zero
        (One, y') -> y'
        (x', One) -> x'
        (x', y') -> multNormalized x' y'
normalize x = x

-- Helper function to evaluate to integer when possible
evalToInt :: Surreal -> Int
evalToInt Zero = 0
evalToInt One = 1
evalToInt NegOne = -1
evalToInt (Succ x) = 1 + evalToInt x
evalToInt (Pred x) = -1 + evalToInt x
evalToInt (Sum x y) = evalToInt x + evalToInt y
evalToInt (Prod x y) = evalToInt x * evalToInt y

-- Simplified arithmetic operations
addNormalized :: Surreal -> Surreal -> Surreal
addNormalized x y = 
    case (evalToInt x, evalToInt y) of
        (nx, ny) -> fromInt (nx + ny)

multNormalized :: Surreal -> Surreal -> Surreal
multNormalized x y = 
    case (evalToInt x, evalToInt y) of
        (nx, ny) -> fromInt (nx * ny)

-- Convert integer to Surreal
fromInt :: Int -> Surreal
fromInt 0 = Zero
fromInt 1 = One
fromInt (-1) = NegOne
fromInt n | n > 0 = Succ (fromInt (n-1))
         | n < 0 = Pred (fromInt (n+1))

instance Num Surreal where
    (+) = Sum
    (*) = Prod
    negate Zero = Zero
    negate One = NegOne
    negate NegOne = One
    negate (Succ x) = Pred (negate x)
    negate (Pred x) = Succ (negate x)
    negate (Sum x y) = Sum (negate x) (negate y)
    negate (Prod x y) = Prod (negate x) y
    abs x = if evalToInt x < 0 then negate x else x
    signum x = case compare x Zero of
                 LT -> NegOne
                 EQ -> Zero
                 GT -> One
    fromInteger n = fromInt (fromIntegral n)

-- Generate successor
succ' :: Int -> Surreal -> Surreal
succ' n x = iterate Succ x !! n

-- Show instance
instance Show Surreal where
    show x = case normalize x of
        Zero -> "0"
        One -> "1"
        NegOne -> "-1"
        s -> show (evalToInt s)

-- Pretty print
prettyPrint :: Surreal -> String
prettyPrint s = show s ++ " (normalized form)"

main :: IO ()
main = do
    putStrLn "Basic surreal numbers:"
    putStrLn $ prettyPrint surZero
    putStrLn $ prettyPrint surOne
    putStrLn $ prettyPrint surNOne
    
    putStrLn "\nArithmetic:"
    putStrLn $ prettyPrint (surOne + surOne)
    putStrLn $ prettyPrint (surOne * surOne)
    putStrLn $ prettyPrint (succ' 2 surZero)
    
    putStrLn "\nComplex calculation:"
    putStrLn $ prettyPrint (succ' 4 surZero * succ' 2 surZero)
    
    putStrLn "\nSome larger numbers:"
    mapM_ (putStrLn . prettyPrint) [succ' n surZero | n <- [1..5]]