{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Surreal where

import Data.List (intercalate)
import qualified Data.Set as Set
import Data.Ratio (numerator, denominator)

-- | A surreal number is defined by its left and right sets
-- Left set contains numbers less than this number
-- Right set contains numbers greater than this number
-- This implements Conway's construction where each number is a "cut"
data Surreal = Surreal 
    { left :: Set.Set Surreal
    , right :: Set.Set Surreal
    } deriving (Read)

-- | Binary tree for the genealogy of surreal numbers
-- Each node has a value and optional left and right children
-- This represents how numbers are constructed day by day
data Tree a = Node a (Maybe (Tree a)) (Maybe (Tree a))
    deriving (Read, Eq, Show)

-- | The empty set - fundamental building block
empty :: Set.Set a
empty = Set.empty

-- | Helper function to construct surreal numbers from optional left/right values
-- This implements the fundamental construction rule: {L|R} where L < R
(<|>) :: Maybe Surreal -> Maybe Surreal -> Surreal
(<|>) Nothing Nothing = Surreal empty empty        -- zero: { | }
(<|>) Nothing (Just r) = Surreal empty (Set.singleton r)   -- less than r
(<|>) (Just l) Nothing = Surreal (Set.singleton l) empty   -- greater than l
(<|>) (Just l) (Just r) = Surreal (Set.singleton l) (Set.singleton r) -- between l and r

-- | Fundamental surreal numbers
surZero :: Surreal
surZero = Nothing <|> Nothing  -- { | }

surOne :: Surreal
surOne = Just surZero <|> Nothing  -- {0|}

surNOne :: Surreal
surNOne = Nothing <|> Just surZero  -- {|0}

-- | Helper for displaying surreal numbers
show' :: (Surreal -> Set.Set Surreal) -> Surreal -> [String]
show' f = Set.toList . Set.map show . f

-- | Implementation of Conway's ordering for surreal numbers
instance Ord Surreal where
    (<=) :: Surreal -> Surreal -> Bool
    x <= y = 
        not (any (>= y) (Set.toList $ left x)) && 
        not (any (<= x) (Set.toList $ right y))

-- | Equality based on cuts being identical
instance Eq Surreal where
    (==) :: Surreal -> Surreal -> Bool
    x == y = x <= y && y <= x

-- | Helper for adding to sets of surreals
add :: Set.Set Surreal -> Surreal -> Set.Set Surreal
add x y = Set.map (+ simplify y) x

-- | Arithmetic operations following Conway's rules
instance Num Surreal where
    -- Negation: swap and negate left and right sets
    negate :: Surreal -> Surreal
    negate x
        | x == surZero = x
        | otherwise = simplify $ Surreal 
            (Set.map negate (right x))
            (Set.map negate (left x))

    -- Addition combines left and right sets according to Conway's rules
    (+) :: Surreal -> Surreal -> Surreal
    (+) x y =
        let sx = simplify x
            sy = simplify y
         in simplify $ Surreal 
              (Set.union (add (left sy) sx) (add (left sx) sy))
              (Set.union (add (right sy) sx) (add (right sx) sy))

    -- Multiplication follows Conway's formula
    (*) :: Surreal -> Surreal -> Surreal
    (*) x y
        | x == surOne = y
        | y == surOne = x
        | x == surZero || y == surZero = surZero
        | otherwise =
            let sx = simplify x
                sy = simplify y
                sOp op s s' = Set.unions $ Set.map (\si -> Set.map (op (simplify si) . simplify) s') s
                addS s s' = if Set.null s || Set.null s' then empty else sOp (+) s s'
                subS s s' = if Set.null s || Set.null s' then empty else sOp (-) s s'
                multS = sOp (*)
                multl set sn = if Set.null set then empty else Set.map (* simplify sn) set
                a = (multl (left sx) sy) `addS` (multl (left sy) sx) `subS` (multS (left sx) (left sy))
                b = (multl (right sx) sy) `addS` (multl (right sy) sx) `subS` (multS (right sx) (right sy))
                c = (multl (left sx) sy) `addS` (multl (right sy) sx) `subS` (multS (left sx) (right sy))
                d = (multl (right sx) sy) `addS` (multl (left sy) sx) `subS` (multS (right sx) (left sy))
             in Surreal (Set.union a b) (Set.union c d)

    abs :: Surreal -> Surreal
    abs s
        | s < surZero = negate s
        | otherwise = s

    signum :: Surreal -> Surreal
    signum s
        | s < surZero = surNOne
        | s > surZero = surOne
        | otherwise = surZero

    fromInteger :: Integer -> Surreal
    fromInteger n
        | n > 0 = surOne + fromInteger (n - 1)
        | n < 0 = surNOne + fromInteger (n + 1)
        | otherwise = surZero

-- | Display surreal numbers in Conway's notation
instance Show Surreal where
    show s = "{" ++ intercalate "," (show' left s) ++ "|" ++ 
             intercalate "," (show' right s) ++ "}"

-- | Generate next day's numbers from current day
nextDay :: [Surreal] -> [Surreal]
nextDay xs = go xs []
  where
    go [] _ = [surZero]
    go [a] [] = [Nothing <|> Just a, a, Just a <|> Nothing]
    go (a : as) [] = go as [Nothing <|> Just a, a, Just a <|> Just (head as)]
    go [a] acc = acc ++ [Just a <|> Nothing, a]
    go (a : as) acc = go as acc ++ [a, Just a <|> Just (head as)]

-- | Generate all surreal numbers up to a given day
genSurreal :: Int -> Set.Set Surreal
genSurreal d = Set.fromList $ iterate nextDay [surZero] !! d

-- | Find the simplest form of a surreal number
findSimple :: Surreal -> Surreal -> Surreal
findSimple n s
    | s < n = findSimple (safeChild left n <|> Just n) s
    | s > n = findSimple (Just n <|> safeChild right n) s
    | otherwise = n

-- | Simplify a surreal number to its canonical form
simplify :: Surreal -> Surreal
simplify = findSimple surZero

-- | Helper for safely getting a child from a set
safeChild :: (Surreal -> Set.Set Surreal) -> Surreal -> Maybe Surreal
safeChild op s = if Set.null (op s) then Nothing else Just (Set.elemAt 0 (op s))

-- | Apply a function n times
applyN :: (a -> a) -> Int -> a -> a
applyN f n a = iterate f a !! n

-- | Generate the nth successor of a number
succ' :: Int -> Surreal -> Surreal
succ' n x = applyN (\s -> Just s <|> safeChild right s) n x

-- | Division of surreal numbers
divide :: Surreal -> Surreal -> Maybe Surreal
divide _ y | y == surZero = Nothing  -- division by zero
divide x y = 
    let r = reciprocal y
    in case r of
         Nothing -> Nothing
         Just v -> Just $ simplify (x * v)
  where
    reciprocal :: Surreal -> Maybe Surreal
    reciprocal z
        | z == surZero = Nothing
        | z == surOne = Just surOne
        | otherwise = findReciprocal z surZero surOne
    
    findReciprocal :: Surreal -> Surreal -> Surreal -> Maybe Surreal
    findReciprocal target low high
        | low == high = Just $ simplify low
        | otherwise = 
            case midpoint low high of
                Nothing -> Just low
                Just mid -> 
                    if mid * target < surOne
                    then findReciprocal target mid high
                    else findReciprocal target low mid
      where
        midpoint l h = divide (l + h) (fromInteger 2)

-- | Binary search for surreal numbers
binarySearch :: Surreal -> Surreal -> Surreal
binarySearch low high
    | low == high = low
    | otherwise = 
        case midpoint low high of
            Nothing -> low
            Just mid -> if mid * mid <= high
                       then binarySearch mid high
                       else binarySearch low mid
  where
    midpoint :: Surreal -> Surreal -> Maybe Surreal
    midpoint l h = divide ((l + h) * fromInteger 1) (fromInteger 2)

-- | Convert rational numbers to surreal numbers
fromRational' :: Rational -> Surreal
fromRational' r =
    let n = fromInteger (numerator r)
        d = fromInteger (denominator r)
    in case divide n d of
         Just x -> x
         Nothing -> error "Division by zero"


-- | Test function
main :: IO ()
main = do
    putStrLn "Basic surreal numbers:"
    print surZero  -- { | }
    print surOne   -- {0|}
    print surNOne  -- {|0}
    putStrLn "\nArithmetic:"
    print (surOne + surOne)     -- 2
    print (surOne * surOne)     -- 1
    print (succ' 2 surZero)     -- 2
    putStrLn "\nComplex calculation:"
    print (succ' 4 surZero * succ' 2 surZero)  -- 8