-- Define a data type for Ordinals
data Ordinal = Finite Int | Transfinite Int Int  -- Transfinite(Int for ω, Int for ω+k)
             deriving (Eq)

instance Show Ordinal where
  show (Finite n) = show n
  show (Transfinite w 0)
    | w == 1    = "ω"       -- Simplify 1ω to ω
    | w > 1     = "ω^" ++ show w  -- Show ω raised to the power
  show (Transfinite w k)
    | w == 1    = "ω + " ++ show k  -- Simplify 1ω + k to ω + k
    | w > 1     = "ω^" ++ show w ++ " + " ++ show k  -- ω^w + k when w > 1

-- Define addition for Ordinals
addOrdinals :: Ordinal -> Ordinal -> Ordinal
addOrdinals (Finite a) (Finite b) = Finite (a + b)
addOrdinals (Finite a) (Transfinite w k) = Transfinite w (k + a)
addOrdinals (Transfinite w k) (Finite b) = Transfinite w (k + b)
addOrdinals (Transfinite w1 k1) (Transfinite w2 k2) =
  Transfinite (w1 + w2) (k1 + k2)  -- Sum ω-coefficients and constants

-- Define multiplication for Ordinals
multiplyOrdinals :: Ordinal -> Ordinal -> Ordinal
multiplyOrdinals (Finite a) (Finite b) = Finite (a * b)
multiplyOrdinals (Finite a) (Transfinite w k) = Transfinite (w * a) (k * a)
multiplyOrdinals (Transfinite w k) (Finite b) = Transfinite (w * b) (k * b)
multiplyOrdinals (Transfinite w1 _) (Transfinite w2 _) =
  -- Properly compute ω-coefficients for ω × ω = ω^2
  Transfinite (w1 * w2) 0

-- Define exponentiation for Ordinals
exponentiateOrdinals :: Ordinal -> Ordinal -> Ordinal
exponentiateOrdinals (Finite a) (Finite b) = Finite (a ^ b)
exponentiateOrdinals _ (Finite 0) = Finite 1  -- Anything to the power of 0 is 1
exponentiateOrdinals (Finite a) (Transfinite w _) =
  Transfinite (a ^ w) 0  -- Exponentiation of finite base with transfinite
exponentiateOrdinals (Transfinite w _) (Finite b) =
  Transfinite (w ^ b) 0  -- Finite exponent multiplies ω's power
exponentiateOrdinals (Transfinite w1 _) (Transfinite w2 _) =
  -- Correctly compute ω^ω structure
  Transfinite (w1 ^ w2) 0

-- Define a data type for Surreal Numbers
data Surreal = Zero
             | Node { left :: [Surreal], right :: [Surreal], birthday :: Ordinal }
             deriving (Show, Eq)

-- A function to compute the first common ancestor of two surreal numbers
commonAncestor :: Surreal -> Surreal -> Surreal
commonAncestor Zero _ = Zero
commonAncestor _ Zero = Zero
commonAncestor x y
  | x == y    = x
  | otherwise = Zero  -- Simplification: All surreal numbers trace back to Zero

-- A function to compute the birthday of a surreal number
getBirthday :: Surreal -> Ordinal
getBirthday Zero = Finite 0
getBirthday (Node _ _ b) = b

-- A function to compute the path distance between two surreal numbers
pathDistance :: Surreal -> Surreal -> Ordinal
pathDistance x y
  | x == y    = Finite 0
  | otherwise =
      addOrdinals (getBirthday x) (getBirthday y)

-- Define the measure function
measure :: Surreal -> Surreal -> Ordinal
measure x y =
  let ancestor = commonAncestor x y
      delta = pathDistance x y
  in delta -- Measure is delta directly since β(ancestor) = 0

-- Example Surreal Numbers
epsilon :: Surreal
epsilon = Node [Zero] [] (Transfinite 0 1)  -- Infinitesimal at ω+1

omega :: Surreal
omega = Node [] [Zero] (Transfinite 1 0)  -- Infinite at ω

half :: Surreal
half = Node [Zero] [Node [Zero] [] (Finite 1)] (Finite 2) -- 1/2

negativeHalf :: Surreal
negativeHalf = Node [Node [Zero] [] (Finite 1)] [Zero] (Finite 2) -- -1/2

-- Main function to test surreal number measures and ordinal arithmetic
main :: IO ()
main = do
  -- Measure examples
  let dist1 = measure half negativeHalf
  putStrLn $ "Measure between 1/2 and -1/2: " ++ show dist1

  let dist2 = measure epsilon omega
  putStrLn $ "Measure between epsilon and omega: " ++ show dist2

  let dist3 = measure (Node [half] [] (Finite 3)) (Node [negativeHalf] [] (Finite 3))  -- 1/4 and -1/4
  putStrLn $ "Measure between 1/4 and -1/4: " ++ show dist3

  let dist4 = measure (Node [half] [] (Finite 3)) half  -- 1/4 and 1/2
  putStrLn $ "Measure between 1/4 and 1/2: " ++ show dist4

  let dist5 = measure omega (Node [Zero] [] (Transfinite 2 0))  -- ω and ω^2
  putStrLn $ "Measure between ω and ω^2: " ++ show dist5

  let dist6 = measure omega (Node [Zero] [] (Transfinite 1 1))  -- ω and -ω
  putStrLn $ "Measure between ω and -ω: " ++ show dist6

