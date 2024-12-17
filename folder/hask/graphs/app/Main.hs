{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}

-- Import necessary libraries for graph visualization and data structures
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.GraphViz as GV
import qualified Data.GraphViz.Types as GVT
import qualified Data.GraphViz.Types.Monadic as GVM
import qualified Data.GraphViz.Attributes.Complete as GVA
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO
import System.Process (callCommand)
import Control.Monad (forM_)
import Debug.Trace (trace)

-- ====================================
-- Section 1: Fundamental Mathematical Structures
-- ====================================

-- Ordinal numbers form the backbone of our surreal number system
-- They can be finite (regular numbers) or transfinite (like ω)
data Ordinal = Finite Int | Transfinite Int Int
             deriving (Eq)

-- Show instance for pretty-printing ordinals
instance Show Ordinal where
    show (Finite n) = show n
    show (Transfinite w k)
        | w < 0     = "(-ω)^" ++ show (abs w) ++ (if k == 0 then "" else " + " ++ show k)
        | w == 0    = if k == 0 then "0" else show k
        | w == 1    = "ω" ++ (if k == 0 then "" else (if k > 0 then " + " else " - ") ++ show (abs k))
        | otherwise = "ω^" ++ show w ++ (if k == 0 then "" else (if k > 0 then " + " else " - ") ++ show (abs k))

-- Ordering for ordinals
instance Ord Ordinal where
    compare (Finite a) (Finite b) = compare a b
    compare (Finite _) (Transfinite _ _) = LT
    compare (Transfinite _ _) (Finite _) = GT
    compare (Transfinite w1 k1) (Transfinite w2 k2) =
        case compare w1 w2 of
            EQ -> compare k1 k2
            other -> other

-- First, we need to define how ordinal arithmetic works
addOrdinals :: Ordinal -> Ordinal -> Ordinal
addOrdinals (Finite a) (Finite b) = Finite (a + b)
addOrdinals (Finite a) (Transfinite w k) = Transfinite w (k + a)
addOrdinals (Transfinite w k) (Finite b) = Transfinite w (k + b)
addOrdinals (Transfinite w1 k1) (Transfinite w2 k2) = 
    Transfinite (w1 + w2) (k1 + k2)

-- We should also add multiplication for completeness
multiplyOrdinals :: Ordinal -> Ordinal -> Ordinal
multiplyOrdinals (Finite a) (Finite b) = Finite (a * b)
multiplyOrdinals (Finite 0) _ = Finite 0
multiplyOrdinals (Finite a) (Transfinite w k) = 
    if a > 0 
    then Transfinite w k 
    else Finite 0
multiplyOrdinals (Transfinite w1 k1) (Transfinite w2 k2) = 
    Transfinite (w1 + w2) (k1 * k2)

-- Surreal numbers extend ordinals to include numbers like ε
data Surreal = Zero
             | Node { left :: [Surreal], right :: [Surreal], birthday :: Ordinal }
             deriving (Eq)

instance Show Surreal where
    show Zero = "0"
    show (Node l r b) = "S(" ++ show l ++ "," ++ show r ++ "," ++ show b ++ ")"

-- ====================================
-- Section 2: Time Evolution and Transformations
-- ====================================

-- Represents how spaces can transform over time
data Transformation =
    Identity                            -- No change
  | LocalTransform Ordinal             -- Smooth local change
  | DiscontinuousTransform [Ordinal]   -- Jump discontinuities
  | ResonantTransform Ordinal Ordinal  -- Interaction between two times
  deriving (Show, Eq)

-- Monodromy captures how spaces change as we move through time
data Monodromy = Monodromy {
    transformationMap :: Map.Map HouseSpace Transformation,
    singularTimes :: [Ordinal],        -- Special times where continuity breaks
    memorySubspace :: [HouseSpace],    -- Spaces that maintain some continuity
    resonanceFunction :: Transformation -> Transformation -> Transformation
}

-- ====================================
-- Section 3: House Space Structures
-- ====================================

-- Different types of spaces in our impossible house
data HouseSpace =
    Space String           -- Normal, "real" spaces
  | UnSpace String        -- Impossible spaces
  | Abyss                -- The void beyond reality
  deriving (Eq, Ord, Show)

-- Types of distances between spaces
data HouseDistance =
    PhysicalDistance Int
  | SurrealDistance Ordinal
  | Paradox
  deriving (Eq, Show)

-- Edges can connect spaces in various ways
data MultiEdge =
    SingleEdge HouseSpace HouseDistance
  | InfiniteEdges HouseSpace HouseDistance Ordinal
  | RecursiveEdges HouseSpace HouseDistance Surreal
  | FractionalEdge HouseSpace HouseDistance Rational
  deriving (Show, Eq)

-- The core graph structure
type HouseGraph = Map.Map HouseSpace [MultiEdge]

-- A house that changes over time
data TimeEvolvingHouse = TimeEvolvingHouse {
    baseGraph :: HouseGraph,
    monodromy :: Monodromy,
    currentTime :: Ordinal
}

-- ====================================
-- Section 4: Mathematical Conditions
-- ====================================

-- Condition 1: Local Invertibility
isLocallyInvertible :: Monodromy -> Ordinal -> Bool
isLocallyInvertible m t =
    notElem t (singularTimes m) &&
    all isInvertibleTransform (Map.elems $ transformationMap m)
  where
    isInvertibleTransform Identity = True
    isInvertibleTransform (LocalTransform _) = True
    isInvertibleTransform _ = False


-- Condition 1: Memory Trace
hasMemoryTrace :: Monodromy -> Bool
hasMemoryTrace m = not (null (memorySubspace m)) &&
                   all (hasLimitAtSingularities m) (memorySubspace m)
  where
    hasLimitAtSingularities m space =
        all (\t -> leftLimit t space m == rightLimit t space m) (singularTimes m)

-- First, let's define what we mean by a limit in our space
data Limit =
    FiniteLimit Double
  | TransfiniteLimit Ordinal
  | Undefined
  deriving (Eq, Show)

leftLimit :: Ordinal -> HouseSpace -> Monodromy -> Limit
leftLimit t space m = case Map.lookup space (transformationMap m) of
    Just Identity -> FiniteLimit 0
    Just (LocalTransform ord) -> TransfiniteLimit ord
    Just (DiscontinuousTransform ords) ->
        if t `elem` ords
        then Undefined
        else FiniteLimit 0
    Just (ResonantTransform o1 o2) -> TransfiniteLimit (addOrdinals o1 o2)
    Nothing -> Undefined

rightLimit :: Ordinal -> HouseSpace -> Monodromy -> Limit
rightLimit t space m = case Map.lookup space (transformationMap m) of
    Just Identity -> FiniteLimit 0
    Just (LocalTransform ord) -> TransfiniteLimit ord
    Just (DiscontinuousTransform ords) ->
        if t `elem` ords
        then Undefined
        else FiniteLimit 0
    Just (ResonantTransform o1 o2) -> TransfiniteLimit (addOrdinals o1 o2)
    Nothing -> Undefined

-- Condition 3: Resonance
hasResonance :: Monodromy -> Bool
hasResonance m =
    all checkResonance (pairs (singularTimes m))
  where
    pairs xs = [(x,y) | x <- xs, y <- xs, x < y]
    checkResonance (ti, tj) =
        case Map.lookup (head $ memorySubspace m) (transformationMap m) of
            Just trans -> isResonant trans ti tj
            Nothing -> False
    isResonant trans t1 t2 = True  -- Simplified check

-- ====================================
-- Section 5: Example Spaces
-- ====================================

-- The infamous Five and Half Minute Hallway
fiveMinuteHallway :: HouseGraph
fiveMinuteHallway = Map.fromList
  [ (Space "Entrance",
      [InfiniteEdges (UnSpace "Hallway") (SurrealDistance (Transfinite 1 0)) (Finite 1)]),
    (UnSpace "Hallway",
      [SingleEdge (Space "Entrance") (PhysicalDistance 67),
       InfiniteEdges Abyss (SurrealDistance (Transfinite 2 0)) (Transfinite 1 0)])
  ]

-- A time-evolving version of the hallway
timeEvolvingHallway :: TimeEvolvingHouse
timeEvolvingHallway = TimeEvolvingHouse {
    baseGraph = fiveMinuteHallway,
    monodromy = Monodromy {
        transformationMap = Map.fromList [
            (Space "Entrance", Identity),
            (UnSpace "Hallway", LocalTransform (Transfinite 1 0)),
            (Abyss, DiscontinuousTransform [Transfinite 1 0, Transfinite 2 0])
        ],
        singularTimes = [Transfinite 1 0, Transfinite 2 0],
        memorySubspace = [Space "Entrance"],
        resonanceFunction = \_ _ -> Identity
    },
    currentTime = Finite 0
}

-- ====================================
-- Section 6: Visualization
-- ====================================

visualizeHouseGraph :: HouseGraph -> FilePath -> IO ()
visualizeHouseGraph graph filePath = do
    let dotGraph = GVM.digraph (GVT.Str "HouseGraph") $ do
            GVM.graphAttrs [GVA.RankDir GVA.FromLeft]
            forM_ (Map.toList graph) $ \(src, edges) ->
                forM_ edges $ \edge -> case edge of
                    SingleEdge dst dist ->
                        GVM.edge (GVT.Str $ TL.pack $ show src)
                               (GVT.Str $ TL.pack $ show dst)
                               [GVA.Label $ GVA.StrLabel $ TL.pack $ show dist]
                    InfiniteEdges dst dist n ->
                        GVM.edge (GVT.Str $ TL.pack $ show src)
                               (GVT.Str $ TL.pack $ show dst)
                               [GVA.Label $ GVA.StrLabel $ TL.pack $ show dist ++ " (×" ++ show n ++ ")"]
                    RecursiveEdges dst dist s ->
                        GVM.edge (GVT.Str $ TL.pack $ show src)
                               (GVT.Str $ TL.pack $ show dst)
                               [GVA.Label $ GVA.StrLabel $ TL.pack $ show dist ++ " (×" ++ show s ++ ")"]
                    FractionalEdge dst dist r ->
                        GVM.edge (GVT.Str $ TL.pack $ show src)
                               (GVT.Str $ TL.pack $ show dst)
                               [GVA.Label $ GVA.StrLabel $ TL.pack $ show dist ++ " (×" ++ show r ++ ")"]

    TLIO.writeFile (filePath ++ ".dot") (GV.printDotGraph dotGraph)
    callCommand $ "dot -Tpng -o " ++ filePath ++ ".png " ++ filePath ++ ".dot"

-- ====================================
-- Section 7: Main Program
-- ====================================

main :: IO ()
main = do
    putStrLn "=== Time-Evolving Surreal House Analysis ==="

    -- Visualize the base hallway
    putStrLn "\nVisualizing Five and Half Minute Hallway..."
    visualizeHouseGraph fiveMinuteHallway "hallway_base"

    -- Check mathematical conditions
    putStrLn "\nChecking Mathematical Properties..."
    putStrLn $ "Local Invertibility at t=0: " ++
        show (isLocallyInvertible (monodromy timeEvolvingHallway) (Finite 0))
    putStrLn $ "Has Memory Trace: " ++
        show (hasMemoryTrace (monodromy timeEvolvingHallway))
    putStrLn $ "Has Resonance: " ++
        show (hasResonance (monodromy timeEvolvingHallway))

    putStrLn "\nAnalysis complete. Check the generated PNG files for visualizations."