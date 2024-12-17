{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.GraphViz as GV
import qualified Data.GraphViz.Types as GVT
import qualified Data.GraphViz.Printing as GVP
import qualified Data.GraphViz.Types.Monadic as GVM
import qualified Data.GraphViz.Attributes.Complete as GVA
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO
import System.Process (callCommand)
import Control.Monad (forM_)

-- Ordinal Type
data Ordinal = Finite Int | Transfinite Int Int
             deriving (Eq)

-- Add Ord instance for Ordinal
instance Ord Ordinal where
    compare (Finite a) (Finite b) = compare a b
    compare (Finite _) (Transfinite _ _) = LT
    compare (Transfinite _ _) (Finite _) = GT
    compare (Transfinite w1 k1) (Transfinite w2 k2) =
        case compare w1 w2 of
            EQ -> compare k1 k2
            other -> other

data Surreal = Zero
             | Node { left :: [Surreal], right :: [Surreal], birthday :: Ordinal }
             deriving (Eq)

-- Add Ord instance for Surreal
instance Ord Surreal where
    compare Zero Zero = EQ
    compare Zero (Node _ _ _) = LT
    compare (Node _ _ _) Zero = GT
    compare (Node l1 r1 b1) (Node l2 r2 b2) =
        case compare b1 b2 of
            EQ -> case (compare (length l1) (length l2), compare (length r1) (length r2)) of
                      (EQ, EQ) -> EQ
                      (EQ, c) -> c
                      (c, _) -> c
            other -> other


-- Graph Type
type Edge = (Node, Weight)
type Graph = Map.Map Node [Edge]

-- Initialize Graph
initializeGraph :: [(Node, [Edge])] -> Graph
initializeGraph = Map.fromList

-- Surreal Arithmetic
addOrdinals :: Ordinal -> Ordinal -> Ordinal
addOrdinals (Finite a) (Finite b) = Finite (a + b)
addOrdinals (Finite a) (Transfinite w k) = Transfinite w (k + a)
addOrdinals (Transfinite w k) (Finite b) = Transfinite w (k + b)
addOrdinals (Transfinite w1 k1) (Transfinite w2 k2) = Transfinite (w1 + w2) (k1 + k2)

-- Update Flow (Reduce Weights by 1)
updateFlow :: Graph -> Graph
updateFlow = Map.mapWithKey updateNodeFlow
  where
    updateNodeFlow _ = map (\(n, w) -> (n, addOrdinals w (Finite (-1))))

-- Detect Bottlenecks
detectBottlenecks :: Graph -> [Node]
detectBottlenecks graph =
  Map.keys $ Map.filter (\edges -> sumWeights edges < Finite 1) graph
  where
    sumWeights = foldr (addOrdinals . snd) (Finite 0)

-- Apply Perturbations (Add New Edges)
applyPerturbations :: Graph -> Graph
applyPerturbations graph =
  let newEdge = (Node [] [Zero] (Transfinite 2 0), Transfinite 1 0) -- ω^2
      updatedNode = fromMaybe [] (Map.lookup Zero graph) ++ [newEdge]
  in Map.insert Zero updatedNode graph

-- Update the Surreal Show instance to be more detailed
instance Show Surreal where
    show Zero = "0"
    show (Node l r b) = "S(" ++ showList l ++ "," ++ showList r ++ "," ++ show b ++ ")"
        where 
            showList [] = "[]"
            showList xs = show xs

instance Show Ordinal where
    show (Finite n) = show n
    show (Transfinite w 0)
        | w == 1    = "ω"
        | w > 1     = "ω^" ++ show w
        | otherwise = "ω^(" ++ show w ++ ")"  -- handle negative w
    show (Transfinite w k)
        | w == 1    = "ω + " ++ show k
        | w > 1     = "ω^" ++ show w ++ " + " ++ show k
        | otherwise = "ω^(" ++ show w ++ ") + " ++ show k  -- handle negative w

-- Type aliases need their own Show instances
type Node = Surreal
type Weight = Ordinal



-- Or if you prefer simpler output for nodes in the graph visualization:
showNode :: Node -> String
showNode Zero = "0"
showNode (Node _ _ b) = "node_" ++ show b

showWeight :: Weight -> String
showWeight = show

-- Then update the visualization code to use these:
visualizeGraph :: Graph -> FilePath -> IO ()
visualizeGraph graph filePath = do
    let dotGraph = GVM.digraph (GVT.Str "SurrealGraph") $ do
            GVM.graphAttrs [GVA.RankDir GVA.FromLeft]
            forM_ (Map.toList graph) $ \(src, edges) -> do
                forM_ edges $ \(dst, weight) -> do
                    GVM.edge (GVT.Str $ TL.pack $ showNode src)
                           (GVT.Str $ TL.pack $ showNode dst)
                           [GVA.Label $ GVA.StrLabel $ TL.pack $ showWeight weight]

    TLIO.writeFile (filePath ++ ".dot") (GV.printDotGraph dotGraph)
    callCommand $ "dot -Tpng -o " ++ filePath ++ ".png " ++ filePath ++ ".dot"

-- Convert Graph to DOT Representation

toDotRep :: Graph -> [(String, String, String)]
toDotRep graph =
  [ (showNode src, showNode dst, showWeight weight)
  | (src, edges) <- Map.toList graph
  , (dst, weight) <- edges
  ]


-- System Evaluation
evaluateSystem :: Graph -> IO ()
evaluateSystem graph = do
  putStrLn "System Evaluation:"
  putStrLn $ "Total Nodes: " ++ show (Map.size graph)
  putStrLn $ "Bottlenecks: " ++ show (detectBottlenecks graph)

-- Example Surreal Numbers
epsilon :: Surreal
epsilon = Node [Zero] [] (Transfinite 0 1)  -- Infinitesimal ω + 1

omega :: Surreal
omega = Node [] [Zero] (Transfinite 1 0)   -- Infinite ω

half :: Surreal
half = Node [Zero] [Node [Zero] [] (Finite 1)] (Finite 2)  -- 1/2

-- Main Program
main :: IO ()
main = do
  -- Step 1: Initialize Graph
  let initialGraph = initializeGraph
        [ (Zero, [(epsilon, Transfinite 0 1), (omega, Transfinite 1 0)])
        , (epsilon, [(omega, Transfinite 1 1)])
        , (omega, [(Zero, Transfinite 1 0)])
        ]
  putStrLn "Initial Graph:"
  print (Map.toList initialGraph)

  -- Step 2: Update Flow
  let updatedGraph = updateFlow initialGraph
  putStrLn "\nGraph After Flow Update:"
  print (Map.toList updatedGraph)

  -- Step 3: Detect Bottlenecks
  let bottlenecks = detectBottlenecks updatedGraph
  putStrLn "\nDetected Bottlenecks:"
  print bottlenecks

  -- Step 4: Apply Perturbations
  let perturbedGraph = applyPerturbations updatedGraph
  putStrLn "\nGraph After Applying Perturbations:"
  print (Map.toList perturbedGraph)

  -- Step 5: Evaluate System
  evaluateSystem perturbedGraph

  -- Step 6: Visualize Graph
  putStrLn "\nVisualizing Graph..."
  visualizeGraph perturbedGraph "surreal_graph"
  putStrLn "Graph visualization saved as surreal_graph.png."