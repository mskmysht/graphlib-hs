{-# LANGUAGE FlexibleContexts #-}

module GT.Algebra.Matrix 
  ( adjacencyMatrix
  , degreeMatrix
  , laplacianMatrix) where

import Numeric.LinearAlgebra.Data as LA
import GT.Graph.Class
import Data.Maybe (fromJust)
import Data.Sequence as Q (Seq, elemIndexL)
import Data.Tuple (swap)
import Data.Witherable (Witherable, mapMaybe)

adjacencyMatrix :: Graph g n n' e e' d => g -> Matrix R
adjacencyMatrix g = LA.accum (konst 0 (n, n)) (+) as' where
  n = nodeCount g
  as' = case direction g of
      Directed -> as
      Undirected -> as ++ fmap (\(rc, a) -> (swap rc, a)) as
  is = nodeMap unwrap g :: Seq NodeId
  as = edgeMap (\e' ->
    let
      (i, j) = unwrap e'
      r = fromJust $ elemIndexL i is
      c = fromJust $ elemIndexL j is
    in ((r, c), 1)
    ) g

degreeMatrix :: Graph g n n' e e' d => g -> Matrix R
degreeMatrix g = diagl $ nodeMap (\n' -> fromIntegral (fromJust $ degree (unwrap n') g)) g
 
laplacianMatrix :: Graph g n n' e e' d => g -> Matrix R
laplacianMatrix g = degreeMatrix g - adjacencyMatrix g
