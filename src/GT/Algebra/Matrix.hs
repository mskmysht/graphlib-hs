{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GT.Algebra.Matrix where

import Numeric.LinearAlgebra.Data as LA
import GT.Graph.Class
import Data.Foldable as F (toList)
import Data.Witherable (Filterable, mapMaybe)
import Data.Tuple (swap)

adjacencyMatrix :: forall n' e' g t d. (Direction g d, Filterable t, Eq n', Unwrap (Pair n') e', Graph (g d) t n' e') => g d -> Matrix R
adjacencyMatrix g = LA.accum (konst 0 (n, n)) (+) as' where
  n = nodeSize g
  as' = if isDirect g then as else as ++ fmap (\(p, a) -> (swap p, a)) as
  as = F.toList $ mapMaybe (\e -> do
      let (n, m) = unwrap e
      i <- findNodeIndex n g
      j <- findNodeIndex m g
      return ((i, j), 1)
    ) $ edges g

degreeMatrix :: forall n' e' g t . (Filterable t, Unwrap NodeId n', Graph g t n' e') => g -> Matrix R
degreeMatrix g = diagl $ F.toList $ mapMaybe (\n -> fromIntegral <$> degree (unwrap n) g ) $ nodes g

laplacian :: forall n' e' g t d. (Direction g d, Filterable t, Eq n', Unwrap NodeId n', Unwrap (Pair n') e', Graph (g d) t n' e') => g d -> Matrix R
laplacian g = degreeMatrix g - adjacencyMatrix g
