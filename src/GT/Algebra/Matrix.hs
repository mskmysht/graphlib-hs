{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

module GT.Algebra.Matrix where

import Numeric.LinearAlgebra.Data as LA
import GT.Graph.Class
import Data.List (elemIndex)
-- import Data.Foldable as F (toList)
import Data.Tuple (swap)
import Data.Witherable (Witherable, mapMaybe)

adjacencyMatrix :: forall (d :: IsDirect) n' e' g. (Direction g d, Eq n', Unwrap (Pair n') e', Graph (g d) n' e' d) => g d -> Matrix R
adjacencyMatrix g = LA.accum (konst 0 (n, n)) (+) as' where
  n = nodeCount g
  as' = if isDirect g then as else as ++ fmap (\(p, a) -> (swap p, a)) as
  es' :: [e']
  es' = edges g
  as = mapMaybe (\e -> do
      let (n, m) = unwrap e
      let ns = nodes g
      i <- elemIndex n ns
      j <- elemIndex m ns
      return ((i, j), 1)
    ) es'

degreeMatrix :: forall (d :: IsDirect) n' e' g . (Unwrap NodeId n', Graph (g d) n' e' d) => g d -> Matrix R
degreeMatrix g = diagl $ mapMaybe (\n -> fromIntegral <$> degree (unwrap n) g ) ns' where
  ns' :: [n']
  ns' = nodes g
 
-- Direction g d, 
laplacian :: forall (d :: IsDirect) n' e' g. (Pairing d, Direction g d, Eq n', Unwrap NodeId n', Unwrap (Pair n') e', Graph (g d) n' e' d) => g d -> Matrix R
laplacian g = degreeMatrix g - adjacencyMatrix g
