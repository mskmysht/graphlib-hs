{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import GT.Graph
  ( Builder (assoc),
    EdgeId,
    Graph (direction, edges, isSink, isSource, nodes),
    NodeId,
    UndiGr,
  )

assoc' :: (Graph g n n' e e' d, Builder g n e, Foldable f) => f n' -> f e' -> g
assoc' = assoc

main :: IO ()
main = do
  let es = [(3, 0), (1, 2), (2, 1), (3, 1)]
  let g = assoc' @UndiGr ([]) es
  let ns :: [NodeId] = nodes g
  let es :: [EdgeId] = edges g
  print ns
  print es
  print $ direction g
  print $ isSource g 0 (0, 1)
  print $ isSink g 0 (0, 1)
