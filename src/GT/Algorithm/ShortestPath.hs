{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GT.Algorithm.ShortestPath where

import Control.Monad (forM_)
import Control.Monad.ST (ST, runST)
import Data.HashTable.Class as H (insert, mutateST, new)
import Data.HashTable.ST.Cuckoo as C (HashTable)
import Data.STRef (modifySTRef', newSTRef, readSTRef)
import Data.Sequence
  ( Seq,
    breakl,
    drop,
    index,
    length,
    singleton,
    (><),
    (|>),
  )
import GT.Graph (Graph (adjMap), NodeId, Unwrap (unwrap))
import Prelude hiding (drop, length)

dijkstra :: forall d w g n n' e e'. (Ord w, Num w, Graph g n n' e e' d) => (e' -> w) -> NodeId -> NodeId -> g -> Maybe (w, Seq NodeId)
dijkstra len si ti g = runST $ loop ns0 sdpt0
  where
    ns0 = singleton (0, si, singleton si)
    sdpt0 = do
      ht <- H.new
      H.insert ht si 0
      return ht

    loop :: forall s. Seq (w, NodeId, Seq NodeId) -> ST s (C.HashTable s NodeId w) -> ST s (Maybe (w, Seq NodeId))
    loop ns sdpt
      | length ns == 0 = return Nothing
      | i == ti = return $ Just (di, psi)
      | otherwise =
        case adjMap (\n' e -> let j = unwrap n' in (j, di + len e, psi |> j)) i g of
          Just (tdj :: Seq (NodeId, w, Seq NodeId)) -> do
            rns <- newSTRef $ drop 1 ns
            dpt <- sdpt
            forM_ tdj $ \(j, dj, psj) -> H.mutateST dpt j $ \mv -> do
              let (mdj, f) =
                    maybe
                      (Just dj, ins dj j psj)
                      ( \dj' ->
                          if
                              | dj' > dj -> (Just dj, insUpd dj j psj)
                              | otherwise -> (mv, id)
                      )
                      mv
              modifySTRef' rns f
              return (mdj, ())
            ns' <- readSTRef rns
            loop ns' $ return dpt
          Nothing -> loop (drop 1 ns) sdpt
      where
        (di, i, psi) = index ns 0

    insUpd :: w -> NodeId -> Seq NodeId -> Seq (w, NodeId, Seq NodeId) -> Seq (w, NodeId, Seq NodeId)
    insUpd d i ps ns = (l |> (d, i, ps)) >< m >< drop 1 r
      where
        (l, mr) = breakl (\(d', _, _) -> d' >= d) ns
        (m, r) = breakl (\(_, i', _) -> i' == i) mr

    ins :: w -> NodeId -> Seq NodeId -> Seq (w, NodeId, Seq NodeId) -> Seq (w, NodeId, Seq NodeId)
    ins d i ps ns = (l |> (d, i, ps)) >< r
      where
        (l, r) = breakl (\(d', _, _) -> d' >= d) ns
