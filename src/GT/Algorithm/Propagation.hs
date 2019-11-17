{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module GT.Algorithm.Propagation
  ( independentCascade
  , independentCascadeDyn
  , propagate
  , propagateUntil
  ) where

import Control.Monad (unless)
import Control.Monad.RWS.Strict (RWST, runRWST, ask, tell, get, put)
import Control.Monad.State.Strict (StateT, runStateT, get, put, foldM)
import Control.Monad.Trans (lift)
import Control.Monad.Primitive (PrimMonad)
import Control.Monad.Trans.Maybe (runMaybeT)
import Data.Pointed (Pointed(..))
import Data.Maybe (fromJust)
import GT.Graph.Class
import System.Random.SFMT (Gen, MonadGen, uniformR, initializeFromSeed)
import Control.Monad.ST (runST)


-- | Total active nodes exclude current active nodes, i.e. they are historically active.
independentCascade :: (Graph g n n' e e' d, PrimMonad m, Pointed t, Foldable t, Monoid (t NodeId), Monoid w)
  => MonadGen m -> g -> (e' -> Double) -> (t NodeId -> t NodeId -> Bool) -> (t NodeId -> t NodeId -> w) -> t NodeId -> m ((), (t NodeId, t NodeId), w)
independentCascade gen g pe p f ias = runRWST loop () (ias, mempty) where
  loop = do
    (cas, tas) <- get   -- current active nodes, total active nodes
    t@(as', tas') <- stepIC gen g pe cas tas
    put t
    tell $ f as' tas'
    unless (p cas as') loop


-- | run independent cascade model in dynamic graph
independentCascadeDyn :: forall g n n' e e' d m t w . (Graph g n n' e e' d, PrimMonad m, Pointed t, Foldable t, Monoid (t NodeId), Monoid w)
  => MonadGen m  -- random variable generator
  -> g -- graph
  -> (e' -> Double) -- a map an edge to probability
  -> (t NodeId -> t NodeId -> g -> m g) -- graph updater
  -> (t NodeId -> t NodeId -> Bool) -- condition to terminate
  -> (t NodeId -> t NodeId -> g -> w) -- logging
  -> t NodeId  -- initial active nodes
  -> m ((), (t NodeId, t NodeId, g), w)
independentCascadeDyn gen g pe upd p f ias = runRWST loop () (ias, mempty, g) where
  loop = do
    (cas, tas, g) <- get   -- current active nodes, total active nodes, previous graph
    g' <- lift $ upd cas tas g
    (as', tas') <- stepIC gen g' pe cas tas
    put (as', tas', g')
    tell $ f as' tas' g'
    unless (p cas as') loop


stepIC :: (Graph g n n' e e' d, PrimMonad m, Pointed t, Foldable t, Monoid (t NodeId), Monoid w)
  => MonadGen m -> g -> (e' -> Double) -> t NodeId -> t NodeId -> RWST r w s m (t NodeId, t NodeId)
stepIC gen g pe cas tas = do
  as' <- foldM (\as i -> do
    ms <- runMaybeT $ adjFoldlM (\s n e -> lift $ do
      let j = unwrap n
      if elem j tas || elem j as || elem j s then return s
      else do
        p <- uniformR (0, 1) gen
        return $
          if p <= pe e then s <> point j
          else s
      ) mempty i g
    return $ as <> fromJust ms 
    ) mempty cas
  let tas' = tas <> as'
  return (tas, as')


-- | deprecated
propagate :: forall d n n' g e e' a m s. (Graph g n n' e e' d, PrimMonad m, Pointed s, Foldable s, Monoid (s NodeId))
  => (e' -> Double)
  -> g
  -> (s NodeId -> s NodeId -> a -> a)
  -> a
  -> MonadGen m
  -> StateT (s NodeId, s NodeId) m a
propagate pe g f a gen = do
  (cas, tas) <- get -- current actives, total actives
  nas <- foldM (\nas i -> do
    ms <- runMaybeT $ adjFoldlM (\s n e -> lift $ do
      let j = unwrap n
      if elem j tas || elem j nas || elem j s then return s
      else do
        p <- uniformR (0, 1) gen
        return $
          if p <= pe e then s <> point j
          else s
      ) mempty i g
    return $ case ms of
      Just s -> nas <> s
      Nothing -> nas
    ) mempty cas
  let tas' = tas <> nas
  put (nas, tas')
  return $ f nas tas' a


-- | deprecated
propagateUntil :: forall d n n' g e e' a m s. (Graph g n n' e e' d, Pointed s, Foldable s, Monoid (s NodeId))
  => (e' -> Double)
  -> g
  -> (s NodeId -> s NodeId -> a -> a)
  -> (a -> Bool)
  -> s NodeId
  -> a
  -> Int
  -> (a, (s NodeId, s NodeId))
propagateUntil pe g f p ias a seed = runST $ do
  gen <- initializeFromSeed seed
  runStateT (loop gen $ f ias ias a) (ias, ias)
  where
    loop :: forall m . PrimMonad m => MonadGen m -> a -> StateT (s NodeId, s NodeId) m a
    loop gen a'
      | p a' = return a'
      | otherwise = propagate pe g f a' gen >>= loop gen
