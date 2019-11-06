{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module GT.Algorithm.Propagation
  ( propagate
  , propagateUntil
  ) where

import GT.Graph.Class
import Control.Monad.State.Strict (StateT, runStateT, get, put, foldM)
import Data.Pointed (Pointed(..))
import Control.Monad.Trans (lift)
import Control.Monad.Primitive (PrimMonad)
import Control.Monad.Trans.Maybe (runMaybeT)
import System.Random.SFMT (Gen, MonadGen, uniformR, initializeFromSeed)
import Control.Monad.ST (runST)

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
