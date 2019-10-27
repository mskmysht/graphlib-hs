{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module GT.Algorithm.Propagation where

import GT.Graph.Class
import Control.Monad.State.Strict
import Data.Set
import Control.Monad.Trans
import Control.Monad.Primitive
import Control.Monad.Trans.Maybe
import System.Random.SFMT
import Control.Monad.ST (runST)

type MonadGen m = Gen (PrimState m)

propagate :: forall n' g t e a m s.
  ( Unwrap NodeId n', EdgeAccessor g t n' e, PrimMonad m, Applicative s, Foldable s, Monoid (s NodeId) ) =>
  (e -> Double)
   -> g
   -> (s NodeId -> s NodeId -> a -> a)
   -> a
   -> MonadGen m
   -> StateT (s NodeId, s NodeId) m a
propagate pe g f a gen = do
  (cas, tas) <- get -- current actives, total actives
  nas <- foldM (\nas i -> do
    ms <- runMaybeT $ adjFoldM i g (\s n e -> lift $ do
      let j = unwrap n
      if elem j tas || elem j nas || elem j s then return s
      else do
        p <- uniformR (0, 1) gen
        return $
          if p <= pe e then s <> pure j
          else s
      ) mempty
    return $ case ms of
      Just s -> nas <> s
      Nothing -> nas
    ) mempty cas
  let tas' = tas <> nas
  put (nas, tas')
  return $ f nas tas' a


propagateUntil :: forall n' g t e a m s.
  ( Unwrap NodeId n', EdgeAccessor g t n' e, Applicative s, Foldable s, Monoid (s NodeId) ) =>
  (e -> Double)
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
