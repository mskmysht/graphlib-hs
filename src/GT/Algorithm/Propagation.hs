{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module GT.Algorithm.Propagation where

import GT.Graph.Class
import Control.Monad.State.Strict
import Data.Set
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import System.Random.SFMT

propagate :: forall n' g t e a . (Unwrap NodeId n', EdgeAccessor g t n' e) =>
  (e -> Double)
   -> g
   -> (Set NodeId -> Set NodeId -> a -> a)
   -> a
   -> GenIO
   -> StateT (Set NodeId, Set NodeId) IO a
propagate pe g f a gen = do
  (cas, tas) <- get
  nas <- foldM (\nas i -> do
    ms <- runMaybeT $ adjFoldM i g (\s n e -> liftIO $ do
      let j = unwrap n
      if member j tas then return s
      else do
        p <- uniformR (0, 1) gen
        return $
          if p <= pe e then insert j s
          else s
      ) empty
    return $ case ms of
      Just s -> nas `union` s
      Nothing -> nas
    ) empty cas
  let tas' = tas `union` nas
  put (nas, tas')
  return $ f nas tas' a


propagateUntil :: forall n' g t e a . (Unwrap NodeId n', EdgeAccessor g t n' e) =>
  (e -> Double)
   -> g
   -> (Set NodeId -> Set NodeId -> a -> a)
   -> (a -> Bool)
   -> Set NodeId
   -> a
   -> Int
   -> IO (a, (Set NodeId, Set NodeId))
propagateUntil pe g f p ias a seed = do
  gen <- initializeFromSeed seed
  runStateT (loop gen $ f ias ias a) (ias, ias)
  where
    loop gen a'
      | p a' = return a'
      | otherwise = propagate pe g f a' gen >>= loop gen
   
