{-# LANGUAGE Strict #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GT.Generator.RandomGraph 
  (barabasiAlbert
  )
where

import GT.Graph
import System.Random.SFMT
import Control.Monad.Primitive (PrimMonad)
import Data.Set as St
import Data.Sequence as S
import Data.Foldable as F (toList)
import Data.Maybe (fromJust)
import Control.Monad.Except (ExceptT(..))
import Control.Monad.Trans (lift)

barabasiAlbert :: forall d m n' e' g n e . (Pairing d, PrimMonad m, Wrap NodeId n n', Wrap EdgeId e e', Builder g n e, Graph g n' e' d) =>
  Int -- the number of node
   -> Int -- the number of each degree
   -> Int -- seed value
   -> (NodeId -> n) -- node builder
   -> (NodeId -> NodeId -> e) -- edge builder
   -> ExceptT String m g
barabasiAlbert n m seed nb eb
  | m < 1 || m >= n = ExceptT $ return $ Left $ "Barabási–Albert network must have m >= 1 and m < n, m = " ++ show m ++ ", n = " ++ show n
  | otherwise = lift $ initializeFromSeed seed >>= ba n m where
    ba :: Int -> Int -> MonadGen m -> m g
    ba n m gen = do 
      ses <- g m (S.fromList [0 .. m - 1]) S.empty S.empty
      return $ build [0 .. m - 1] nb ses eb
      where
        g :: Int -> Seq Int -> Seq EdgeId -> Seq Int -> m (Seq EdgeId)
        g s ts es rns
          | s == n = return es
          | otherwise = let
              ss = S.replicate m s
              !rns' = rns >< ss >< ts
            in rc rns' m >>= g (s + 1) rns' (es >< S.zip ss ts)
        rc :: Seq Int -> Int -> m (Seq Int)
        rc seq n = loop St.empty 0 where
          l = S.length seq
          loop :: Set Int -> Int -> m (Seq Int)
          loop t s
            | s == m = return $ S.fromList $ St.toList t
            | otherwise = do
                i <- uniformR (0, l - 1) gen
                let !x = fromJust $ seq S.!? i
                loop (St.insert x t) (if member x t then s + 1 else s)
