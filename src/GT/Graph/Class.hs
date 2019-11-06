{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}

module GT.Graph.Class where

import Control.Monad.Trans.Maybe (MaybeT(..))
-- import Data.Pointed (Pointed)
import Data.Proxy (Proxy(..))
import Data.Witherable (Witherable)
import Control.Lens (Cons)

import GHC.Exts (Constraint)

type Pair n = (n, n)

type NodeId = Int
type EdgeId = Pair NodeId

type With i v = (i, v)
type NWith n = With NodeId n
type EWith e = With EdgeId e


data Direction = Directed | Undirected deriving Show

class Directing (d :: Direction) where
  ppair :: Proxy d -> NodeId -> NodeId -> EdgeId
  pisSource :: Proxy d -> NodeId -> EdgeId -> Bool
  pisSink :: Proxy d -> NodeId -> EdgeId -> Bool
  pdirection :: Proxy d -> Direction

instance Directing 'Directed where
  ppair _ source sink = (source, sink)
  {-# INLINE ppair #-}
  pisSource _ i (source, _) = i == source
  {-# INLINE pisSource #-}
  pisSink _ i (_, sink) = i == sink
  {-# INLINE pisSink #-}
  pdirection _ = Directed
  {-# INLINE pdirection #-}

instance Directing 'Undirected where
  ppair _ i j
    | i < j     = (i, j)
    | otherwise = (j, i)
  {-# INLINE ppair #-}
  pisSource _ i (i', j') = i == i' || i == j'
  {-# INLINE pisSource #-}
  pisSink _ i (i', j') = i == i' || i == j'
  {-# INLINE pisSink #-}
  pdirection _ = Undirected
  {-# INLINE pdirection #-}


opposite :: NodeId -> EdgeId -> Maybe NodeId
opposite i (i', j')
  | i /= i' && i /= j' = Nothing
  | i == i'            = Just j'
  | otherwise          = Just i'
{-# INLINE opposite #-}

opposite' :: NodeId -> EdgeId -> NodeId
opposite' i p@(i', j')
  | i == i' = j'
  | i == j' = i'
  | otherwise = error $ "Edge " ++ show p ++ "does not contain node " ++ show i ++ "."
{-# INLINE opposite' #-}

class Unwrap i w => Wrap i v w where
  wrap :: i -> v -> w
  wfoldr :: Foldable f => (i -> v -> r -> r) -> r -> f w -> r

instance Wrap i v (With i v) where
  wrap = (,)
  {-# INLINE wrap #-}
  wfoldr f = foldr (\(i, v) r -> f i v r)
  {-# INLINE wfoldr #-}

instance Wrap a a a where
  wrap = const
  {-# INLINE wrap #-}
  wfoldr f = foldr (\a r -> f a a r)
  {-# INLINE wfoldr #-}

class Unwrap i w where
  unwrap :: w -> i

instance Unwrap i (With i v) where
  unwrap = fst
  {-# INLINE unwrap #-}

instance {-# OVERLAPS #-} (a ~ b) => Unwrap a b where
  unwrap = id
  {-# INLINE unwrap #-}

class (Directing d, Wrap NodeId n n', Unwrap NodeId n', Wrap EdgeId e e', Unwrap EdgeId e') => Graph g n n' e e' d | g -> n n' e e' d where
  pair :: g -> NodeId -> NodeId -> EdgeId
  isSource :: g -> NodeId -> EdgeId -> Bool
  isSink :: g -> NodeId -> EdgeId -> Bool
  direction :: g -> Direction
  nodeCount :: g -> Int
  edgeCount :: g -> Int
  removeEdge :: NodeId -> NodeId -> g -> g
  getNode :: NodeId -> g -> n'
  getNode i g = wrap i (getNodeValue i g)
  {-# INLINE getNode #-}
  getNodeValue :: NodeId -> g -> n
  getEdge :: EdgeId -> g -> e'
  getEdge p g = wrap p (getEdgeValue p g)
  {-# INLINE getEdge #-}
  getEdgeValue :: EdgeId -> g -> e
  nodes :: (Cons (t n') (t n') n' n', Monoid (t n')) => g -> t n'
  nodes = nodeMap id
  {-# INLINE nodes #-}
  edges :: (Cons (t e') (t e') e' e', Monoid (t e')) => g -> t e'
  edges = edgeMap id
  {-# INLINE edges #-}
  nodeMap    :: (Cons (t r) (t r) r r, Monoid (t r)) => (n' -> r) -> g -> t r
  edgeMap    :: (Cons (t r) (t r) r r, Monoid (t r)) => (e' -> r) -> g -> t r
  adjMap     :: (Cons (t r) (t r) r r, Monoid (t r)) => (n' -> e' -> r) -> NodeId -> g -> Maybe (t r)
  adjNodeMap :: (Cons (t r) (t r) r r, Monoid (t r)) => (n' -> r) -> NodeId -> g -> Maybe (t r)
  adjNodeMap f = adjMap (\n' e' -> f n')
  {-# INLINE adjNodeMap #-}
  adjFoldl     :: (r -> n' -> e' -> r) -> r -> NodeId -> g -> Maybe r
  adjNodeFoldl :: (r -> n' -> r) -> r -> NodeId -> g -> Maybe r
  adjNodeFoldl f = adjFoldl (\r n' _ -> f r n')
  {-# INLINE adjNodeFoldl #-}
  adjFoldlM    :: Monad m => (r -> n' -> e' -> m r) -> r -> NodeId -> g -> MaybeT m r
  adjForM_   :: Monad m => NodeId -> g -> (n' -> e' -> m r) -> MaybeT m ()
  degree :: NodeId -> g -> Maybe Int
  degree = adjNodeFoldl (\a _ -> a + 1) 0
  {-# INLINE degree #-}


class Builder g n e | g -> n e where
  -- n corresponds NodeId by the index of each n
  assoc :: (Foldable f1, Foldable f2, Wrap NodeId n n', Wrap EdgeId e e') => f1 n' -> f2 e' -> g
  build :: (Traversable t1, Traversable t2) => t1 NodeId -> (NodeId -> n) -> t2 EdgeId -> (NodeId -> NodeId -> e) -> g
