{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}

module GT.Graph.Class where

import Control.Lens (Cons)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Proxy (Proxy (..))
import Data.Witherable (Witherable)
import GHC.Exts (Constraint)

type Pair n = (n, n)

type NodeId = Int

type EdgeId = Pair NodeId

type With i v = (i, v)

type NWith n = With NodeId n

type EWith e = With EdgeId e

data Direction = Directed | Undirected deriving (Show)

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
    | i < j = (i, j)
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
  | i == i' = Just j'
  | otherwise = Just i'
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
  nodeIVMap :: (Cons (t r) (t r) r r, Monoid (t r)) => (NodeId -> n -> r) -> g -> t r
  nodeMap :: (Cons (t r) (t r) r r, Monoid (t r)) => (n' -> r) -> g -> t r
  nodeMap f = nodeIVMap (\i n -> f $ wrap i n)
  {-# INLINE nodeMap #-}
  nodeIMap :: (Cons (t r) (t r) r r, Monoid (t r)) => (NodeId -> r) -> g -> t r
  nodeIMap f = nodeIVMap (\i _ -> f i)
  {-# INLINE nodeIMap #-}
  nodeIds :: (Cons (t NodeId) (t NodeId) NodeId NodeId, Monoid (t NodeId)) => g -> t NodeId
  nodeIds = nodeIVMap const
  {-# INLINE nodeIds #-}
  nodeIndices :: (Cons (t (NodeId, Int)) (t (NodeId, Int)) (NodeId, Int) (NodeId, Int), Monoid (t (NodeId, Int))) => g -> t (NodeId, Int)
  edgeIVMap :: (Cons (t r) (t r) r r, Monoid (t r)) => (EdgeId -> e -> r) -> g -> t r
  edgeMap :: (Cons (t r) (t r) r r, Monoid (t r)) => (e' -> r) -> g -> t r
  edgeMap f = edgeIVMap (\p e -> f $ wrap p e)
  {-# INLINE edgeMap #-}
  edgeIMap :: (Cons (t r) (t r) r r, Monoid (t r)) => (EdgeId -> r) -> g -> t r
  edgeIMap f = edgeIVMap (\p _ -> f p)
  {-# INLINE edgeIMap #-}
  edgeIds :: (Cons (t EdgeId) (t EdgeId) EdgeId EdgeId, Monoid (t EdgeId)) => g -> t EdgeId
  edgeIds = edgeIVMap const
  {-# INLINE edgeIds #-}
  edgeIndices :: (Cons (t (EdgeId, Int)) (t (EdgeId, Int)) (EdgeId, Int) (EdgeId, Int), Monoid (t (EdgeId, Int))) => g -> t (EdgeId, Int)
  nodeIVFoldl :: (r -> NodeId -> n -> r) -> r -> g -> r
  nodeFoldl :: (r -> n' -> r) -> r -> g -> r
  nodeFoldl f = nodeIVFoldl (\r i n -> f r $ wrap i n)
  {-# INLINE nodeFoldl #-}
  nodeIFoldl :: (r -> NodeId -> r) -> r -> g -> r
  nodeIFoldl f = nodeIVFoldl (\r i _ -> f r i)
  {-# INLINE nodeIFoldl #-}
  nodeIVFoldr :: (NodeId -> n -> r -> r) -> r -> g -> r
  nodeFoldr :: (n' -> r -> r) -> r -> g -> r
  nodeFoldr f = nodeIVFoldr (\i n r -> f (wrap i n) r)
  {-# INLINE nodeFoldr #-}
  nodeIFoldr :: (NodeId -> r -> r) -> r -> g -> r
  nodeIFoldr f = nodeIVFoldr (\i _ r -> f i r)
  {-# INLINE nodeIFoldr #-}
  edgeIVFoldl :: (r -> EdgeId -> e -> r) -> r -> g -> r
  edgeFoldl :: (r -> e' -> r) -> r -> g -> r
  edgeFoldl f = edgeIVFoldl (\r p e -> f r $ wrap p e)
  {-# INLINE edgeFoldl #-}
  edgeIFoldl :: (r -> EdgeId -> r) -> r -> g -> r
  edgeIFoldl f = edgeIVFoldl (\r p _ -> f r p)
  {-# INLINE edgeIFoldl #-}
  edgeIVFoldr :: (EdgeId -> e -> r -> r) -> r -> g -> r
  edgeFoldr :: (e' -> r -> r) -> r -> g -> r
  edgeFoldr f = edgeIVFoldr (\p e r -> f (wrap p e) r)
  {-# INLINE edgeFoldr #-}
  edgeIFoldr :: (EdgeId -> r -> r) -> r -> g -> r
  edgeIFoldr f = edgeIVFoldr (\p _ r -> f p r)
  {-# INLINE edgeIFoldr #-}
  adjMap :: (Cons (t r) (t r) r r, Monoid (t r)) => (n' -> e' -> r) -> NodeId -> g -> Maybe (t r)
  adjIMap :: (Cons (t r) (t r) r r, Monoid (t r)) => (NodeId -> EdgeId -> r) -> NodeId -> g -> Maybe (t r)
  adjNodeMap :: (Cons (t r) (t r) r r, Monoid (t r)) => (n' -> r) -> NodeId -> g -> Maybe (t r)
  adjNodeMap f = adjMap (\n' _ -> f n')
  {-# INLINE adjNodeMap #-}
  adjNodeIMap :: (Cons (t r) (t r) r r, Monoid (t r)) => (NodeId -> r) -> NodeId -> g -> Maybe (t r)
  adjNodeIMap f = adjIMap (\i _ -> f i)
  {-# INLINE adjNodeIMap #-}
  adjFoldl :: (r -> n' -> e' -> r) -> r -> NodeId -> g -> Maybe r
  adjIFoldl :: (r -> NodeId -> EdgeId -> r) -> r -> NodeId -> g -> Maybe r
  adjNodeFoldl :: (r -> n' -> r) -> r -> NodeId -> g -> Maybe r
  adjNodeFoldl f = adjFoldl (\r n' _ -> f r n')
  {-# INLINE adjNodeFoldl #-}
  adjNodeIFoldl :: (r -> NodeId -> r) -> r -> NodeId -> g -> Maybe r
  adjNodeIFoldl f = adjIFoldl (\r i _ -> f r i)
  {-# INLINE adjNodeIFoldl #-}
  adjFoldlM :: Monad m => (r -> n' -> e' -> m r) -> r -> NodeId -> g -> MaybeT m r
  adjIFoldlM :: Monad m => (r -> NodeId -> EdgeId -> m r) -> r -> NodeId -> g -> MaybeT m r
  adjForM_ :: Monad m => NodeId -> g -> (n' -> e' -> m r) -> MaybeT m ()
  adjIForM_ :: Monad m => NodeId -> g -> (NodeId -> EdgeId -> m r) -> MaybeT m ()
  degree :: NodeId -> g -> Maybe Int
  degree = adjNodeFoldl (\a _ -> a + 1) 0
  {-# INLINE degree #-}

class Builder g n e | g -> n e where
  -- n corresponds NodeId by the index of each n
  assoc :: (Graph g n n' e e' d, Foldable f1, Foldable f2, Wrap NodeId n n', Wrap EdgeId e e') => f1 n' -> f2 e' -> g
  build :: (Graph g n n' e e' d, Traversable t1, Traversable t2) => t1 NodeId -> (NodeId -> n) -> t2 EdgeId -> (NodeId -> NodeId -> e) -> g
