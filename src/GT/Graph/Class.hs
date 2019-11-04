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

-- deprecated
-- type PairWith n' e = (n', n', e)

data IsDirect = Directed | Undirected

class Pairing (d :: IsDirect) where
  pairing :: Proxy d -> NodeId -> NodeId -> EdgeId
  isSource :: Proxy d -> NodeId -> EdgeId -> Bool
  isSink :: Proxy d -> NodeId -> EdgeId -> Bool

instance Pairing 'Directed where
  pairing _ source sink = (source, sink)
  {-# INLINE pairing #-}
  isSource _ i (source, _) = i == source
  {-# INLINE isSource #-}
  isSink _ i (_, sink) = i == sink
  {-# INLINE isSink #-}

instance Pairing 'Undirected where
  pairing _ i j
    | i < j     = (i, j)
    | otherwise = (j, i)
  {-# INLINE pairing #-}
  isSource _ i (i', j') = i == i' || i == j'
  {-# INLINE isSource #-}
  isSink _ i (i', j') = i == i' || i == j'
  {-# INLINE isSink #-}

-- pattern (@@) :: n -> n -> Pair n
-- pattern (@@) n m <- (n, m)
-- infix @@ 5
-- pattern (:.-) :: n -> n -> Pair n
-- pattern (:.-) i j = (i, j)

-- need to change a return type to Maybe NodeId
opposite :: NodeId -> EdgeId -> NodeId
{-# INLINE opposite #-}
opposite i (i', j') = if i == i' then j' else i'

-- newtype C (d :: IsDirect) = C { decouple :: Pair NodeId }

-- class Couple (d :: IsDirect) where
--   couple :: NodeId -> NodeId -> C d
--   isSource :: NodeId -> C d -> Bool
--   isSink :: NodeId -> C d -> Bool

-- opposite :: NodeId -> C d -> NodeId
-- {-# INLINE opposite #-}
-- opposite i (C (i', j')) = if i == i' then j' else i'

-- instance Couple 'Directed where
--   couple a b = C (a, b)
--   {-# INLINE couple #-}
--   isSource i (C (i', _)) = i == i'
--   {-# INLINE isSource #-}
--   isSink i (C (_, j')) = i == j'
--   {-# INLINE isSink #-}

-- instance Couple 'Undirected where
--   couple a b
--     | a < b     = C (a, b)
--     | otherwise = C (b, a)
--   {-# INLINE couple #-}
--   isSource i (C (i', j')) = i == i' || i == j'
--   {-# INLINE isSource #-}
--   isSink i (C (i', j')) = i == i' || i == j'
--   {-# INLINE isSink #-}


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

-- deprecated
-- instance Wrap (Pair n') e (PairWith n' e) where
--   wrap (n, m) e = (n, m, e)
--   {-# INLINE wrap #-}

-- deprecated
-- instance Unwrap (Pair n') (PairWith n' e) where
--   unwrap (n, m, e) = (n, m)
--   {-# INLINE unwrap #-}

-- instance Unwrap i n' => Unwrap (Pair i) (Pair n') where
--   unwrap (n, m) = (unwrap n, unwrap m)
--   {-# INLINE unwrap #-}


class Direction g (d :: IsDirect) where
  isDirect :: g d -> Bool


class Graph g n' e' d | g -> n' e' d where
  nodeCount :: g -> Int
  edgeCount :: g -> Int
  removeEdge :: NodeId -> NodeId -> g -> g
  getNode :: NodeId -> g -> n'
  getEdge :: EdgeId -> g -> e'
  nodes :: (Cons (t n') (t n') n' n', Monoid (t n')) => g -> t n'
  nodes = nodeMap id
  edges :: (Cons (t e') (t e') e' e', Monoid (t e')) => g -> t e'
  edges = edgeMap id
  nodeMap    :: (Cons (t r) (t r) r r, Monoid (t r)) => (n' -> r) -> g -> t r
  edgeMap    :: (Cons (t r) (t r) r r, Monoid (t r)) => (e' -> r) -> g -> t r
  adjMap     :: (Cons (t r) (t r) r r, Monoid (t r)) => (n' -> e' -> r) -> NodeId -> g -> Maybe (t r)
  adjNodeMap :: (Cons (t r) (t r) r r, Monoid (t r)) => (n' -> r) -> NodeId -> g -> Maybe (t r)
  adjNodeMap f = adjMap (\n' e' -> f n')
  adjFoldl     :: (r -> n' -> e' -> r) -> r -> NodeId -> g -> Maybe r
  adjNodeFoldl :: (r -> n' -> r) -> r -> NodeId -> g -> Maybe r
  adjNodeFoldl f = adjFoldl (\r n' _ -> f r n')
  adjFoldlM    :: Monad m => (r -> n' -> e' -> m r) -> r -> NodeId -> g -> MaybeT m r
  adjForM_   :: Monad m => NodeId -> g -> (n' -> e' -> m r) -> MaybeT m ()
  degree :: NodeId -> g -> Maybe Int
  degree = adjNodeFoldl (\a _ -> a + 1) 0



class Foldable f => SemiFoldable i f v where
  selem :: i -> f v -> Bool
  sfoldl :: (r -> i -> v -> r) -> r -> f v -> r
  sfoldr :: (i -> v -> r -> r) -> r -> f v -> r
  sfoldlM :: Monad m => (r -> i -> v -> m r) -> r -> f v -> m r
  sforM_ :: Monad m => f v -> (i -> v -> m r) -> m ()
  convert :: (Monoid (t r), Cons (t r) (t r) r r) => (i -> v -> r) -> f v -> t r


class Builder g n e | g -> n e where
  -- n corresponds NodeId by the index of each n
  assoc :: (Foldable f1, Foldable f2, Wrap NodeId n n', Wrap EdgeId e e') => f1 n' -> f2 e' -> g
  build :: (Traversable t1, Traversable t2) => t1 NodeId -> (NodeId -> n) -> t2 EdgeId -> (NodeId -> NodeId -> e) -> g
