{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE DataKinds #-}

module GT.Graph.Class where

import Control.Monad.Trans.Maybe

type NodeId = Int
type NWith n = (NodeId, n)
type Pair n = (n, n)
type PairWith n e = (n, n, e)


data IsDirect = Directed | Undirected
newtype C (d :: IsDirect) = C { decouple :: Pair NodeId }

class Couple (d :: IsDirect) where
  couple :: NodeId -> NodeId -> C d
  isSource :: NodeId -> C d -> Bool
  isSink :: NodeId -> C d -> Bool

opposite :: NodeId -> C d -> NodeId
{-# INLINE opposite #-}
opposite i (C (i', j')) = if i == i' then j' else i'

instance Couple 'Directed where
  couple a b = C (a, b)
  {-# INLINE couple #-}
  isSource i (C (i', _)) = i == i'
  {-# INLINE isSource #-}
  isSink i (C (_, j')) = i == j'
  {-# INLINE isSink #-}

instance Couple 'Undirected where
  couple a b
    | a < b     = C (a, b)
    | otherwise = C (b, a)
  {-# INLINE couple #-}
  isSource i (C (i', j')) = i == i' || i == j'
  {-# INLINE isSource #-}
  isSink i (C (i', j')) = i == i' || i == j'
  {-# INLINE isSink #-}


class Unwrap i w => Wrap i v w where
  wrap :: i -> v -> w

class Unwrap i w where
  unwrap :: w -> i

instance Wrap NodeId n (NWith n) where
  wrap = (,)
  {-# INLINE wrap #-}

instance Unwrap NodeId (NWith n) where
  unwrap = fst
  {-# INLINE unwrap #-}

instance Wrap (Pair n') e (PairWith n' e) where
  wrap (n, m) e = (n, m, e)
  {-# INLINE wrap #-}

instance Unwrap (Pair n') (PairWith n' e) where
  unwrap (n, m, e) = (n, m)
  {-# INLINE unwrap #-}

instance Wrap a a a where
  wrap = const
  {-# INLINE wrap #-}

instance {-# OVERLAPS #-} (a ~ b) => Unwrap a b where
  unwrap = id
  {-# INLINE unwrap #-}

instance Unwrap i n' => Unwrap (Pair i) (Pair n') where
  unwrap (n, m) = (unwrap n, unwrap m)
  {-# INLINE unwrap #-}


class Direction g (d :: IsDirect) where
  isDirect :: g d -> Bool

instance Direction g 'Directed where
  isDirect g = True

instance Direction g 'Undirected where
  isDirect g = False


class Traversable t => Graph g t n' e' | g -> t n' e' where
  nodes :: g -> t n'
  edges :: g -> t e'
  nodeSize :: g -> Int
  edgeSize :: g -> Int
  nodeCont :: g -> (n' -> r) -> t r
  edgeCont :: g -> (e' -> r) -> t r
  adjNodeCont :: NodeId -> g -> (n' -> r) -> Maybe (t r)
  adjNodeFold :: NodeId -> g -> (r -> n' -> r) -> r -> Maybe r
  fromId :: NodeId -> g -> n'
  removeEdge :: NodeId -> NodeId -> g -> g
  degree :: NodeId -> g -> Maybe Int
  degree i g = adjNodeFold i g (\a _ -> a + 1) 0
  findNodeIndex :: Eq n' => n' -> g -> Maybe Int


pairCont :: (Traversable t, Graph g t n' (Pair NodeId)) => g -> (NodeId -> NodeId -> r) -> t r
pairCont g f = edgeCont g $ uncurry f


class (Traversable t, Graph g t n' (PairWith n' e)) => EdgeAccessor g t n' e | g -> n' e where
  toPairs :: Eq e => e -> g -> t (Pair NodeId)
  adjCont :: NodeId -> g -> (n' -> e -> r) -> Maybe (t r)
  adjContM_ :: Monad m => NodeId -> g -> (n' -> e -> m r) -> m (Maybe ())
  adjFold :: NodeId -> g -> (r -> n' -> e -> r) -> r -> Maybe r
  adjFoldM :: Monad m => NodeId -> g -> (r -> n' -> e -> m r) -> r -> MaybeT m r
  fromPair :: Pair NodeId -> g -> e


class Couple d => Builder g d where
  -- n corresponds NodeId by the index of each n
  assoc :: [n] -> [PairWith NodeId e] -> g n e d
  build :: (NodeId -> n) -> Int -> [PairWith NodeId (NodeId -> NodeId -> e)] -> g n e d


class Couple d => BasicBuilder g d where
  assocB :: Int -> [Pair NodeId] -> g d
  assocB1 :: [NodeId] -> [Pair NodeId] -> g d
