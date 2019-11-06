{-# LANGUAGE Strict #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}


module GT.Graph.Internal where

import GT.Graph.Class

import Data.HashMap.Strict as HM (HashMap, insert)
import Data.HashSet as HS (HashSet, insert, filter)
import Data.IntMap.Strict as IM (IntMap, insert)
import Data.Map.Strict as M (Map, insert)
import Data.Maybe (fromJust)
import Data.Sequence (Seq)
import Data.Set as S (Set, insert, filter)
import Data.Vector as V (Vector)

import Data.Foldable (length, foldlM)
import Data.Proxy (Proxy(..))
import Data.Traversable (sequenceA, for)
import Data.Witherable as W (FilterableWithIndex, ifilter, Filterable, filter)

import Control.Lens ((^?), (^?!), Cons, (<|), TraversableWithIndex, itraversed, index)
import Control.Lens.Indexed (FoldableWithIndex, iany, ifoldl, ifoldr, iforM_, ifoldlM, ifor, iforM)

import Control.Monad (guard, forM, forM_, foldM)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Trans.Class (lift)

import GHC.Exts (Constraint)

import Prelude hiding (filter)

data G nc n n' ec e e' (d :: Direction) = G
  { nid :: !Int -- next node id
  , nc :: !Int
  , ec :: !Int
  , ns :: !(nc n)
  , es :: !(ec e)
  , prx :: Proxy d
  }

type VGr n e = G (M.Map NodeId) n (NWith n) (HM.HashMap EdgeId) e (EWith e)
type DiVGr n e = VGr n e 'Directed
type UndiVGr n e = VGr n e 'Undirected

type NVGr n = G (M.Map NodeId) n (NWith n) S.Set EdgeId EdgeId
type DiNVGr n = NVGr n 'Directed
type UndiNVGr n = NVGr n 'Undirected

type EVGr e = G S.Set NodeId NodeId (HM.HashMap EdgeId) e (EWith e)
type DiEVGr e = EVGr e 'Directed
type UndiEVGr e = EVGr e 'Undirected

type Gr = G S.Set NodeId NodeId S.Set EdgeId EdgeId
type DiGr = Gr 'Directed
type UndiGr = Gr 'Undirected

{- backword compatibiilty -}
type MapGr n e = VGr n e
type DiMapGr n e = MapGr n e 'Directed
type UndiMapGr n e = MapGr n e 'Undirected
type BasicGr = Gr
type DiBasicGr = BasicGr 'Directed
type UndiBasicGr = BasicGr 'Undirected

instance {-# OVERLAPS #-} (Wrap EdgeId e e', SemiFoldable EdgeId ec e, Show e, Show e') => Show (G nc NodeId n' ec e e' d) where
  show g = show $ sfoldr @EdgeId (\p e a -> (wrap p e :: e') : a) [] $ es g

instance (Wrap EdgeId e e', SemiIndexable NodeId nc n, SemiFoldable EdgeId ec e, Show n, Show e, Show e') => Show (G nc n n' ec e e' d) where
  show g = show $ sfoldr @EdgeId (\p@(i, j) e a -> (wrap p e :: e', sindex (ns g) i, sindex (ns g) j) : a) [] $ es g

_adjCombinator :: f -> NodeId -> ns -> es -> (NodeId -> ns -> Bool) -> (NodeId -> n')
  -> (NodeId -> es -> es) -> ((EdgeId -> n') -> f -> es -> a) -> Maybe a
_adjCombinator f i ns es p gn esf conv = do
  guard $ p i ns
  return $ conv (gn . opposite' i) f (esf i es)

oppositeV' :: (Wrap NodeId n n', SemiIndexable NodeId nc n) => nc n -> NodeId -> EdgeId -> n'
oppositeV' ns i p = wrap j (sindex ns j) where
  j = opposite' i p
{-# INLINE oppositeV' #-}

filterWithSource :: (SemiFilterable EdgeId ec e, Directing d) => Proxy d -> ec e -> NodeId -> ec e
filterWithSource prx es i = sfilter (pisSource prx i) es
{-# INLINE filterWithSource #-}

_adjFCombinator :: (SemiFoldable NodeId nc n, SemiIndexable NodeId nc n, SemiFilterable EdgeId ec e, Directing d, Wrap EdgeId e e', Wrap NodeId n n')
  => Proxy d -> nc n -> ec e
  -> ((a -> EdgeId -> e -> b) -> q -> ec e -> r)
  -> (a -> n' -> e' -> b) -> q -> NodeId -> Maybe r
_adjFCombinator prx ns es g f r i = do
  guard $ selem i ns
  return $ g (\a p e -> f a (oppositeV' ns i p) (wrap p e)) r (filterWithSource prx es i)
{-# INLINE _adjFCombinator #-}

_adjMCombinator :: (SemiFoldable NodeId nc n, SemiIndexable NodeId nc n, SemiFilterable EdgeId ec e, Directing d, Wrap EdgeId e e', Wrap NodeId n n')
  => Proxy d -> nc n -> ec e
  -> ((EdgeId -> e -> b) -> ec e -> r)
  -> (n' -> e' -> b) -> NodeId -> Maybe r
_adjMCombinator prx ns es g f i = do
  guard $ selem i ns
  return $ g (\p e -> f (oppositeV' ns i p) (wrap p e)) (filterWithSource prx es i)
{-# INLINE _adjMCombinator #-}


class Foldable f => SemiFoldable i f v where
  selem :: i -> f v -> Bool
  sfoldl :: (r -> i -> v -> r) -> r -> f v -> r
  sfoldr :: (i -> v -> r -> r) -> r -> f v -> r
  sfoldlM :: Monad m => (r -> i -> v -> m r) -> r -> f v -> m r
  sforM_ :: Monad m => f v -> (i -> v -> m r) -> m ()
  convert :: (Monoid (t r), Cons (t r) (t r) r r) => (i -> v -> r) -> f v -> t r

class SemiFilterable i f v where
  sfilter :: (i -> Bool) -> f v -> f v

class SemiIndexable i t v where
  sindex :: t v -> i -> v

class SemiTraversable i t v where
  sfor :: Applicative f => t v -> (i -> v -> f r) -> f (t r)
  sforM :: Monad m => t v -> (i -> v -> m r) -> m (t r)


instance {-# OVERLAPS #-} (Eq a, Foldable f) => SemiFoldable a f a where
  selem = elem
  {-# INLINE selem #-}
  sfoldl f = foldl (\r p -> f r p p)
  {-# INLINE sfoldl #-}
  convert f = foldr (\a s -> f a a <| s) mempty
  {-# INLINE convert #-}
  sfoldr f = foldr (\a r -> f a a r)
  {-# INLINE sfoldr #-}
  sfoldlM f = foldlM (\r a -> f r a a)
  {-# INLINE sfoldlM #-}
  sforM_ ts f = forM_ ts (\a -> f a a)
  {-# INLINE sforM_ #-}


instance (Eq i, FoldableWithIndex i (f i)) => SemiFoldable i (f i) a where
  selem i = iany (\i' _ -> i == i')
  {-# INLINE selem #-}
  sfoldl f = ifoldl (\p r e -> f r p e)
  {-# INLINE sfoldl #-}
  convert f = ifoldr (\i a s -> f i a <| s) mempty
  {-# INLINE convert #-}
  sfoldr = ifoldr
  {-# INLINE sfoldr #-}
  sfoldlM f = ifoldlM (\r i v -> f i r v)
  {-# INLINE sfoldlM #-}
  sforM_ = iforM_
  {-# INLINE sforM_ #-}

instance {-# OVERLAPS #-} (Eq a, Filterable f) => SemiFilterable a f a where
  sfilter = W.filter
  {-# INLINE sfilter #-}

instance Eq a => SemiFilterable a Set a where
  sfilter = S.filter
  {-# INLINE sfilter #-}

instance Eq a => SemiFilterable a HashSet a where
  sfilter = HS.filter
  {-# INLINE sfilter #-}

instance (Eq i, FilterableWithIndex i (f i)) => SemiFilterable i (f i) a where
  sfilter f = ifilter (\p _ -> f p)
  {-# INLINE sfilter #-}

instance {-# OVERLAPS #-} SemiIndexable a t a where
  sindex _ = id
  {-# INLINE sindex #-}

instance (Eq i, TraversableWithIndex i (t i)) => SemiIndexable i (t i) a where
  sindex f i = f ^?!itraversed.index i
  {-# INLINE sindex #-}

instance {-# OVERLAPS #-} (Eq a, Traversable t) => SemiTraversable a t a where
  sfor ts f = for ts (\a -> f a a)
  {-# INLINE sfor #-}
  sforM ts f = forM ts (\a -> f a a)
  {-# INLINE sforM #-}

instance (Eq i, TraversableWithIndex i (t i)) => SemiTraversable i (t i) a where
  sfor = ifor
  {-# INLINE sfor #-}
  sforM = iforM
  {-# INLINE sforM #-}

type SemiWitherable i t v = (SemiFoldable i t v, SemiFilterable i t v, SemiTraversable i t v)
type SemiFFI i t v = (SemiFoldable i t v, SemiFilterable i t v, SemiIndexable i t v)


instance (Directing d, Wrap NodeId n n', Unwrap NodeId n', SemiFFI NodeId nc n, Wrap EdgeId e e', Unwrap EdgeId e', SemiFFI EdgeId ec e)
  => Graph (G nc n n' ec e e' d) n n' e e' d where
  pair = ppair . prx
  {-# INLINE pair #-}
  isSource = pisSource . prx
  {-# INLINE isSource #-}
  isSink = pisSink . prx
  {-# INLINE isSink #-}
  direction = pdirection . prx
  {-# INLINE direction #-}

  nodeCount = nc
  {-# INLINE nodeCount #-}
  edgeCount = ec
  {-# INLINE edgeCount #-}

  removeEdge i j g = g { ec = length es', es = es' }
    where
      es' = sfilter (\p -> p == pair g i j) (es g)
  {-# INLINE removeEdge #-}

  getNodeValue i g = sindex (ns g) i
  {-# INLINE getNodeValue #-}
  getEdgeValue p g = sindex (es g) p
  {-# INLINE getEdgeValue #-}

  nodeMap f = convert @NodeId (\i n -> f $ wrap i n) . ns
  {-# INLINE nodeMap #-}
  edgeMap f = convert @EdgeId (\p e -> f $ wrap p e) . es
  {-# INLINE edgeMap #-}

  adjMap f i g = _adjMCombinator (prx g) (ns g) (es g) convert f i
  {-# INLINE adjMap #-}
  adjFoldl f r i g = _adjFCombinator (prx g) (ns g) (es g) sfoldl f r i
  {-# INLINE adjFoldl #-}
  adjFoldlM f r i g = MaybeT $ sequenceA $ _adjFCombinator (prx g) (ns g) (es g) sfoldlM f r i
  {-# INLINE adjFoldlM #-}
  adjForM_ i g f = MaybeT $ sequenceA $ _adjMCombinator (prx g) (ns g) (es g) (flip sforM_) f i
  {-# INLINE adjForM_ #-}


class (Foldable nc, Monoid (nc n)) => NodeBuilderBase nc n where
  _nassoc' :: (Foldable f, Wrap NodeId n n', SemiFoldable EdgeId ec e) => f n' -> ec e -> (Int, Int, nc n)

instance NodeBuilderBase Set NodeId where
  _nassoc' = _nassoc (\i _ s -> S.insert i s) S.insert
  {-# INLINE _nassoc' #-}

instance NodeBuilderBase HashSet NodeId where
  _nassoc' = _nassoc (\i _ s -> HS.insert i s) HS.insert
  {-# INLINE _nassoc' #-}

instance NodeBuilderBase (Map NodeId) n where
  _nassoc' = _nassoc M.insert (const id)
  {-# INLINE _nassoc' #-}
 
instance NodeBuilderBase (HashMap NodeId) n where
  _nassoc' = _nassoc HM.insert (const id)
  {-# INLINE _nassoc' #-}
 
instance NodeBuilderBase [] NodeId where
  _nassoc' = _cnassoc'
  {-# INLINE _nassoc' #-}

instance NodeBuilderBase Seq NodeId where
  _nassoc' = _cnassoc'
  {-# INLINE _nassoc' #-}

instance NodeBuilderBase V.Vector NodeId where
  _nassoc' = _cnassoc'
  {-# INLINE _nassoc' #-}

class (Monoid (ec e), SemiFoldable EdgeId ec e) => EdgeBuilderBase d ec e where
  _eassoc' :: (Foldable f, Wrap EdgeId e e') => Proxy d -> f e' -> (Int, ec e)

instance Directing d => EdgeBuilderBase d Set EdgeId where
  _eassoc' prx = _eassoc @d prx (\p _ s -> S.insert p s)
  {-# INLINE _eassoc' #-}

instance Directing d => EdgeBuilderBase d HashSet EdgeId where
  _eassoc' prx = _eassoc @d prx (\p _ s -> HS.insert p s)
  {-# INLINE _eassoc' #-}

instance Directing d => EdgeBuilderBase d (Map EdgeId) e where
  _eassoc' prx = _eassoc @d prx M.insert
  {-# INLINE _eassoc' #-}

instance Directing d => EdgeBuilderBase d (HashMap EdgeId) e where
  _eassoc' prx = _eassoc @d prx HM.insert
  {-# INLINE _eassoc' #-}


_cnassoc' :: (Foldable f, Foldable nc, Monoid (nc NodeId), Wrap NodeId NodeId n', SemiFoldable EdgeId ec e, Cons (nc NodeId) (nc NodeId) NodeId NodeId)
  => f n' -> ec e -> (Int, Int, nc NodeId)
_cnassoc' = _nassoc (\a _ s -> a <| s) (const id)
{-# INLINE _cnassoc' #-}

_nassoc :: (Foldable f, Foldable nc, Monoid (nc n), Wrap NodeId n n', SemiFoldable EdgeId ec e)
  => (NodeId -> n -> nc n -> nc n) -> (NodeId -> nc n  -> nc n) -> f n' -> ec e -> (Int, Int, nc n)
_nassoc f' f'' ns es = (h', length ns'', ns'') where
  (ns'', h') = sfoldr @EdgeId (\(i, j) _ (s, h) -> (f'' j $ f'' i s, max i $ max j h)) (ns', h) es
  (ns', h) = wfoldr @NodeId (\i n (s, h) -> (f' i n s, max i h)) (mempty, 0) ns
{-# INLINE _nassoc #-}

_eassoc :: (Directing d, Foldable f, Monoid (ec e), SemiFoldable EdgeId ec e, Wrap EdgeId e e') => Proxy d -> (EdgeId -> e -> ec e -> ec e) -> f e' -> (Int, ec e)
_eassoc prx f es = (length es', es') where
  es' = wfoldr @EdgeId (\(i', j') e s -> if i' == j' then s else f (ppair prx i' j') e s) mempty es
{-# INLINE _eassoc #-}


instance (Directing d, Wrap NodeId n n', Wrap EdgeId e e', NodeBuilderBase nc n, EdgeBuilderBase d ec e, SemiFoldable EdgeId ec e)
  => Builder (G nc n n' ec e e' d) n e where
  assoc ns es = G (h' + 1) nc ec ns'' es' prx where
    prx = Proxy
    (ec, es') = _eassoc' @d prx es
    (h', nc, ns'') = _nassoc' ns es'
  {-# INLINE assoc #-}
  build ns nf es ef = assoc (fmap (\i -> wrap i (nf i) :: n') ns) (fmap (\p@(i, j) -> wrap p (ef i j) :: e') es)
  {-# INLINE build #-}

-- _nbuild :: (Foldable nc, Monoid (nc n), SemiFoldable EdgeId ec e) => (NodeId -> n -> nc n -> nc n) -> (NodeId -> n) -> ec e -> (Int, Int, nc n)
-- _nbuild f' nf es = (h', length ns'', ns'') where
--   (ns'', h') = sfoldr @EdgeId (\(i, j) _ (s, h) -> (f' j (nf j) (f' i (nf i) s), max i $ max j h)) (mempty, 0) es

-- _ebuild :: (Pairing d, SemiFoldable NodeId nc n, Monoid (ec e), SemiFoldable EdgeId ec e) => Proxy d -> nc n -> (EdgeId -> e -> ec e -> ec e) -> (Int, ec e)
-- _ebuild prx ns ef = (length es', es') where
--   es' = wfoldr @EdgeId (\(i', j') e s -> if i' == j' then s else f (pairing prx i' j') e s) mempty ns


{-
accum :: a -> ST s (V.MVector s a) -> ST s (V.MVector s a)
accum a svec = do
  vec <- svec
  let p = VM.length vec
  vec' <- VM.grow vec 1
  VM.write vec' p a
  return vec'

intmapToVector :: forall v a . (IM.Key -> v -> a) -> IM.IntMap v -> V.Vector a
intmapToVector f hm = V.create stv where
  stv :: forall s . ST s (V.MVector s a)
  stv = IM.foldlWithKey (\s k v -> accum (f k v) s) (VM.new 0) hm

hashmapToVector :: forall k v a . (k -> v -> a) -> H.HashMap k v -> V.Vector a
hashmapToVector f hm = V.create stv where
  stv :: forall s . ST s (V.MVector s a)
  stv = H.foldlWithKey' (\s k v -> accum (f k v) s) (VM.new 0) hm

toVector :: forall f v a . Foldable f => (v -> a) -> f v -> V.Vector a
toVector f hs = V.create stv where
  stv :: forall s . ST s (V.MVector s a)
  stv = foldl
    (\sv v -> do
      vec <- sv
      let p = VM.length vec
      vec' <- VM.grow vec 1
      VM.write vec' p $ f v
      return vec'
    )
    (VM.new 0)
    hs
-}