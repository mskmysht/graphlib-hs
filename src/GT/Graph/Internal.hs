{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TypeApplications #-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}


module GT.Graph.Internal
  ( Gr
  , DiGr
  , UndiGr
  , VGr
  , DiVGr
  , UndiVGr
  , NVGr
  , DiNVGr
  , UndiNVGr
  , EVGr
  , DiEVGr
  , UndiEVGr
  {- deprecated -}
  , BasicGr
  , MapGr
  , DiBasicGr
  , UndiBasicGr
  , DiMapGr
  , UndiMapGr
  )
where

import           GT.Graph.Class
-- import qualified Data.Vector.Mutable           as VM
-- import           Control.Monad.ST               ( ST )
-- import           Data.Pointed                   ( Pointed(..) )

import Data.HashMap.Strict as HM (HashMap, insert)
import Data.HashSet as HS (HashSet, insert, filter)
import Data.IntMap.Strict as IM (IntMap, insert)
import Data.Map.Strict as M (Map, insert)
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

{-
data SGr nc ec n e (d :: IsDirect) = SGr
   { nid :: !Int
   , nc :: !Int
   , ec :: !Int
   , ns :: !(nc n)
   , es :: !(ec e)
   }
-}

data G nc n n' ec e e' (d :: IsDirect) = G
   { nid :: !Int -- next node id
   , nc :: !Int
   , ec :: !Int
   , ns :: !(nc n)
   , es :: !(ec e)
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

_adjConbinator :: 
  f -> NodeId -> ns -> es
  -> (NodeId -> ns -> Bool) -> (NodeId -> n')
  -> (NodeId -> es -> es) -> ((EdgeId -> n') -> f -> es -> a) -> Maybe a
_adjConbinator f i ns es p gn esf conv = do
  guard $ p i ns -- (\j _ -> i == j)
  return $ conv (gn . opposite i) f (esf i es)

class SemiFilterable i f v where
  sfilter :: (i -> Bool) -> f v -> f v

class SemiIndexable i t v where
  sindex :: t v -> i -> v

class SemiTraversable i t v where
  -- sindex :: t v -> i -> v
  sfor :: Applicative f => t v -> (i -> v -> f r) -> f (t r)
  sforM :: Monad m => t v -> (i -> v -> m r) -> m (t r)

instance {-# OVERLAPS #-} (Eq a, Foldable f) => SemiFoldable a f a where
  selem = elem
  sfoldl f = foldl (\r p -> f r p p)
  convert f = foldr (\a s -> f a a <| s) mempty
  sfoldr f = foldr (\a r -> f a a r)
  sfoldlM f = foldlM (\r a -> f r a a)
  sforM_ ts f = forM_ ts (\a -> f a a)

instance (Eq i, FoldableWithIndex i (f i)) => SemiFoldable i (f i) a where
  selem i = iany (\i' _ -> i == i')
  sfoldl f = ifoldl (\p r e -> f r p e)
  convert f = ifoldr (\i a s -> f i a <| s) mempty
  sfoldr = ifoldr
  sfoldlM f = ifoldlM (\r i v -> f i r v)
  sforM_ = iforM_

instance {-# OVERLAPS #-} (Eq a, Filterable f) => SemiFilterable a f a where
  sfilter = W.filter

instance Eq a => SemiFilterable a Set a where
  sfilter = S.filter

instance Eq a => SemiFilterable a HashSet a where
  sfilter = HS.filter

instance (Eq i, FilterableWithIndex i (f i)) => SemiFilterable i (f i) a where
  sfilter f = ifilter (\p _ -> f p)

instance {-# OVERLAPS #-} SemiIndexable a t a where
  sindex _ = id

instance (Eq i, TraversableWithIndex i (t i)) => SemiIndexable i (t i) a where
  sindex f i = f ^?!itraversed.index i

instance {-# OVERLAPS #-} (Eq a, Traversable t) => SemiTraversable a t a where
  -- sindex _ = id
  sfor ts f = for ts (\a -> f a a)
  sforM ts f = forM ts (\a -> f a a)

instance (Eq i, TraversableWithIndex i (t i)) => SemiTraversable i (t i) a where
  -- sindex f i = f ^?!itraversed.index i
  sfor = ifor
  sforM = iforM

type SemiWitherable i t v = (SemiFoldable i t v, SemiFilterable i t v, SemiTraversable i t v)
type SemiFFI i t v = (SemiFoldable i t v, SemiFilterable i t v, SemiIndexable i t v)


-- instance Direction (Gr nc n n' ec e e' d) 'Directed where
--   isDirect g = True

-- instance Direction (Gr nc n n' ec e e' d) 'Undirected where
--   isDirect g = False


instance (Pairing d, Wrap NodeId n n', SemiFFI NodeId nc n, Wrap EdgeId e e', SemiFFI EdgeId ec e) => Graph (G nc n n' ec e e' d) n' e' d where
  nodeCount = nc
  edgeCount = ec

  removeEdge i j g = g { ec = length es', es = es' }
    where
      es' = sfilter (\p -> p == pairing @d Proxy i j) (es g)

  getNode i g = wrap i $ sindex (ns g) i
  getEdge p g = wrap p $ sindex (es g) p

  nodeMap f = convert @NodeId (\i n -> f $ wrap i n) . ns
  edgeMap f = convert @EdgeId (\p e -> f $ wrap p e) . es

  adjMap f i g = 
    _adjConbinator f i (ns g) (es g) selem (`getNode` g) (sfilter . isSource @d Proxy) (\mf f' -> convert (\p e -> f' (mf p) (wrap p e)))
  adjFoldl f r i g = 
    _adjConbinator f i (ns g) (es g) selem (`getNode` g) (sfilter . isSource @d Proxy) (\mf f' -> sfoldl (\a p e -> f' a (mf p) (wrap p e)) r)
  adjFoldlM f r i g = MaybeT $ sequenceA $ 
    _adjConbinator f i (ns g) (es g) selem (`getNode` g) (sfilter . isSource @d Proxy) (\mf f' -> sfoldlM (\a p e -> f' a (mf p) (wrap p e)) r)
  adjForM_ i g f = MaybeT $ sequenceA $ 
    _adjConbinator f i (ns g) (es g) selem (`getNode` g) (sfilter . isSource @d Proxy) (\mf f' -> (`sforM_` (\p e -> f' (mf p) (wrap p e))))


class (Foldable nc, Monoid (nc n)) => NodeBuilderBase nc n where
  _nassoc' :: (Foldable f, Wrap NodeId n n', SemiFoldable EdgeId ec e) => f n' -> ec e -> (Int, Int, nc n)

instance NodeBuilderBase Set NodeId where
  _nassoc' = _nassoc (\i _ s -> S.insert i s) S.insert

instance NodeBuilderBase HashSet NodeId where
  _nassoc' = _nassoc (\i _ s -> HS.insert i s) HS.insert

instance NodeBuilderBase (Map NodeId) n where
  _nassoc' = _nassoc M.insert (const id)
 
instance NodeBuilderBase (HashMap NodeId) n where
  _nassoc' = _nassoc HM.insert (const id)
 
instance NodeBuilderBase [] NodeId where
  _nassoc' = _cnassoc'

instance NodeBuilderBase Seq NodeId where
  _nassoc' = _cnassoc'

instance NodeBuilderBase V.Vector NodeId where
  _nassoc' = _cnassoc'

class (Monoid (ec e), SemiFoldable EdgeId ec e) => EdgeBuilderBase d ec e where
  _eassoc' :: (Foldable f, Wrap EdgeId e e') => Proxy d -> f e' -> (Int, ec e)

instance Pairing d => EdgeBuilderBase d Set EdgeId where
  _eassoc' prx = _eassoc @d prx (\p _ s -> S.insert p s)

instance Pairing d => EdgeBuilderBase d HashSet EdgeId where
  _eassoc' prx = _eassoc @d prx (\p _ s -> HS.insert p s)

instance Pairing d => EdgeBuilderBase d (Map EdgeId) e where
  _eassoc' prx = _eassoc @d prx M.insert

instance Pairing d => EdgeBuilderBase d (HashMap EdgeId) e where
  _eassoc' prx = _eassoc @d prx HM.insert


_cnassoc' :: (Foldable f, Foldable nc, Monoid (nc NodeId), Wrap NodeId NodeId n', SemiFoldable EdgeId ec e, Cons (nc NodeId) (nc NodeId) NodeId NodeId)
  => f n' -> ec e -> (Int, Int, nc NodeId)
_cnassoc' = _nassoc (\a _ s -> a <| s) (const id)

_nassoc :: (Foldable f, Foldable nc, Monoid (nc n), Wrap NodeId n n', SemiFoldable EdgeId ec e)
  => (NodeId -> n -> nc n -> nc n) -> (NodeId -> nc n  -> nc n) -> f n' -> ec e -> (Int, Int, nc n)
_nassoc f' f'' ns es = (h', length ns'', ns'') where
  (ns'', h') = sfoldr @EdgeId (\(i, j) _ (s, h) -> (f'' j $ f'' i s, max i $ max j h)) (ns', h) es
  (ns', h) = wfoldr @NodeId (\i n (s, h) -> (f' i n s, max i h)) (mempty, 0) ns

_eassoc :: (Pairing d, Foldable f, Monoid (ec e), SemiFoldable EdgeId ec e, Wrap EdgeId e e') => Proxy d -> (EdgeId -> e -> ec e -> ec e) -> f e' -> (Int, ec e)
_eassoc prx f es = (length es', es') where
  es' = wfoldr @EdgeId (\(i', j') e s -> if i' == j' then s else f (pairing prx i' j') e s) mempty es


instance (Pairing d, Wrap NodeId n n', Wrap EdgeId e e', NodeBuilderBase nc n, EdgeBuilderBase d ec e, SemiFoldable EdgeId ec e)
  => Builder (G nc n n' ec e e' d) n e where
  assoc ns es = G (h' + 1) nc ec ns'' es' where
    (ec, es') = _eassoc' @d Proxy es
    (h', nc, ns'') = _nassoc' ns es'
  build ns nf es ef = assoc (fmap (\i -> wrap i (nf i) :: n') ns) (fmap (\p@(i, j) -> wrap p (ef i j) :: e') es)


-- _nbuild :: (Foldable nc, Monoid (nc n), SemiFoldable EdgeId ec e) => (NodeId -> n -> nc n -> nc n) -> (NodeId -> n) -> ec e -> (Int, Int, nc n)
-- _nbuild f' nf es = (h', length ns'', ns'') where
--   (ns'', h') = sfoldr @EdgeId (\(i, j) _ (s, h) -> (f' j (nf j) (f' i (nf i) s), max i $ max j h)) (mempty, 0) es

-- _ebuild :: (Pairing d, SemiFoldable NodeId nc n, Monoid (ec e), SemiFoldable EdgeId ec e) => Proxy d -> nc n -> (EdgeId -> e -> ec e -> ec e) -> (Int, ec e)
-- _ebuild prx ns ef = (length es', es') where
--   es' = wfoldr @EdgeId (\(i', j') e s -> if i' == j' then s else f (pairing prx i' j') e s) mempty ns

{-
instance Builder MapGr d where
  assoc ns ps = SGr s s (S.size ehs) nmap es   where
    vcond s n = n >= 0 && n < s
    econd s (n, m) = vcond s n && vcond s m && n /= m
    ns' = ns
    s = length ns'
    (nmap, _) = foldl (\(im, i) n -> (IM.insert i n im, i + 1)) (IM.empty, 0) ns'
    (es, ehs) = foldr
      (\((i, j), e) (hm, hs) ->
        let q = pairing @d Proxy i j
        in  if econd s q then (H.insert q e hm, S.insert q hs) else (hm, hs)
      )
      (H.empty, S.empty)
      ps
  build nf s ps =
    assoc (fmap nf [0 .. s - 1]) (fmap (\(p@(i, j), ef) -> (p, ef i j)) ps)
-}

{-
instance BasicBuilder BasicGr d where
  assocB s es = SGr s s (S.size ehs) (S.fromList [0 .. s - 1]) ehs
    where
      vcond s n = n >= 0 && n < s
      econd s (n, m) = vcond s n && vcond s m && n /= m
      ehs = foldl
        (\hs (i, j) ->
          let q = pairing @d Proxy i j
          in if econd s q then S.insert q hs else hs
        )
        S.empty
        es
  assocB1 ns es = SGr s s (S.size ehs) nmap ehs
    where
      -- nmap will be distinct and ascend sorted by S.fromList
      nmap = let (vs, ws) = unzip es in S.fromList $ ns ++ vs ++ ws
      s = S.size nmap
      ehs = foldl
        (\hs (i, j) ->
          let q = pairing @d Proxy i j
          in if i /= j then S.insert q hs else hs
        )
        S.empty
        es
-}

{-
instance Pairing d => Graph (BasicGr d) NodeId (Pair NodeId) where
  nodes = nodeMap id
  edges = edgeMap id
  nodeMap f = toMonoid f . ns
  edgeMap f = toMonoid f . es
  nodeCount = nc
  edgeCount = ec
  -- adjNodeMap f i g    = _adjCont (\_ j -> j) (\_ p -> p) Proxy (\mf f' -> toMonoid $ \p -> f' (mf p)) (S.toList $ ns g) (S.toList $ es g) f i g
  -- adjNodeFold f r i g = _adjCont (\_ j -> j) (\_ p -> p) Proxy (\mf f' -> foldl (\a p -> f' a (mf p)) r) (S.toList $ ns g) (S.toList $ es g) f i g
    --  do
    -- guard $ S.member i $ ns g
    -- let es' = S.filter (\(i', _) -> i' == i) $ es g
    -- return $ foldl (\a (_, j) -> f a j) r es'
  getEdge = const
  removeEdge i j g =
    let es' = S.delete (i, j) $ es g
    in g { ec = S.size es', es = es' }
  -- findNodeIndex n g = Sq.elemIndexL n $ nodes g

-}

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