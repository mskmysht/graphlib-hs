{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MonoLocalBinds #-}

module GT.Graph
  ( BasicGr
  , MapGr
  , DiBasicGr
  , UndiBasicGr
  , DiMapGr
  , UndiMapGr
  )
where

import           GT.Graph.Class
import           Control.Monad.ST               ( ST )
import qualified Data.HashMap.Strict           as H
import qualified Data.IntMap.Strict            as IM
import qualified Data.Set                      as S
import qualified Data.Vector                   as V
import qualified Data.Vector.Mutable           as VM
import qualified Data.Sequence                 as Sq
import           Control.Monad                  ( guard
                                                , forM_
                                                , foldM
                                                )
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Class
import           GHC.Exts (Constraint)

data SGr nc ec n e (d :: IsDirect) = SGr
   { nid :: !Int
   , nc :: !Int
   , ec :: !Int
   , ns :: !(nc n)
   , es :: !(ec e)
   }

type BasicGr = SGr S.Set S.Set NodeId (Pair NodeId)
type MapGr = SGr IM.IntMap (H.HashMap (Pair NodeId))

type DiBasicGr = BasicGr 'Directed
type UndiBasicGr = BasicGr 'Undirected
type DiMapGr n e = MapGr n e 'Directed
type UndiMapGr n e = MapGr n e 'Undirected

instance Show (BasicGr d) where
  show g = show (es g)

instance (Show n, Show e) => Show (MapGr n e d) where
  show g = show
    (H.foldrWithKey (\(i, j) e a -> (ns g IM.! i, ns g IM.! j, e) : a) [] $ es g
    )


instance (Wrap NodeId n (NWith n), Couple d) => Graph (MapGr n e d) Sq.Seq (NWith n) (PairWith (NWith n) e) where
  nodes g = intmapToSeq wrap $ ns g
  edges g = hashmapToSeq (\(i, j) e -> (fromId i g, fromId j g, e)) $ es g
  nodeSize = nc
  edgeSize = ec
  nodeCont g f = intmapToSeq (\i n -> f $ wrap i n) $ ns g
  edgeCont g f =
    hashmapToSeq (\(i, j) e -> f (fromId i g, fromId j g, e)) $ es g
  adjNodeCont i g f = do
    guard $ IM.member i $ ns g
    let es' = H.filterWithKey (\i' _ -> fst i' == i) $ es g
    return $ hashmapToSeq (\(_, j) _ -> f $ fromId j g) es'
  adjNodeFold i g f r = do
    guard $ IM.member i $ ns g
    let es' = H.filterWithKey (\i' _ -> fst i' == i) $ es g
    return $ H.foldlWithKey' (\a (_, j) _ -> f a $ fromId j g) r es'
  fromId i g = wrap i $ ns g IM.! i
  removeEdge i j g =
    let es' = H.delete (i, j) $ es g
    in g { ec = H.size es', es = es' }
  findNodeIndex n g = Sq.elemIndexL n $ nodes g


instance (Wrap NodeId n (NWith n), Couple d) => EdgeAccessor (MapGr n e d) Sq.Seq (NWith n) e where
  toPairs e g = hashmapToSeq const $ H.filter (== e) $ es g
  adjCont i g f = do
    guard $ IM.member i $ ns g
    let es' = H.filterWithKey (\p _ -> isSource i $ C @d p) $ es g
    return $ hashmapToSeq (\p e -> f (fromId (opposite i $ C p) g) e) es'
  adjContM_ i g f =
    if IM.member i $ ns g then
      let es' = H.filterWithKey (\p _ -> isSource i $ C @d p) $ es g
      in Just <$> forM_ (H.toList es') (\(p, e) -> f (fromId (opposite i (C p)) g) e)
    else
      return Nothing
  adjFold i g f r = do
    guard $ IM.member i $ ns g
    let es' = H.filterWithKey (\p _ -> isSource i $ C @d p) $ es g
    return $ H.foldlWithKey' (\a p e -> f a (fromId (opposite i $ C @d p) g) e) r es'
  adjFoldM i g f r =
    if IM.member i $ ns g then
      let es' = H.filterWithKey (\p _ -> isSource i $ C @d p) $ es g
      in lift $ foldM (\a (p, e) -> f a (fromId (opposite i $ C @d p) g) e) r $ H.toList es'
    else
      MaybeT $ return Nothing
  fromPair p g = es g H.! p


instance Couple d => Graph (BasicGr d) Sq.Seq NodeId (Pair NodeId) where
  nodes g = nodeCont g id
  edges g = edgeCont g id
  nodeCont g f = toSeq f $ ns g
  edgeCont g f = toSeq f $ es g
  nodeSize = nc
  edgeSize = ec
  adjNodeCont i g f = do
    guard $ S.member i $ ns g
    let es' = S.filter (\(i', _) -> i' == i) $ es g
    return $ toSeq (\(_, j) -> f j) es'
  adjNodeFold i g f r = do
    guard $ S.member i $ ns g
    let es' = S.filter (\(i', _) -> i' == i) $ es g
    return $ foldl (\a (_, j) -> f a j) r es'
  fromId = const
  removeEdge i j g =
    let es' = S.delete (i, j) $ es g
    in g { ec = S.size es', es = es' }
  findNodeIndex n g = Sq.elemIndexL n $ nodes g



intmapToSeq :: (IM.Key -> v -> a) -> IM.IntMap v -> Sq.Seq a
intmapToSeq f = IM.foldlWithKey (\s k v -> s Sq.|> f k v) Sq.empty

hashmapToSeq :: (k -> v -> a) -> H.HashMap k v -> Sq.Seq a
hashmapToSeq f = H.foldlWithKey' (\s k v -> s Sq.|> f k v) Sq.empty

toSeq :: Foldable f => (v -> a) -> f v -> Sq.Seq a
toSeq f = foldl (\s v -> s Sq.|> f v) Sq.empty

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

accum :: a -> ST s (V.MVector s a) -> ST s (V.MVector s a)
accum a svec = do
  vec <- svec
  let p = VM.length vec
  vec' <- VM.grow vec 1
  VM.write vec' p a
  return vec'


instance Couple d => BasicBuilder BasicGr d where
  assocB s es = SGr s s (S.size ehs) (S.fromList [0 .. s - 1]) ehs   where
    vcond s n = n >= 0 && n < s
    econd s (n, m) = vcond s n && vcond s m && n /= m
    ehs = foldl
      (\hs (n, m) ->
        let q = decouple @d (couple n m)
        in if econd s q then S.insert q hs else hs
      )
      S.empty
      es


instance Couple d => Builder MapGr d where
  assoc ns ps = SGr s s (S.size ehs) ns' es   where
    vcond s n = n >= 0 && n < s
    econd s (n, m) = vcond s n && vcond s m && n /= m
    s = length ns
    (ns', _) = foldl (\(im, i) n -> (IM.insert i n im, i + 1)) (IM.empty, 0) ns
    (es, ehs) = foldr
      (\(n, m, e) (hm, hs) ->
        let q = decouple @d (couple n m)
        in  if econd s q then (H.insert q e hm, S.insert q hs) else (hm, hs)
      )
      (H.empty, S.empty)
      ps
  build nf s ps =
    assoc (fmap nf [0 .. s - 1]) (fmap (\(i, j, ef) -> (i, j, ef i j)) ps)

