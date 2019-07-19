{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import GT.Graph.Class
import GT.Graph
import GT.Generator.RandomGraph
import GT.Parse
import GT.Algorithm.Propagation

import qualified Data.Map.Strict as M

import System.Exit
import Control.Monad (replicateM_)
import Criterion.Main
import qualified Data.Vector as V
import qualified Data.Sequence as Q
import qualified Data.Set as S
import Data.Witherable (Witherable, mapMaybe)
import Prelude hiding (mapMaybe)

sample :: IO ()
sample = do
  let es = [(3, 0), (1, 2), (2, 1), (3, 1)]
  let g1 = assocB 4 es :: DiBasicGr
  print g1
  print $ adjNodeCont 2 g1 id
  let g2 = assocB 4 es :: UndiBasicGr
  print g2
  print $ adjNodeCont 2 g2 id

-- repeatBA n m r = replicateM_ r $ do
--   g <- barabasiAlbert n m 1234 (,) assocB :: IO UndiBasicGr
--   return ()
--   -- print $ nodeSize g
  
-- benchmark :: IO ()
-- benchmark = defaultMain
--   [ bench "BA" $ whnfIO (repeatBA 1000 100 1) ]
-- -- main = repeatBA 1000 100 10

data VNode = VNode
  { nodeId :: Int
  , nodeValues :: M.Map String Double
  } deriving Show

data VEdge = VEdge
  { edgeId :: Int
  , source :: Int
  , target :: Int
  , edgeValues :: M.Map String Double
  } deriving Show

readML :: IO (UndiMapGr VNode VEdge)
readML = do
  let fp = "/Users/masaaki/Documents/PropagationSimulator/exp_data/PreventPropagation/BA_Spectral/BA000.graphml"
  f <- readFile fp
  let mg = parseGraphml (\n m -> compare (nodeId n) (nodeId m)) VNode (\i s t m -> (s, t, VEdge i s t m)) f
  case mg of
    Just g -> return g
    Nothing -> exitFailure

experiment :: (Witherable t, Graph g t (NWith n) (PairWith (NWith n) e), EdgeAccessor g t (NWith n) e) => g -> IO ()
experiment g = do
  let ns = nodes g
  let rad = (fromIntegral $ nodeSize g) / (sum $ mapMaybe (\(i, _) -> adjNodeFold i g (\d _ -> d + 1) 0) ns)
  let l = truncate $ 0.05 * fromIntegral (nodeSize g) :: Int
  let ias = [0 .. (l - 1)] -- [0, 1, 2]
  print (rad, l)
  -- print $ fmap (\i -> adjNodeCont i g id) ias
  (_, ts) <- propagateUntil (const $ rad * 2) g ( \c t (_, a) -> (S.size c, (S.size t):a) ) (
    \case
      (n, _) -> n == 0
      -- _ -> True
    ) (S.fromList ias) (0, []) 1234
  print ts

main :: IO ()
main = do
  -- g <- barabasiAlbert 100 5 1234 id (\i j -> (i, j, (i, j))) :: IO (UndiMapGr NodeId (Pair NodeId))
  g <- readML
  let es = do
        ((i, n), (j, m), e) <- Q.filter (\(_, _, e) -> M.member "MyBetweennessCentrality" $ edgeValues e) $ edges g
        return ((i, j), edgeValues e M.! "MyBetweennessCentrality")
  let es' = Q.sortBy (\(_, v) (_, w) -> compare w v) $ es
  let ses = fst <$> Q.take 20 es'
  let g' = foldl (\g (i, j) -> removeEdge i j g) g ses  
  print $ edgeSize g'