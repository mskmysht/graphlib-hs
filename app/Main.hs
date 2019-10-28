{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

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
import Data.Foldable (length)
import Data.Witherable (Witherable, mapMaybe)
import Prelude hiding (mapMaybe)

import GHC.Exts (fromList)

import System.Environment (getArgs)

sample :: IO ()
sample = do
  let es = [(3, 0), (1, 2), (2, 1), (3, 1)]
  let g1 = assocB 4 es :: DiBasicGr
  print g1
  print $ adjNodeCont 2 g1 id
  let g2 = assocB 4 es :: UndiBasicGr
  print g2
  print $ adjNodeCont 2 g2 id

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

readML :: FilePath -> IO (UndiMapGr VNode VEdge)
readML fp = do
  f <- readFile fp
  let mg = parseGraphml (\n m -> compare (nodeId n) (nodeId m)) VNode (\i p@(s, t) m -> (p, VEdge i s t m)) f
  case mg of
    Just g -> return g
    Nothing -> exitFailure

propexp :: (Witherable t, Graph g t (NWith n) (EWith e), EdgeAccessor g t (NWith n) (EWith e)) => Int -> g -> IO ()
propexp seed g = do
  let ns = nodes g
  let rad = fromIntegral (nodeSize g) / sum (mapMaybe (\(i, _) -> adjNodeFold i g (\d _ -> d + 1) 0) ns)
  let l = truncate $ 0.05 * fromIntegral (nodeSize g) :: Int
  let ias = [0 .. (l - 1)] -- [0, 1, 2]
  print (rad, l)
  let (ss, ts) = propagateUntil (const $ rad * 2) g ( \c t (_, a) -> (length c, length t : a) ) (
        \case
          (n, _) -> n == 0
        ) (fromList ias) (0, []) seed
  print ss
  print (ts :: (S.Set NodeId, S.Set NodeId))

readexp :: UndiMapGr VNode VEdge -> String -> IO ()
readexp g property = do
  let es = do
        ((i, j), e) <- Q.filter (\(_, e) -> M.member property $ edgeValues e) $ edges g
        return ((i, j), edgeValues e M.! property)
  let es' = Q.sortBy (\(_, v) (_, w) -> compare w v) es
  let ses = fst <$> Q.take 20 es'
  let g' = foldl (\g (i, j) -> removeEdge i j g) g ses  
  print $ edgeSize g'

main :: IO ()
main = do
  ssd:fp:_ <- getArgs
  g <- readML fp
  propexp (read ssd) g
