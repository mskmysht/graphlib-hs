{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

module Main where

import GT.Graph
import GT.Generator.RandomGraph
import GT.Parse
import GT.Algorithm.Propagation

import qualified Data.Map.Strict as M

import System.Exit
import Control.Lens (Cons)
import Control.Monad (replicateM_)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Monad.Trans (lift)
import Criterion.Main
import qualified Data.Vector as V
import qualified Data.Sequence as Q
import qualified Data.Set as S
import Data.Foldable (length)
import Data.Pointed (Pointed)
import Data.Proxy (Proxy(..))
import Data.Witherable (Filterable, mapMaybe, catMaybes)
import Prelude hiding (mapMaybe)
import Text.Read (readMaybe)

import GHC.Exts (fromList)

import System.Environment (getArgs)

sample :: IO ()
sample = do
  let es = [(3, 0), (1, 2), (2, 1), (3, 1)] :: [EdgeId]
  let g1 = assoc ([] :: [NodeId]) es :: DiBasicGr
  print g1
  print (adjNodeMap id 2 g1 :: Maybe [NodeId])
  let g2 = assoc ([] :: [NodeId]) es :: UndiBasicGr
  print g2
  print (adjNodeMap id 2 g2 :: Maybe [NodeId])

data VNode = VNode
  { nodeId :: Int
  , nodeValues :: M.Map String Double
  } deriving Show

data VEdge = VEdge
  { edgeId :: Maybe Int
  , source :: Int
  , target :: Int
  , edgeValues :: M.Map String Double
  } deriving Show

readML :: Directing d => FilePath -> ExceptT String IO (VGr VNode VEdge d)
readML fp = ExceptT $ do
  f <- readFile fp
  return $ parseGraphml (\i m -> VNode i (mapMaybe readMaybe m)) (\s t i m -> VEdge i s t (mapMaybe readMaybe m)) f

propexp :: (Graph g n n' e (EWith e) d) => Int -> g -> IO ()
propexp seed g = do
  let sd = catMaybes @Q.Seq @Double (nodeMap (\n' -> let i = unwrap n' in adjNodeFoldl (\d _ -> d + 1) 0 i g) g)
  let rad = fromIntegral (nodeCount g) / sum sd
  let l = truncate $ 0.05 * fromIntegral (nodeCount g) :: Int
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
  print $ edgeCount g'

main :: IO ()
main = do
  ssd:fp:_ <- getArgs
  eg <- runExceptT $ readML @'Directed fp
  case eg of
    Left msg -> print msg
    Right g -> do
      print (nodeMap fst g :: [NodeId])
      print (edgeMap fst g :: [EdgeId])
      -- propexp (read ssd) g
      print $ direction g
      print $ isSource g 0 (0, 1)
      print $ isSink g 0 (0, 1)
