{-# LANGUAGE Strict #-}
{-# LANGUAGE BangPatterns #-}

module GT.Generator.RandomGraph 
  (barabasiAlbert
  )
where

import GT.Graph.Class
import System.Random.SFMT
import Data.Set as St
import Data.Sequence as S
import Data.Foldable as F (toList)
import Data.Maybe (fromJust)


barabasiAlbert :: Builder g d =>
  Int -- the number of node
   -> Int -- the number of each degree
   -> Int -- seed value
   -> (NodeId -> n) -- node builder
   -> (NodeId -> NodeId -> PairWith NodeId e) -- edge builder
   -> IO (g n e d)
barabasiAlbert n m seed nb eb
  | m < 1 || m >= n = error $ "Barabási–Albert network must have m >= 1 and m < n, m = " ++ show m ++ ", n = " ++ show n
  | otherwise = initializeFromSeed seed >>= ba n m where
    ba n m gen = do 
      ses <- g m (S.fromList [0 .. m - 1]) S.empty S.empty
      return $ assoc (fmap nb [0 .. n - 1]) (F.toList $ fmap (uncurry eb) ses)
     where
      g :: Int -> Seq Int -> Seq (Int, Int) -> Seq Int -> IO (Seq (Pair NodeId))
      g s ts es rns
        | s == n = return es
        | otherwise = let
            ss = S.replicate m s
            !rns' = rns >< ss >< ts
          in rc rns' m >>= g (s + 1) rns' (es >< S.zip ss ts)
      rc :: Seq Int -> Int -> IO (Seq Int)
      rc seq n = loop St.empty 0 where
        l = S.length seq
        loop :: Set Int -> Int -> IO (Seq Int)
        loop t s
          | s == m = return $ S.fromList $ St.toList t
          | otherwise = do
              i <- uniformR (0, l - 1) gen
              let !x = fromJust $ seq S.!? i
              loop (St.insert x t) (if member x t then s + 1 else s)
             