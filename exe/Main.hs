{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import GT.Graph
  ( Builder (assoc),
    UndiGr,
  )

main :: IO ()
main = do
  let g = assoc @UndiGr ([]) [(3, 0), (1, 2), (2, 1), (3, 1)]
  print g
