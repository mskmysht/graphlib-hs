{-# LANGUAGE TypeApplications #-}

module GT.GraphSpec where

import GT.Graph
import Test.Hspec

spec :: Spec
spec = do
  describe "GT.Graph" $ do
    describe "Builder" $ do
      describe "assoc" $ do
        it "make a graph with a node list and an edge list" $ do
          let g = assoc @UndiGr [] [(0, 1), (1, 2), (2, 3), (3, 0)]
          nodes g `shouldBe` [0, 1, 2, 3]
          edges g `shouldBe` [(0, 1), (0, 3), (1, 2), (2, 3)]
    describe "Graph" $ do
      let ug = assoc @UndiGr [] [(0, 1), (1, 2), (2, 3), (3, 0)]
      let dg = assoc @DiGr [] [(0, 1), (1, 2), (2, 3), (3, 0)]
      describe "direction" $ do
        it "returns direction of a graph" $ do
          show (direction ug) `shouldBe` show Undirected
          show (direction dg) `shouldBe` show Directed
      describe "isSource" $ do
        context "when g is undirected" $ do
          it "determines whether a node is one of endpoints of an edge" $ do
            (0, 1) `shouldSatisfy` isSource ug 0
            (0, 1) `shouldSatisfy` isSource ug 1
            (0, 1) `shouldNotSatisfy` isSource ug 2
        context "when g is directed" $ do
          it "determines whether a node is the source of an edge" $ do
            (0, 1) `shouldSatisfy` isSource dg 0
            (0, 1) `shouldNotSatisfy` isSource dg 1
            (0, 1) `shouldNotSatisfy` isSource dg 2
      describe "isSink" $ do
        context "when g is undirected" $ do
          it "determines whether a node is one of endpoints of an edge" $ do
            (0, 1) `shouldSatisfy` isSink ug 0
            (0, 1) `shouldSatisfy` isSink ug 1
            (0, 1) `shouldNotSatisfy` isSink ug 2
        context "when g is directed" $ do
          it "determines whether a node is the sink of an edge" $ do
            (0, 1) `shouldNotSatisfy` isSink dg 0
            (0, 1) `shouldSatisfy` isSink dg 1
            (0, 1) `shouldNotSatisfy` isSink dg 2
