{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module GT.ParseSpec
  ( spec,
  )
where

import Control.Monad.Except (ExceptT (..), runExceptT)
import qualified Data.Map.Strict as M
import Data.Witherable (Filterable, catMaybes, mapMaybe)
import GT.Graph
import GT.Parse
import Test.Hspec
import Text.Read (readMaybe)

data VNode = VNode
  { nodeId :: Int,
    nodeValues :: M.Map String Double
  }
  deriving (Show)

data VEdge = VEdge
  { edgeId :: Maybe Int,
    source :: Int,
    target :: Int,
    edgeValues :: M.Map String Double
  }
  deriving (Show)

spec :: Spec
spec = do
  describe "parse graphML file: test/sample.xml" $ do
    it "works" $ do
      f <- readFile "test/sample.xml"
      let eg :: Either String (DiVGr (M.Map String String) (M.Map String String)) =
            parseGraphml (\i m -> m) (\s t i m -> m) f
      nodes <$> eg `shouldBe` (Right [(0, M.singleton "hoge" "0"), (1, M.empty), (2, M.empty), (3, M.empty)])
      edges <$> eg `shouldBe` (Right [((0, 1), M.empty), ((1, 2), M.empty), ((2, 3), M.empty), ((3, 1), M.empty)])
