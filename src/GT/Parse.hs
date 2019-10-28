{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module GT.Parse 
  (parseGraphml)
where

import GT.Graph

import Data.Tree.NTree.TypeDefs 
import Text.XML.HXT.Parser.XmlParsec 
import Text.XML.HXT.DOM.TypeDefs 
import Text.XML.HXT.DOM.QualifiedName
import Data.List
import Text.Read
import qualified Data.Map.Strict as M


parseGraphml :: Builder g d =>
  (n -> n -> Ordering)
   -> (Int -> M.Map String Double -> n)
   -> (Int -> EdgeId -> M.Map String Double -> EWith e)
   -> String -- the content of a file
   -> Maybe (g n e d)
parseGraphml cmp nb eb cnt = do
  let mg = do
        gmc <- findTag "graphml" $ xreadDoc cnt
        gc <- findTag "graph" $ getChildren gmc
        makeGraph cmp nb eb gc
  case mg of
    Just g -> return g
    Nothing -> fail "parse error"

makeGraph :: forall g d n e . Builder g d =>
  (n -> n -> Ordering)
   -> (Int -> M.Map String Double -> n)
   -> (Int -> EdgeId -> M.Map String Double -> EWith e)
   -> XmlTree
   -> Maybe (g n e d)
makeGraph cmp nb eb gc = do
  let ns = filterTags "node" $ getChildren gc
  let es = filterTags "edge" $ getChildren gc
  vns <- traverse (fromNodeTag nb) ns
  ves <- traverse (fromEdgeTag (\i j -> decouple @d $ couple i j) eb) es
  return $ assoc (sortBy cmp vns) ves

fromNodeTag :: (Int -> M.Map String Double -> a) -> XmlTree -> Maybe a
fromNodeTag f (NTree n at) = do  
  i <- getAttr "id" readMaybe n
  m <- datasToMap $ filterTags "data" at
  return $ f i m

-- fromEdgeTag :: (Int -> Int -> Int -> M.Map String Double -> a) -> XmlTree -> Maybe a
fromEdgeTag :: (Int -> Int -> EdgeId) -> (Int -> EdgeId -> M.Map String Double -> a) -> XmlTree -> Maybe a
fromEdgeTag c f (NTree n at) = do
  i <- getAttr "id" readMaybe n
  s <- getAttr "source" readMaybe n
  t <- getAttr "target" readMaybe n
  m <- datasToMap $ filterTags "data" at
  return $ f i (c s t) m

getAttr :: String -> (String -> Maybe a) -> XNode -> Maybe a
getAttr name f (XTag _ attrs) = do
  [NTree (XText v) _] <- findAttr name attrs
  f v

getChildren :: XmlTree -> XmlTrees
getChildren (NTree _ cs) = cs

dataToKeyValue :: XmlTree -> Maybe (String, Double)
dataToKeyValue (NTree (XTag _ attrs) cs) = do
  [NTree (XText key) _] <- findAttr "key" attrs
  [NTree (XText valueStr) _] <- Just cs
  value <- readMaybe valueStr
  return (key, value)
dataToKeyValue _ = Nothing

datasToMap :: XmlTrees -> Maybe (M.Map String Double)
datasToMap xts = M.fromList <$> traverse dataToKeyValue xts

findAttr :: String -> XmlTrees -> Maybe XmlTrees
findAttr tn xts = do
  NTree (XAttr qn) xts' <- find (\(NTree xn _) ->
    case xn of
      XAttr qn -> localPart qn == tn
      _        -> False
    ) xts 
  return xts'

-- return attributes and children
findTag :: String -> XmlTrees -> Maybe XmlTree
findTag tn = find (\(NTree xn _) ->
    case xn of
      XTag qn _ -> localPart qn == tn
      _         -> False
  )

filterTags :: String -> XmlTrees -> XmlTrees
filterTags tn = filter (\(NTree xn _) -> case xn of
    XTag qn _ -> localPart qn == tn
    _         -> False
  )
