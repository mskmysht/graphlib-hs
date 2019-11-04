{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}

module GT.Parse 
  (parseGraphml)
where

import GT.Graph

import Text.XML.HXT.Parser.XmlParsec 
import Text.XML.HXT.DOM.TypeDefs 
import Text.XML.HXT.DOM.QualifiedName
import Text.Read
import Data.Either.Extra (maybeToEither)
import Data.List
import qualified Data.Map.Strict as M
import Data.Tree.NTree.TypeDefs 
import Data.Proxy (Proxy(..))

parseGraphml :: (Pairing d, Wrap NodeId n n', Wrap EdgeId e e', Graph g n' e' d, Builder g n e)
  => (Int -> M.Map String String -> n)
  -> (NodeId -> NodeId -> Maybe Int -> M.Map String String -> e)
  -> String -- the content of a file
  -> Either String g
parseGraphml nb eb cnt = do
    gmc <- findTagE "graphml" $ xreadDoc cnt
    gc <- findTagE "graph" $ getChildren gmc
    makeGraph nb eb gc

makeGraph :: forall d g n n' e e' . (Wrap NodeId n n', Wrap EdgeId e e', Pairing d, Graph g n' e' d, Builder g n e)
  => (Int -> M.Map String String -> n)
  -> (NodeId -> NodeId -> Maybe Int -> M.Map String String -> e)
  -> XmlTree
  -> Either String g
makeGraph nb eb gc = do
  let ns = filterTags "node" $ getChildren gc
  let es = filterTags "edge" $ getChildren gc
  ns' <- traverse (fromNodeTag @n' nb) ns
  es' <- traverse (fromEdgeTag @e' eb) es
  return $ assoc ns' es'

fromNodeTag :: forall n' n . Wrap NodeId n n' => (NodeId -> M.Map String String -> n) -> XmlTree -> Either String n'
fromNodeTag f (NTree n at) = maybeToEither "failed in converting node tags" $ do  
  i <- getAttr "id" readMaybe n
  m <- datasToMap $ filterTags "data" at
  return $ wrap i (f i m)

fromEdgeTag :: forall e' e . Wrap EdgeId e e' => (NodeId -> NodeId -> Maybe Int -> M.Map String String -> e) -> XmlTree -> Either String e'
fromEdgeTag f (NTree n at) = maybeToEither "failed in converting edge tags" $ do
  s <- getAttr "source" readMaybe n
  t <- getAttr "target" readMaybe n
  m <- datasToMap $ filterTags "data" at
  return $ wrap (s, t) (f s t (getAttr "id" readMaybe n) m)

getAttr :: String -> (String -> Maybe a) -> XNode -> Maybe a
getAttr name f (XTag _ attrs) = do
  [NTree (XText v) _] <- findAttr name attrs
  f v

getChildren :: XmlTree -> XmlTrees
getChildren (NTree _ cs) = cs

dataToKeyValue :: XmlTree -> Maybe (String, String)
dataToKeyValue (NTree (XTag _ attrs) cs) = do
  [NTree (XText key) _] <- findAttr "key" attrs
  [NTree (XText valueStr) _] <- Just cs
  -- value <- readMaybe valueStr
  return (key, valueStr)
dataToKeyValue _ = Nothing

datasToMap :: XmlTrees -> Maybe (M.Map String String)
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
findTag tn = find $ \(NTree xn _) ->
    case xn of
      XTag qn _ -> localPart qn == tn
      _         -> False

findTagE :: String -> XmlTrees -> Either String XmlTree
findTagE tn t = maybeToEither ("failed in finding tag: " ++ tn) $ findTag tn t

filterTags :: String -> XmlTrees -> XmlTrees
filterTags tn = filter $ \(NTree xn _) -> case xn of
    XTag qn _ -> localPart qn == tn
    _         -> False
