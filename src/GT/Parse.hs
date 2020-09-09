{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GT.Parse
  ( parseGraphml,
    readMatrixMarket,
  )
where

import Control.Monad.Except (ExceptT (..), mapExceptT, runExceptT)
import Data.Either.Extra (maybeToEither)
import Data.List (find)
import qualified Data.Map.Strict as M
import Data.Proxy (Proxy (..))
import Data.Tree.NTree.TypeDefs (NTree (NTree))
import GT.Graph
  ( Builder (assoc),
    Graph,
    NodeId,
    UndiGr,
    Wrap (wrap),
  )
import System.IO (IOMode (..), hGetLine, hIsEOF, hPutStrLn, withFile)
import Text.Read (readEither, readMaybe)
import Text.XML.HXT.DOM.QualifiedName (localPart)
import Text.XML.HXT.DOM.TypeDefs
  ( XNode (XAttr, XTag, XText),
    XmlTree,
    XmlTrees,
    localPart,
  )
import Text.XML.HXT.Parser.XmlParsec (xreadDoc)

parseGraphml ::
  (Graph g n n' e e' d, Builder g n e) =>
  (NodeId -> M.Map String String -> n) ->
  (NodeId -> NodeId -> Maybe Int -> M.Map String String -> e) ->
  String -> -- the content of a file
  Either String g
parseGraphml nb eb cnt = do
  gmc <- findTagE "graphml" $ xreadDoc cnt
  gc <- findTagE "graph" $ getChildren gmc
  makeGraph nb eb gc

makeGraph ::
  forall g n n' e e' d.
  (Graph g n n' e e' d, Builder g n e) =>
  (NodeId -> M.Map String String -> n) ->
  (NodeId -> NodeId -> Maybe Int -> M.Map String String -> e) ->
  XmlTree ->
  Either String g
makeGraph nb eb gc =
  assoc
    <$> traverse (fmap (\(i, m) -> wrap i (nb i m) :: n') . fromNodeTag) ns
    <*> traverse (fmap (\(s, t, mi, m) -> wrap (s, t) (eb s t mi m) :: e') . fromEdgeTag) es
  where
    ns = filterTags "node" $ getChildren gc
    es = filterTags "edge" $ getChildren gc

fromNodeTag :: XmlTree -> Either String (NodeId, M.Map String String)
fromNodeTag (NTree n at) = maybeToEither "failed in converting node tags" $ do
  i <- getAttr "id" readMaybe n
  m <- datasToMap $ filterTags "data" at
  return (i, m)

fromEdgeTag :: XmlTree -> Either String (NodeId, NodeId, Maybe Int, M.Map String String)
fromEdgeTag (NTree n at) = maybeToEither "failed in converting edge tags" $ do
  s <- getAttr "source" readMaybe n
  t <- getAttr "target" readMaybe n
  m <- datasToMap $ filterTags "data" at
  return (s, t, getAttr "id" readMaybe n, m)

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
  NTree (XAttr qn) xts' <-
    find
      ( \(NTree xn _) ->
          case xn of
            XAttr qn -> localPart qn == tn
            _ -> False
      )
      xts
  return xts'

-- return attributes and children
findTag :: String -> XmlTrees -> Maybe XmlTree
findTag tn = find $ \(NTree xn _) ->
  case xn of
    XTag qn _ -> localPart qn == tn
    _ -> False

findTagE :: String -> XmlTrees -> Either String XmlTree
findTagE tn t = maybeToEither ("failed in finding tag: " ++ tn) $ findTag tn t

filterTags :: String -> XmlTrees -> XmlTrees
filterTags tn = filter $ \(NTree xn _) -> case xn of
  XTag qn _ -> localPart qn == tn
  _ -> False

readMatrixMarket :: FilePath -> ExceptT String IO UndiGr
readMatrixMarket path =
  (\((s : _) : es) -> assoc ([] :: [NodeId]) $ fmap (\(i : j : _) -> (i, j)) es)
    <$> (ExceptT $ withFile path ReadMode $ \handle -> parse hIsEOF hGetLine handle)

split :: Eq a => a -> [a] -> [[a]]
split d cs = sps d cs [] []
  where
    sps d [] [] rs = rs
    sps d [] r rs = rs ++ [r]
    sps d (c : cs) r rs
      | c == d = sps d cs r rs
      | otherwise = spe d cs (r ++ [c]) rs
    spe d [] [] rs = rs
    spe d [] r rs = rs ++ [r]
    spe d (c : cs) r rs
      | c == d = sps d cs [] (rs ++ [r])
      | otherwise = spe d cs (r ++ [c]) rs

parse :: forall m a. Monad m => (a -> m Bool) -> (a -> m String) -> a -> m (Either String [[Int]])
parse p f a = sequenceA <$> loop mempty a
  where
    loop :: Monad m => [Either String [Int]] -> a -> m [Either String [Int]]
    loop rs a = do
      b <- p a
      if b
        then return rs
        else f a >>= \s -> loop (rs <> line s) a
    line :: String -> [Either String [Int]]
    line [] = mempty
    line s@(c : _)
      | c == '%' = mempty
      | otherwise = [traverse readEither $ split ' ' s]
