{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}

module Graph.Properties where

import qualified Algebra.Graph.Labelled as Labelled
import qualified Data.HashMap.Strict as HM
import qualified Data.Hashable as H
import Data.Text (Text)

data Graph e v p = Graph
  { graph :: Labelled.Graph [e] v
  , vertexProperties :: HM.HashMap v p
  , edgeProperties :: HM.HashMap e p
  }
  deriving (Show, Functor)

instance (Eq e, H.Hashable e, Eq v, H.Hashable v, Semigroup p) => Semigroup (Graph e v p) where
  a <> b =
    Graph
      { graph = graph a <> graph b
      , vertexProperties = HM.unionWith (<>) (vertexProperties a) (vertexProperties b)
      , edgeProperties = HM.unionWith (<>) (edgeProperties a) (edgeProperties b)
      }

instance (Eq e, H.Hashable e, Eq v, H.Hashable v, Semigroup p) => Monoid (Graph e v p) where
  mempty = empty

newtype Properties a = Properties {unProperties :: HM.HashMap Text a}
  deriving (Show, Functor)

instance Semigroup a => Semigroup (Properties a) where
  Properties a <> Properties b = Properties (HM.unionWith (<>) a b)

instance Semigroup a => Monoid (Properties a) where
  mempty = Properties HM.empty

empty :: Graph e v p
empty = Graph{graph = Labelled.empty, vertexProperties = HM.empty, edgeProperties = HM.empty}

adjustVertex :: (Eq v, H.Hashable v) => (p -> p) -> v -> Graph e v p -> Graph e v p
adjustVertex f v g = g{vertexProperties = HM.adjust f v (vertexProperties g)}

adjustEdge :: (Eq e, H.Hashable e) => (p -> p) -> e -> Graph e v p -> Graph e v p
adjustEdge f e g = g{edgeProperties = HM.adjust f e (edgeProperties g)}

vertex :: H.Hashable v => (v, p) -> Graph e v p
vertex (v, p) = Graph (Labelled.vertex v) (HM.singleton v p) HM.empty

edge ::
  (H.Hashable v, H.Hashable e, Semigroup p, Eq v) =>
  (e, p) ->
  (v, p) ->
  (v, p) ->
  Graph e v p
edge (e, ep) (a, ap) (b, bp) =
  Graph
    (Labelled.edge [e] a b)
    (HM.singleton a ap <> HM.singleton b bp)
    (HM.singleton e ep)

vertexList :: (H.Hashable v, Eq v, Ord v) => Graph e v p -> [(v, p)]
vertexList g =
  map withProperties (Labelled.vertexList $ graph g)
 where
  withProperties v = (v, vertexProperties g HM.! v)

edgeList ::
  (H.Hashable v, Eq v, Ord v, H.Hashable e, Eq e) =>
  Graph e v p ->
  [(e, p, v, p, v, p)]
edgeList g =
  map withProperties $ concatMap flatten (Labelled.edgeList $ graph g)
 where
  flatten (es, a, b) = map (,a,b) es
  withProperties (e, a, b) =
    ( e
    , edgeProperties g HM.! e
    , a
    , vertexProperties g HM.! a
    , b
    , vertexProperties g HM.! b
    )