{-# LANGUAGE TupleSections #-}

module Graph.Properties where

import qualified Algebra.Graph.Labelled as Labelled
import qualified Data.HashMap.Strict as HM
import Data.Hashable (Hashable)

data Graph e v p = Graph
  { graph :: Labelled.Graph [e] v
  , vertexProperties :: HM.HashMap v p
  , edgeProperties :: HM.HashMap e p
  }
  deriving (Show)

instance (Hashable v, Hashable e, Eq v, Eq e, Semigroup p) => Semigroup (Graph e v p) where
  a <> b =
    Graph
      { graph = graph a <> graph b
      , vertexProperties = HM.unionWith (<>) (vertexProperties a) (vertexProperties b)
      , edgeProperties = HM.unionWith (<>) (edgeProperties a) (edgeProperties b)
      }

instance (Hashable v, Hashable e, Eq v, Eq e, Monoid p) => Monoid (Graph e v p) where
  mempty = empty

empty :: Graph e v p
empty = Graph{graph = Labelled.empty, vertexProperties = HM.empty, edgeProperties = HM.empty}

vertex' :: (Hashable v, Monoid p) => v -> p -> Graph e v p
vertex' v p = empty{graph = Labelled.vertex v, vertexProperties = HM.singleton v p}

vertex :: (Hashable v, Monoid p) => v -> Graph e v p
vertex v = vertex' v mempty

edge :: (Eq v, Hashable v, Hashable e, Monoid p) => e -> v -> v -> Graph e v p
edge e a b = empty{graph = Labelled.edge [e] a b, vertexProperties = HM.fromList [(a, mempty), (b, mempty)], edgeProperties = HM.singleton e mempty}

edge' :: (Eq v, Hashable v, Hashable e, Monoid p) => e -> p -> v -> p -> v -> p -> Graph e v p
edge' e ep a ap b bp =
  empty
    { graph = Labelled.edge [e] a b
    , vertexProperties = HM.fromList [(a, ap), (b, bp)]
    , edgeProperties = HM.singleton e ep
    }

mapVertexProperties :: (HM.HashMap v p -> HM.HashMap v p) -> Graph e v p -> Graph e v p
mapVertexProperties f g = g{vertexProperties = f (vertexProperties g)}

mapEdgeProperties :: (HM.HashMap e p -> HM.HashMap e p) -> Graph e v p -> Graph e v p
mapEdgeProperties f g = g{edgeProperties = f (edgeProperties g)}

vertexList :: Ord v => Graph e v p -> [v]
vertexList = Labelled.vertexList . graph

edgeList :: (Ord v, Eq e) => Graph e v p -> [(e, v, v)]
edgeList = concatMap flatten . Labelled.edgeList . graph
 where
  flatten (es, a, b) = map (,a,b) es