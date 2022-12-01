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

alterVertex :: (Eq v, H.Hashable v) => (Maybe p -> Maybe p) -> v -> Graph e v p -> Graph e v p
alterVertex f v g = g{vertexProperties = HM.alter f v (vertexProperties g)}

adjustEdge :: (Eq e, H.Hashable e) => (p -> p) -> e -> Graph e v p -> Graph e v p
adjustEdge f e g = g{edgeProperties = HM.adjust f e (edgeProperties g)}

alterEdge :: (Eq e, H.Hashable e) => (Maybe p -> Maybe p) -> e -> Graph e v p -> Graph e v p
alterEdge f v g = g{edgeProperties = HM.alter f v (edgeProperties g)}

vertex :: H.Hashable v => v -> Graph e v p
vertex v = Graph (Labelled.vertex v) HM.empty HM.empty

edge :: e -> v -> v -> Graph e v p
edge e a b = Graph (Labelled.edge [e] a b) HM.empty HM.empty

vertexLabel :: (Eq v, H.Hashable v) => v -> Graph e v p -> Maybe p
vertexLabel v = HM.lookup v . vertexProperties

edgeLabel :: (Eq e, H.Hashable e) => e -> Graph e v p -> Maybe p
edgeLabel e = HM.lookup e . edgeProperties

labelVertex :: (Eq v, H.Hashable v) => v -> p -> Graph e v p -> Graph e v p
labelVertex v p = alterVertex (const $ Just p) v

labelEdge :: (Eq e, H.Hashable e) => e -> p -> Graph e v p -> Graph e v p
labelEdge v p = alterEdge (const $ Just p) v

vertexList :: Ord v => Graph e v p -> [v]
vertexList = Labelled.vertexList . graph

edgeList :: (Ord v, Eq e) => Graph e v p -> [(e, v, v)]
edgeList = concatMap flatten . Labelled.edgeList . graph
 where
  flatten (es, a, b) = map (,a,b) es