{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Graph where

import qualified Data.Aeson as Aeson
import qualified Data.Hashable as H
import Data.Text (Text)
import Data.UUID
import qualified Graph.Properties as Prop

type Graph = Prop.Graph (Id Edge) (Id Node) GraphLabel

type Label = Text

newtype Id a = Id {unId :: UUID}
  deriving (Show, Eq, Ord, H.Hashable)

type Properties = Prop.Properties [Aeson.Value]

{-

data GraphLabel = GraphLabel
  { labels :: [Label]
  , properties :: Properties
  }
  deriving (Show)

instance Semigroup GraphLabel where
  a <> b = GraphLabel (labels a <> labels b) (properties a <> properties b)

instance Monoid GraphLabel where
  mempty = GraphLabel mempty mempty

data Labelled l a = Labelled {label :: l, labelled :: a}
  deriving (Show)

newtype Node = Node {unNode :: Labelled GraphLabel (Id Node)}
  deriving (Show)

newtype Edge = Edge {unEdge :: Labelled GraphLabel (Id Edge)}
  deriving (Show)

nodeId :: Node -> Id Node
nodeId = labelled . unNode

edgeId :: Edge -> Id Edge
edgeId = labelled . unEdge

class HasId a where
  getId :: a -> UUID

instance HasId Node where
  getId = unId . nodeId

instance HasId Edge where
  getId = unId . edgeId

class HasLabels a where
  addLabel :: Label -> a -> a
  getLabels :: a -> [Label]

instance HasLabels [Label] where
  addLabel l ls = l : ls
  getLabels = id

instance HasLabels l => HasLabels (Labelled l a) where
  addLabel l' (Labelled l a) = Labelled (addLabel l' l) a
  getLabels (Labelled l _) = getLabels l

instance HasLabels GraphLabel where
  addLabel l gl = gl{labels = addLabel l (labels gl)}
  getLabels gl = getLabels (labels gl)

instance HasLabels Node where
  addLabel l = Node . addLabel l . unNode
  getLabels = labels . label . unNode

instance HasLabels Edge where
  addLabel l = Edge . addLabel l . unEdge
  getLabels = labels . label . unEdge

class HasProperties a where
  mergeProperties :: Properties -> a -> a
  getProperties :: a -> Properties

instance HasProperties Properties where
  mergeProperties a b = a <> b
  getProperties = id

instance HasProperties l => HasProperties (Labelled l a) where
  mergeProperties p (Labelled l a) = Labelled (mergeProperties p l) a
  getProperties (Labelled l _) = getProperties l

instance HasProperties GraphLabel where
  mergeProperties p gl = gl{properties = mergeProperties p (properties gl)}
  getProperties = properties

instance HasProperties Node where
  mergeProperties p = Node . mergeProperties p . unNode
  getProperties = getProperties . unNode

instance HasProperties Edge where
  mergeProperties p = Edge . mergeProperties p . unEdge
  getProperties = getProperties . unEdge

{-
node :: Id Node -> Node
node n = Node n mempty mempty

edge :: Id Edge -> Edge
edge e = Edge e mempty mempty

relate :: Node -> Edge -> Node -> Graph
relate a e b =
  Prop.edge
    (edgeId e, Just $ Properties (edgeLabels e) (edgeProperties e))
    (nodeId a, Just $ Properties (nodeLabels a) (nodeProperties a))
    (nodeId b, Just $ Properties (nodeLabels b) (nodeProperties b))

(.:) :: HasLabels a => a -> Label -> a
a .: l = addLabel l a

infixl 0 .:

(.=) :: HasProperties a => a -> Prop.Properties [Aeson.Value] -> a
a .= p = mergeProperties p a

infixl 0 .=

nodeList :: Graph -> [Node]
nodeList = map toNode . Prop.vertexList
 where
  toNode (n, p) = Node n (fromMaybe mempty p)

edgeList :: Graph -> [(Edge, Node, Node)]
edgeList = map toTriple . Prop.edgeList
 where
  toEdge (n, p) = Node n (fromMaybe mempty p)
  toTriple (e, ep, a, ap, b, bp) = (toEdge e ep, toNode a ap, toNode b bp)
  toNode n p = Node n (maybe [] labels p) (maybe mempty properties p)
  toEdge e p = Edge e (maybe [] labels p) (maybe mempty properties p)

-}
-}