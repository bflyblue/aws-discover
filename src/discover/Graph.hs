{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Graph (
  module Graph,
  Aeson.KeyValue (..),
) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Hashable (Hashable)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.UUID
import qualified Data.UUID.V4 as V4
import qualified Graph.Properties as Prop

type Graph = Prop.Graph Edge Node Attributes

newtype Node = Node {nodeId :: UUID}
  deriving (Show, Eq, Ord, Hashable)

newtype Edge = Edge {edgeId :: UUID}
  deriving (Show, Eq, Ord, Hashable)

type Label = Text

type Labels = HS.HashSet Label

type Properties = Aeson.Object

data Attributes = Attributes
  { attrLabels :: !Labels
  , attrProperties :: !Properties
  }
  deriving (Show)

instance Semigroup Attributes where
  a <> b = Attributes (attrLabels a <> attrLabels b) (attrProperties a <> attrProperties b)

instance Monoid Attributes where
  mempty = Attributes mempty mempty

newNode :: IO Node
newNode = Node <$> V4.nextRandom

newEdge :: IO Edge
newEdge = Edge <$> V4.nextRandom

empty :: Graph
empty = Prop.empty

vertex :: Node -> Graph
vertex = Prop.vertex

edge :: Edge -> Node -> Node -> Graph
edge = Prop.edge

(-<) :: Node -> Edge -> (Node, Edge)
a -< e = (a, e)

(>-) :: (Node, Edge) -> Node -> Graph
(a, e) >- b = edge e a b

infixl 8 -<, >-

(#) :: HasAttributes a => a -> Text -> Graph -> Graph
(#) = label

(#=) :: (HasAttributes a) => a -> Properties -> Graph -> Graph
a #= p = mergeProperties a p

infix 7 #, #=

class HasAttributes a where
  maybeAttributes :: a -> Graph -> Maybe Attributes
  attributes :: a -> Graph -> Attributes
  default attributes :: (Show a) => a -> Graph -> Attributes
  attributes a g = fromMaybe (error $ "Missing attributes for " <> show a) $ maybeAttributes a g
  adjustAttributes :: (Attributes -> Attributes) -> a -> Graph -> Graph
  {-# MINIMAL maybeAttributes, adjustAttributes #-}

instance HasAttributes Node where
  maybeAttributes n = HM.lookup n . Prop.vertexProperties
  adjustAttributes f = Prop.mapVertexProperties . HM.adjust f

instance HasAttributes Edge where
  maybeAttributes n = HM.lookup n . Prop.edgeProperties
  adjustAttributes f = Prop.mapEdgeProperties . HM.adjust f

labels :: HasAttributes a => a -> Graph -> Labels
labels a = attrLabels . attributes a

properties :: HasAttributes a => a -> Graph -> Properties
properties a = attrProperties . attributes a

adjustLabels :: HasAttributes a => (Labels -> Labels) -> a -> Graph -> Graph
adjustLabels f = adjustAttributes (\a -> a{attrLabels = f (attrLabels a)})

adjustProperties :: HasAttributes a => (Properties -> Properties) -> a -> Graph -> Graph
adjustProperties f = adjustAttributes (\a -> a{attrProperties = f (attrProperties a)})

label :: HasAttributes a => a -> Text -> Graph -> Graph
label a l = adjustLabels (HS.insert l) a

mergeProperties :: (HasAttributes a) => a -> Properties -> Graph -> Graph
mergeProperties a p = adjustProperties (<> p) a

props :: [(Aeson.Key, Aeson.Value)] -> Aeson.Object
props = KeyMap.fromList

nodeList :: Graph -> [Node]
nodeList = Prop.vertexList

edgeList :: Graph -> [(Edge, Node, Node)]
edgeList = Prop.edgeList