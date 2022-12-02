{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Graph where

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Hashable (Hashable)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.UUID
import qualified Data.UUID.V4 as V4
import qualified Graph.Properties as Prop

type Graph a = Prop.Graph Edge Node (Attributes a)

newtype Node = Node {nodeId :: UUID}
  deriving (Show, Eq, Ord, Hashable)

newtype Edge = Edge {edgeId :: UUID}
  deriving (Show, Eq, Ord, Hashable)

type Label = Text

type Labels = HS.HashSet Label

newtype Properties a = Properties {unProperties :: HM.HashMap Text a}
  deriving (Show, Functor)

instance Semigroup a => Semigroup (Properties a) where
  Properties a <> Properties b = Properties (HM.unionWith (<>) a b)

instance Semigroup a => Monoid (Properties a) where
  mempty = Properties HM.empty

data Attributes a = Attributes
  { attrLabels :: Labels
  , attrProperties :: Properties a
  }
  deriving (Show)

instance Semigroup a => Semigroup (Attributes a) where
  a <> b = Attributes (attrLabels a <> attrLabels b) (attrProperties a <> attrProperties b)

instance Semigroup a => Monoid (Attributes a) where
  mempty = Attributes mempty mempty

newNode :: IO Node
newNode = Node <$> V4.nextRandom

newEdge :: IO Edge
newEdge = Edge <$> V4.nextRandom

empty :: Graph a
empty = Prop.empty

vertex :: Semigroup a => Node -> Graph a
vertex = Prop.vertex

edge :: Semigroup a => Edge -> Node -> Node -> Graph a
edge = Prop.edge

(-<) :: Node -> Edge -> (Node, Edge)
a -< e = (a, e)

(>-) :: Semigroup a => (Node, Edge) -> Node -> Graph a
(a, e) >- b = edge e a b

infixl 8 -<, >-

(#) :: HasAttributes a => a -> Text -> Graph b -> Graph b
(#) = label

(#=) :: (HasAttributes a, Semigroup b) => a -> [Property b] -> Graph b -> Graph b
a #= p = mergeProperties a (props p)

infix 7 #, #=

class HasAttributes a where
  maybeAttributes :: a -> Graph b -> Maybe (Attributes b)
  attributes :: a -> Graph b -> Attributes b
  default attributes :: (Show a) => a -> Graph b -> Attributes b
  attributes a g = fromMaybe (error $ "Missing attributes for " <> show a) $ maybeAttributes a g
  adjustAttributes :: (Attributes b -> Attributes b) -> a -> Graph b -> Graph b
  {-# MINIMAL maybeAttributes, adjustAttributes #-}

instance HasAttributes Node where
  maybeAttributes n = HM.lookup n . Prop.vertexProperties
  adjustAttributes f = Prop.mapVertexProperties . HM.adjust f

instance HasAttributes Edge where
  maybeAttributes n = HM.lookup n . Prop.edgeProperties
  adjustAttributes f = Prop.mapEdgeProperties . HM.adjust f

labels :: HasAttributes a => a -> Graph b -> Labels
labels a = attrLabels . attributes a

properties :: HasAttributes a => a -> Graph b -> Properties b
properties a = attrProperties . attributes a

adjustLabels :: HasAttributes a => (Labels -> Labels) -> a -> Graph b -> Graph b
adjustLabels f = adjustAttributes (\a -> a{attrLabels = f (attrLabels a)})

adjustProperties :: HasAttributes a => (Properties b -> Properties b) -> a -> Graph b -> Graph b
adjustProperties f = adjustAttributes (\a -> a{attrProperties = f (attrProperties a)})

label :: HasAttributes a => a -> Text -> Graph b -> Graph b
label a l = adjustLabels (HS.insert l) a

mergeProperties :: (HasAttributes a, Semigroup b) => a -> Properties b -> Graph b -> Graph b
mergeProperties a p = adjustProperties (<> p) a

data Property a = Prop Text a

(.=) :: Text -> a -> Property a
a .= b = Prop a b

props :: [Property a] -> Properties a
props ps = Properties $ HM.fromList [(k, v) | Prop k v <- ps]

nodeList :: Graph a -> [Node]
nodeList = Prop.vertexList

edgeList :: Graph a -> [(Edge, Node, Node)]
edgeList = Prop.edgeList