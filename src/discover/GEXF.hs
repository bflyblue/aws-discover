{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module GEXF where

import Data.Default (def)
import Data.List (sort)
import qualified Data.Text as Text
import qualified Data.UUID as UUID
import Database.Types
import qualified Text.XML as XML

writeGEXF :: FilePath -> Graph -> IO ()
writeGEXF filename g =
  XML.writeFile xmlSettings filename $ graphDoc g
 where
  xmlSettings = def{XML.rsPretty = True}

graphDoc :: Graph -> XML.Document
graphDoc g = XML.Document (XML.Prologue [] Nothing []) (gexf g) []

gexf :: Graph -> XML.Element
gexf g =
  XML.Element
    "gexf"
    [ ("xmlns", "http://gexf.net/1.3")
    , ("xmlns:xsi", "http://www.w3.org/2001/XMLSchema−instance")
    , ("xsi:schemaLocation", "http://gexf.net/1.3 http://gexf.net/1.3/gexf.xsd")
    , ("version", "1.3")
    ]
    [ XML.NodeElement $ graph g
    ]

graph :: Graph -> XML.Element
graph g =
  XML.Element
    "graph"
    [("defaultedgetype", "directed")]
    [ XML.NodeElement $ nodes (nodeList g)
    , XML.NodeElement $ edges (edgeFlatList g)
    ]

nodes :: [Node] -> XML.Element
nodes = XML.Element "nodes" [] . map (XML.NodeElement . node)

edges :: [(Edge, Node, Node)] -> XML.Element
edges = XML.Element "edges" [] . map (XML.NodeElement . edge)

node :: Node -> XML.Element
node n =
  XML.Element
    "node"
    [ ("id", UUID.toText $ unId $ nodeId n)
    , ("label", Text.intercalate "," $ nodeLabels n)
    ]
    []

edge :: (Edge, Node, Node) -> XML.Element
edge (e, a, b) =
  XML.Element
    "edge"
    [ ("id", UUID.toText $ unId $ edgeId e)
    , ("label", Text.intercalate "," $ edgeLabels e)
    , ("source", UUID.toText $ unId $ nodeId a)
    , ("target", UUID.toText $ unId $ nodeId b)
    , ("kind", Text.intercalate "," $ sort $ edgeLabels e)
    ]
    []