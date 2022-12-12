{-# LANGUAGE OverloadedStrings #-}

module Main where

import Config
import Database

main :: IO ()
main = do
  cfg <- readConfigFile "aws-discover.yaml"
  r <- withDb cfg (run example)
  print r

example :: Db (Id Node, Id Node, Id Edge)
example = do
  a <- createNode (mkLabels ["Instance"]) (mkProps [("name", "inst1")])
  b <- createNode (mkLabels ["VPC"]) (mkProps [("name", "vpc1")])

  e <- createEdge (mkLabels ["InVPC"]) (mkProps []) a b

  addProperties (mkProps [("architecture", "x64")]) [a]

  return (a, b, e)

{-
  MATCH (n) WHERE n.a = 'b' SET n.x = 'y'
-}