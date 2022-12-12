{-# LANGUAGE OverloadedStrings #-}

module Main where

import Config
import Control.Monad (forM_, void)
import Database

main :: IO ()
main = do
  cfg <- readConfigFile "aws-discover.yaml"
  r <- withDb cfg (run example)
  print r

example :: Db ([Id Node], [Id Node])
example = do
  as <- mergeNode (mkLabels ["Instance"]) (mkProps [("name", "inst1")])
  bs <- mergeNode (mkLabels ["VPC"]) (mkProps [("name", "vpc1")])

  forM_ as $ \a -> do
    forM_ bs $ \b -> do
      void $ mergeEdge (mkLabels ["InVPC"]) (mkProps []) a b

  addProperties (mkProps [("architecture", "x64")]) as

  -- n <- matchNode (hasLabel "Instance" .& "name" .= "inst1" .| "architecture" .= "x64")
  -- e <- matchEdge (hasLabel "InVPC") (Just a) (Just b)

  -- return (n, e)
  return (as, bs)

{-
  MATCH (n) WHERE n.a = 'b' SET n.x = 'y'
-}