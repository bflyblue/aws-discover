{-# LANGUAGE OverloadedStrings #-}

module Main where

import Config
import Database
import Hasql.Session

main :: IO ()
main = do
  cfg <- readConfigFile "aws-discover.yaml"
  r <- withDb cfg (run (runExpr example))
  print r
 where
  example = Let "a" (NewNode (lit mempty) (lit props)) (AddNodeLabels (lit labels) "a")
  labels = mkLabels ["a"]
  props = mkProps [("p1", "123")]