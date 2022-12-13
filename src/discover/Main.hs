{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Amazonka
import System.IO (stdout)

import qualified AWS.EC2.Instances
import qualified AWS.EC2.Vpcs
import Config

main :: IO ()
main = do
  cfg <- readConfigFile "aws-discover.yaml"
  lgr <- Amazonka.newLogger Amazonka.Info stdout
  discoveredEnv <- Amazonka.newEnv Amazonka.discover
  let env =
        discoveredEnv
          { Amazonka.envLogger = lgr
          , Amazonka.envRegion = Amazonka.Ireland
          }

  AWS.EC2.Instances.discover env cfg
  AWS.EC2.Vpcs.discover env cfg

{-
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
-}

{-
  MATCH (n) WHERE n.a = 'b' SET n.x = 'y'
-}