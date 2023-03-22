{-# OPTIONS_GHC -Wno-orphans #-}

module AWS.CloudFormation.Orphans where

import Data.Aeson

import qualified Amazonka.CloudFormation.Types.ModuleInfo as ModuleInfo
import qualified Amazonka.CloudFormation.Types.StackDriftInformationSummary as StackDriftInformationSummary
import qualified Amazonka.CloudFormation.Types.StackResourceDriftInformation as StackResourceDriftInformation
import qualified Amazonka.CloudFormation.Types.StackResourceDriftInformationSummary as StackResourceDriftInformationSummary
import qualified Amazonka.CloudFormation.Types.StackResourceSummary as StackResourceSummary
import qualified Amazonka.CloudFormation.Types.StackSummary as StackSummary

instance ToJSON ModuleInfo.ModuleInfo where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON StackDriftInformationSummary.StackDriftInformationSummary where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON StackResourceDriftInformation.StackResourceDriftInformation where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON StackResourceDriftInformationSummary.StackResourceDriftInformationSummary where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON StackResourceSummary.StackResourceSummary where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON StackSummary.StackSummary where
  toEncoding = genericToEncoding defaultOptions