{-# OPTIONS_GHC -Wno-orphans #-}

module AWS.RDS.Orphans where

import Data.Aeson

import qualified Amazonka.RDS.Types.AvailabilityZone as AvailabilityZone
import qualified Amazonka.RDS.Types.CertificateDetails as CertificateDetails
import qualified Amazonka.RDS.Types.DBInstance as DBInstance
import qualified Amazonka.RDS.Types.DBInstanceAutomatedBackup as DBInstanceAutomatedBackup
import qualified Amazonka.RDS.Types.DBInstanceAutomatedBackupsReplication as DBInstanceAutomatedBackupsReplication
import qualified Amazonka.RDS.Types.DBInstanceRole as DBInstanceRole
import qualified Amazonka.RDS.Types.DBInstanceStatusInfo as DBInstanceStatusInfo
import qualified Amazonka.RDS.Types.DBParameterGroupStatus as DBParameterGroupStatus
import qualified Amazonka.RDS.Types.DBSecurityGroupMembership as DBSecurityGroupMembership
import qualified Amazonka.RDS.Types.DBSubnetGroup as DBSubnetGroup
import qualified Amazonka.RDS.Types.DomainMembership as DomainMembership
import qualified Amazonka.RDS.Types.Endpoint as Endpoint
import qualified Amazonka.RDS.Types.MasterUserSecret as MasterUserSecret
import qualified Amazonka.RDS.Types.OptionGroupMembership as OptionGroupMembership
import qualified Amazonka.RDS.Types.Outpost as Outpost
import qualified Amazonka.RDS.Types.PendingCloudwatchLogsExports as PendingCloudwatchLogsExports
import qualified Amazonka.RDS.Types.PendingModifiedValues as PendingModifiedValues
import qualified Amazonka.RDS.Types.ProcessorFeature as ProcessorFeature
import qualified Amazonka.RDS.Types.RestoreWindow as RestoreWindow
import qualified Amazonka.RDS.Types.Subnet as Subnet
import qualified Amazonka.RDS.Types.Tag as Tag
import qualified Amazonka.RDS.Types.VpcSecurityGroupMembership as VpcSecurityGroupMembership

instance ToJSON AvailabilityZone.AvailabilityZone where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON CertificateDetails.CertificateDetails where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON DBInstanceRole.DBInstanceRole where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON DBInstanceAutomatedBackupsReplication.DBInstanceAutomatedBackupsReplication where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON DBInstanceAutomatedBackup.DBInstanceAutomatedBackup where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON DBInstance.DBInstance where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON DBInstanceStatusInfo.DBInstanceStatusInfo where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON DBParameterGroupStatus.DBParameterGroupStatus where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON DBSecurityGroupMembership.DBSecurityGroupMembership where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON DBSubnetGroup.DBSubnetGroup where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON DomainMembership.DomainMembership where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON Endpoint.Endpoint where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON MasterUserSecret.MasterUserSecret where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON OptionGroupMembership.OptionGroupMembership where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON PendingCloudwatchLogsExports.PendingCloudwatchLogsExports where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON PendingModifiedValues.PendingModifiedValues where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON ProcessorFeature.ProcessorFeature where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON RestoreWindow.RestoreWindow where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON Outpost.Outpost where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON Subnet.Subnet where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON Tag.Tag where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON VpcSecurityGroupMembership.VpcSecurityGroupMembership where
  toEncoding = genericToEncoding defaultOptions
