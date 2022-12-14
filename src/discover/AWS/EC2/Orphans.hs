{-# OPTIONS_GHC -Wno-orphans #-}

module AWS.EC2.Orphans where

import Data.Aeson
import Database.Types

import qualified Amazonka.EC2.Types as EC2
import qualified Amazonka.EC2.Types.CapacityReservationSpecificationResponse as CapacityReservationSpecificationResponse
import qualified Amazonka.EC2.Types.CapacityReservationTargetResponse as CapacityReservationTargetResponse
import qualified Amazonka.EC2.Types.CpuOptions as CpuOptions
import qualified Amazonka.EC2.Types.EbsInstanceBlockDevice as EbsInstanceBlockDevice
import qualified Amazonka.EC2.Types.ElasticGpuAssociation as ElasticGpuAssociation
import qualified Amazonka.EC2.Types.ElasticInferenceAcceleratorAssociation as ElasticInferenceAcceleratorAssociation
import qualified Amazonka.EC2.Types.EnclaveOptions as EnclaveOptions
import qualified Amazonka.EC2.Types.GroupIdentifier as GroupIdentifier
import qualified Amazonka.EC2.Types.HibernationOptions as HibernationOptions
import qualified Amazonka.EC2.Types.IamInstanceProfile as IamInstanceProfile
import qualified Amazonka.EC2.Types.Instance as Instance
import qualified Amazonka.EC2.Types.InstanceBlockDeviceMapping as InstanceBlockDeviceMapping
import qualified Amazonka.EC2.Types.InstanceIpv4Prefix as InstanceIpv4Prefix
import qualified Amazonka.EC2.Types.InstanceIpv6Address as InstanceIpv6Address
import qualified Amazonka.EC2.Types.InstanceIpv6Prefix as InstanceIpv6Prefix
import qualified Amazonka.EC2.Types.InstanceMaintenanceOptions as InstanceMaintenanceOptions
import qualified Amazonka.EC2.Types.InstanceMetadataOptionsResponse as InstanceMetadataOptionsResponse
import qualified Amazonka.EC2.Types.InstanceNetworkInterface as InstanceNetworkInterface
import qualified Amazonka.EC2.Types.InstanceNetworkInterfaceAssociation as InstanceNetworkInterfaceAssociation
import qualified Amazonka.EC2.Types.InstanceNetworkInterfaceAttachment as InstanceNetworkInterfaceAttachment
import qualified Amazonka.EC2.Types.InstancePrivateIpAddress as InstancePrivateIpAddress
import qualified Amazonka.EC2.Types.InstanceState as InstanceState
import qualified Amazonka.EC2.Types.LicenseConfiguration as LicenseConfiguration
import qualified Amazonka.EC2.Types.Monitoring as Monitoring
import qualified Amazonka.EC2.Types.Placement as Placement
import qualified Amazonka.EC2.Types.PrivateDnsNameOptionsOnLaunch as PrivateDnsNameOptionsOnLaunch
import qualified Amazonka.EC2.Types.PrivateDnsNameOptionsResponse as PrivateDnsNameOptionsResponse
import qualified Amazonka.EC2.Types.ProductCode as ProductCode
import qualified Amazonka.EC2.Types.StateReason as StateReason
import qualified Amazonka.EC2.Types.Subnet as Subnet
import qualified Amazonka.EC2.Types.SubnetCidrBlockState as SubnetCidrBlockState
import qualified Amazonka.EC2.Types.SubnetIpv6CidrBlockAssociation as SubnetIpv6CidrBlockAssociation
import qualified Amazonka.EC2.Types.Vpc as Vpc
import qualified Amazonka.EC2.Types.VpcCidrBlockAssociation as VpcCidrBlockAssociation
import qualified Amazonka.EC2.Types.VpcCidrBlockState as VpcCidrBlockState
import qualified Amazonka.EC2.Types.VpcIpv6CidrBlockAssociation as VpcIpv6CidrBlockAssociation

toProps :: ToJSON a => a -> Properties
toProps a = case toJSON a of
  Object o -> Properties o
  _ -> error "expected Object"

instance ToJSON CapacityReservationSpecificationResponse.CapacityReservationSpecificationResponse where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON CapacityReservationTargetResponse.CapacityReservationTargetResponse where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON CpuOptions.CpuOptions where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON EbsInstanceBlockDevice.EbsInstanceBlockDevice where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON ElasticGpuAssociation.ElasticGpuAssociation where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON ElasticInferenceAcceleratorAssociation.ElasticInferenceAcceleratorAssociation where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON EnclaveOptions.EnclaveOptions where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON GroupIdentifier.GroupIdentifier where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON IamInstanceProfile.IamInstanceProfile where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON InstanceBlockDeviceMapping.InstanceBlockDeviceMapping where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON InstanceState.InstanceState where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON Instance.Instance where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON InstanceIpv4Prefix.InstanceIpv4Prefix where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON InstanceIpv6Address.InstanceIpv6Address where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON InstanceIpv6Prefix.InstanceIpv6Prefix where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON InstanceMaintenanceOptions.InstanceMaintenanceOptions where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON InstanceNetworkInterfaceAssociation.InstanceNetworkInterfaceAssociation where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON InstanceNetworkInterface.InstanceNetworkInterface where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON InstanceNetworkInterfaceAttachment.InstanceNetworkInterfaceAttachment where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON InstanceMetadataOptionsResponse.InstanceMetadataOptionsResponse where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON InstancePrivateIpAddress.InstancePrivateIpAddress where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON HibernationOptions.HibernationOptions where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON LicenseConfiguration.LicenseConfiguration where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON Monitoring.Monitoring where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON Placement.Placement where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON PrivateDnsNameOptionsOnLaunch.PrivateDnsNameOptionsOnLaunch where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON PrivateDnsNameOptionsResponse.PrivateDnsNameOptionsResponse where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON ProductCode.ProductCode where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON StateReason.StateReason where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON SubnetCidrBlockState.SubnetCidrBlockState where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON Subnet.Subnet where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON SubnetIpv6CidrBlockAssociation.SubnetIpv6CidrBlockAssociation where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON EC2.Tag where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON Vpc.Vpc where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON VpcCidrBlockAssociation.VpcCidrBlockAssociation where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON VpcIpv6CidrBlockAssociation.VpcIpv6CidrBlockAssociation where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON VpcCidrBlockState.VpcCidrBlockState where
  toEncoding = genericToEncoding defaultOptions