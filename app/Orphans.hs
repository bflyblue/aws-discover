{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Orphans where

import qualified Amazonka.EC2.Types.GroupIdentifier
import qualified Amazonka.EC2.Types.Instance
import qualified Amazonka.EC2.Types.InstanceState
import qualified Amazonka.EC2.Types.InstanceStateName
import qualified Amazonka.EC2.Types.InstanceType
import qualified Amazonka.EC2.Types.IpPermission
import qualified Amazonka.EC2.Types.IpRange
import qualified Amazonka.EC2.Types.SecurityGroup
import qualified Amazonka.EC2.Types.Subnet
import qualified Amazonka.EC2.Types.UserIdGroupPair
import qualified Amazonka.EC2.Types.Vpc
import qualified Amazonka.EC2.Types.VpcCidrBlockAssociation
import qualified Amazonka.EC2.Types.VpcIpv6CidrBlockAssociation
import qualified Amazonka.EC2.Types.VpcState
import qualified Amazonka.Lambda.Types.FunctionConfiguration
import qualified Amazonka.Lambda.Types.VpcConfigResponse

import qualified Data.Map.Strict as Map
import Database.Bolt ((=:))
import qualified Database.Bolt as Bolt
import Numeric.Natural (Natural)

instance Bolt.IsValue Natural where
  toValue = Bolt.toValue . fromIntegral @Natural @Integer

instance Bolt.IsValue Amazonka.EC2.Types.Instance.Instance where
  toValue Amazonka.EC2.Types.Instance.Instance'{..} =
    Bolt.toValue $
      Map.fromList
        [ "instanceId" =: instanceId
        , "instanceType" =: Amazonka.EC2.Types.InstanceType.fromInstanceType instanceType
        , "platformDetails" =: platformDetails
        , "privateDnsName" =: privateDnsName
        , "privateIpAddress" =: privateIpAddress
        , "publicDnsName" =: publicDnsName
        , "publicIpAddress" =: publicIpAddress
        , "securityGroups" =: (map Amazonka.EC2.Types.GroupIdentifier.groupId <$> securityGroups)
        , "state" =: Amazonka.EC2.Types.InstanceStateName.fromInstanceStateName (Amazonka.EC2.Types.InstanceState.name state)
        , "subnetId" =: subnetId
        , "vpcId" =: vpcId
        ]

instance Bolt.IsValue Amazonka.EC2.Types.Vpc.Vpc where
  toValue Amazonka.EC2.Types.Vpc.Vpc'{..} =
    Bolt.toValue $
      Map.fromList
        [ "ownerId" =: ownerId
        , "isDefault" =: isDefault
        , "state" =: Amazonka.EC2.Types.VpcState.fromVpcState state
        , "vpcId" =: vpcId
        ]

instance Bolt.IsValue Amazonka.EC2.Types.VpcCidrBlockAssociation.VpcCidrBlockAssociation where
  toValue Amazonka.EC2.Types.VpcCidrBlockAssociation.VpcCidrBlockAssociation'{..} =
    Bolt.toValue $
      Map.fromList
        [ "cidrBlock" =: cidrBlock
        , "associationId" =: associationId
        ]

instance Bolt.IsValue Amazonka.EC2.Types.VpcIpv6CidrBlockAssociation.VpcIpv6CidrBlockAssociation where
  toValue Amazonka.EC2.Types.VpcIpv6CidrBlockAssociation.VpcIpv6CidrBlockAssociation'{..} =
    Bolt.toValue $
      Map.fromList
        [ "networkBorderGroup" =: networkBorderGroup
        , "ipv6Pool" =: ipv6Pool
        , "ipv6CidrBlock" =: ipv6CidrBlock
        , "associationId" =: associationId
        ]

instance Bolt.IsValue Amazonka.EC2.Types.Subnet.Subnet where
  toValue Amazonka.EC2.Types.Subnet.Subnet'{..} =
    Bolt.toValue $
      Map.fromList
        [ "ownerId" =: ownerId
        , "subnetId" =: subnetId
        , "subnetArn" =: subnetArn
        , "cidrBlock" =: cidrBlock
        , "availabilityZoneId" =: availabilityZoneId
        , "availabilityZone" =: availabilityZone
        , "vpcId" =: vpcId
        ]

instance Bolt.IsValue Amazonka.Lambda.Types.FunctionConfiguration.FunctionConfiguration where
  toValue Amazonka.Lambda.Types.FunctionConfiguration.FunctionConfiguration'{..} =
    Bolt.toValue $
      Map.fromList
        [ "functionName" =: functionName
        , "functionArn" =: functionArn
        , "timeout" =: timeout
        , "memorySize" =: memorySize
        , "handler" =: handler
        , "role" =: role'
        , "version" =: version
        , "codeSize" =: codeSize
        , "vpcId" =: (Amazonka.Lambda.Types.VpcConfigResponse.vpcId <$> vpcConfig)
        , "securityGroupsIds" =: (Amazonka.Lambda.Types.VpcConfigResponse.securityGroupIds <$> vpcConfig)
        , "subnetIds" =: (Amazonka.Lambda.Types.VpcConfigResponse.subnetIds <$> vpcConfig)
        ]

instance Bolt.IsValue Amazonka.EC2.Types.SecurityGroup.SecurityGroup where
  toValue Amazonka.EC2.Types.SecurityGroup.SecurityGroup'{..} =
    Bolt.toValue $
      Map.fromList
        [ "groupId" =: groupId
        , "groupName" =: groupName
        , "description" =: description
        , "ownerId" =: ownerId
        , "vpcId" =: vpcId
        ]

instance Bolt.IsValue Amazonka.EC2.Types.IpPermission.IpPermission where
  toValue Amazonka.EC2.Types.IpPermission.IpPermission'{..} =
    Bolt.toValue $
      Map.fromList
        [ "ipProtocol" =: ipProtocol
        , "fromPort" =: fromPort
        , "toPort" =: toPort
        , "ipRanges" =: (map Amazonka.EC2.Types.IpRange.cidrIp <$> ipRanges)
        , "userIdGroupPairs" =: (map Amazonka.EC2.Types.UserIdGroupPair.groupId <$> userIdGroupPairs)
        ]

{-
instance Bolt.IsValue Amazonka.EC2.Types.IpRange.IpRange where
  toValue Amazonka.EC2.Types.IpRange.IpRange'{..} =
    Bolt.toValue $
      Map.fromList
        [ "cidrIp" =: cidrIp
        , "description" =: description
        ]
-}