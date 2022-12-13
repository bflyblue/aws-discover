{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Orphans where

import qualified Amazonka
import qualified Amazonka.APIGateway.Types
import qualified Amazonka.APIGateway.Types.ApiKeySourceType
import qualified Amazonka.APIGateway.Types.BasePathMapping
import qualified Amazonka.APIGateway.Types.ConnectionType
import qualified Amazonka.APIGateway.Types.DomainName
import qualified Amazonka.APIGateway.Types.EndpointConfiguration
import qualified Amazonka.APIGateway.Types.EndpointType
import qualified Amazonka.APIGateway.Types.Integration
import qualified Amazonka.APIGateway.Types.IntegrationType
import qualified Amazonka.APIGateway.Types.Method
import qualified Amazonka.APIGateway.Types.Resource
import qualified Amazonka.APIGateway.Types.RestApi
import qualified Amazonka.APIGateway.Types.SecurityPolicy
import qualified Amazonka.CloudWatchLogs.Types.LogGroup
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
import qualified Amazonka.Lambda.Types.EnvironmentResponse
import qualified Amazonka.Lambda.Types.FunctionConfiguration
import qualified Amazonka.Lambda.Types.Layer
import qualified Amazonka.Lambda.Types.PackageType
import qualified Amazonka.Lambda.Types.VpcConfigResponse
import qualified Amazonka.RDS.Types.DBInstance
import qualified Amazonka.RDS.Types.DBInstanceRole
import qualified Amazonka.RDS.Types.DBSecurityGroupMembership
import qualified Amazonka.RDS.Types.Endpoint
import qualified Amazonka.RDS.Types.ReplicaMode
import qualified Amazonka.RDS.Types.VpcSecurityGroupMembership
import qualified Amazonka.SecretsManager.GetSecretValue
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Foldable as Foldable
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map.Strict as Map
import qualified Data.Scientific as Scientific
import qualified Data.Text as Text
import Numeric.Natural (Natural)

{-
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
        , "revisionId" =: revisionId
        , "layers" =: (map Amazonka.Lambda.Types.Layer.arn <$> layers)
        , "codeSize" =: codeSize
        , "vpcId" =: (Amazonka.Lambda.Types.VpcConfigResponse.vpcId <$> vpcConfig)
        , "securityGroupsIds" =: (Amazonka.Lambda.Types.VpcConfigResponse.securityGroupIds <$> vpcConfig)
        , "subnetIds" =: (Amazonka.Lambda.Types.VpcConfigResponse.subnetIds <$> vpcConfig)
        , "packageType" =: (Amazonka.Lambda.Types.PackageType.fromPackageType <$> packageType)
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

instance Bolt.IsValue Amazonka.Lambda.Types.EnvironmentResponse.EnvironmentResponse where
  toValue Amazonka.Lambda.Types.EnvironmentResponse.EnvironmentResponse'{variables} =
    Bolt.toValue $
      maybe
        mempty
        (Map.fromList . HashMap.toList . HashMap.filterWithKey safe . fmap (Bolt.toValue . Amazonka.fromSensitive) . Amazonka.fromSensitive)
        variables
   where
    safe :: Text.Text -> Bolt.Value -> Bool
    safe key _val = safeKey key

    safeKey key = not $ any (`Text.isSuffixOf` key) ["_PASSWORD", "_KEY", "_SECRET"]

instance Bolt.IsValue Amazonka.RDS.Types.DBInstance.DBInstance where
  toValue Amazonka.RDS.Types.DBInstance.DBInstance'{..} =
    Bolt.toValue $
      Map.fromList
        [ "dbInstanceArn" =: dbInstanceArn
        , "allocatedStorage" =: allocatedStorage
        , "associatedRoles" =: (map Amazonka.RDS.Types.DBInstanceRole.roleArn <$> associatedRoles)
        , "availabilityZone" =: availabilityZone
        , "dbClusterIdentifier" =: dbClusterIdentifier
        , "dbInstanceClass" =: dbInstanceClass
        , "dbInstanceIdentifier" =: dbInstanceIdentifier
        , "dbInstancePort" =: dbInstancePort
        , "dbInstanceStatus" =: dbInstanceStatus
        , "dbName" =: dbName
        , "dbiResourceId" =: dbiResourceId
        , "engine" =: engine
        , "engineVersion" =: engineVersion
        , "maxAllocatedStorage" =: maxAllocatedStorage
        , "multiAZ" =: multiAZ
        , "networkType" =: networkType
        , "publiclyAccessible" =: publiclyAccessible
        , "readReplicaDBInstanceIdentifiers" =: readReplicaDBInstanceIdentifiers
        , "readReplicaSourceDBInstanceIdentifier" =: readReplicaSourceDBInstanceIdentifier
        , "replicaMode" =: (Amazonka.RDS.Types.ReplicaMode.fromReplicaMode <$> replicaMode)
        , "secondaryAvailabilityZone" =: secondaryAvailabilityZone
        , "storageEncrypted" =: storageEncrypted
        , "storageType" =: storageType
        , "dbSecurityGroups" =: (map Amazonka.RDS.Types.DBSecurityGroupMembership.dbSecurityGroupName <$> dbSecurityGroups)
        , "vpcSecurityGroups" =: (map Amazonka.RDS.Types.VpcSecurityGroupMembership.vpcSecurityGroupId <$> vpcSecurityGroups)
        ]

instance Bolt.IsValue Amazonka.RDS.Types.Endpoint.Endpoint where
  toValue Amazonka.RDS.Types.Endpoint.Endpoint'{..} =
    Bolt.toValue $
      Map.fromList
        [ "port" =: port
        , "hostedZoneId" =: hostedZoneId
        , "address" =: address
        ]

instance Bolt.IsValue Amazonka.SecretsManager.GetSecretValue.GetSecretValueResponse where
  toValue Amazonka.SecretsManager.GetSecretValue.GetSecretValueResponse'{..} =
    Bolt.toValue $
      Map.fromList
        [ "name" =: name
        , "arn" =: arn
        , "versionId" =: versionId
        ]

instance Bolt.IsValue Amazonka.APIGateway.Types.RestApi.RestApi where
  toValue Amazonka.APIGateway.Types.RestApi.RestApi'{..} =
    Bolt.toValue $
      Map.fromList
        [ "id" =: id
        , "name" =: name
        , "description" =: description
        , "binaryMediaTypes" =: binaryMediaTypes
        , "apiKeySource" =: (Amazonka.APIGateway.Types.ApiKeySourceType.fromApiKeySourceType <$> apiKeySource)
        , "minimumCompressionSize" =: minimumCompressionSize
        , "version" =: version
        ]

instance Bolt.IsValue Amazonka.APIGateway.Types.EndpointConfiguration.EndpointConfiguration where
  toValue Amazonka.APIGateway.Types.EndpointConfiguration.EndpointConfiguration'{..} =
    Bolt.toValue $
      Map.fromList
        [ "vpcEndpointIds" =: vpcEndpointIds
        , "types" =: (map Amazonka.APIGateway.Types.EndpointType.fromEndpointType <$> types)
        ]

instance Bolt.IsValue Amazonka.APIGateway.Types.Resource.Resource where
  toValue Amazonka.APIGateway.Types.Resource.Resource'{..} =
    Bolt.toValue $
      Map.fromList
        [ "pathPart" =: pathPart
        , "path" =: path
        , "parentId" =: parentId
        , "id" =: id
        ]

instance Bolt.IsValue Amazonka.APIGateway.Types.Method.Method where
  toValue Amazonka.APIGateway.Types.Method.Method'{..} =
    Bolt.toValue $
      Map.fromList
        [ "httpMethod" =: httpMethod
        , "operationName" =: operationName
        , "authorizationType" =: authorizationType
        , "apiKeyRequired" =: apiKeyRequired
        ]

instance Bolt.IsValue Amazonka.APIGateway.Types.Integration.Integration where
  toValue Amazonka.APIGateway.Types.Integration.Integration'{..} =
    Bolt.toValue $
      Map.fromList
        [ "cacheKeyParameters" =: cacheKeyParameters
        , "cacheNamespace" =: cacheNamespace
        , "connectionId" =: connectionId
        , "connectionType" =: (Amazonka.APIGateway.Types.ConnectionType.fromConnectionType <$> connectionType)
        , "passthroughBehavior" =: passthroughBehavior
        , "requestParameters" =: (map (\(k, v) -> k <> ":" <> v) . HashMap.toList <$> requestParameters)
        , "timeoutInMillis" =: timeoutInMillis
        , "type" =: (Amazonka.APIGateway.Types.IntegrationType.fromIntegrationType <$> type')
        , "uri" =: uri
        ]

instance Bolt.IsValue Amazonka.APIGateway.Types.DomainName.DomainName where
  toValue Amazonka.APIGateway.Types.DomainName.DomainName'{..} =
    Bolt.toValue $
      Map.fromList
        [ "certificateArn" =: certificateArn
        , "certificateName" =: certificateName
        , -- , "certificateUploadDate" =: certificateUploadDate
          "distributionDomainName" =: distributionDomainName
        , "distributionHostedZoneId" =: distributionHostedZoneId
        , "domainName" =: domainName
        , -- , "endpointConfiguration" =: endpointConfiguration
          "regionalCertificateArn" =: regionalCertificateArn
        , "regionalCertificateName" =: regionalCertificateName
        , "regionalDomainName" =: regionalDomainName
        , "regionalHostedZoneId" =: regionalHostedZoneId
        , "securityPolicy" =: (Amazonka.APIGateway.Types.SecurityPolicy.fromSecurityPolicy <$> securityPolicy)
        ]

instance Bolt.IsValue Amazonka.APIGateway.Types.BasePathMapping.BasePathMapping where
  toValue Amazonka.APIGateway.Types.BasePathMapping.BasePathMapping'{..} =
    Bolt.toValue $
      Map.fromList
        [ "basePath" =: basePath
        , "restApiId" =: restApiId
        , "stage" =: stage
        ]

instance Bolt.IsValue Amazonka.CloudWatchLogs.Types.LogGroup.LogGroup where
  toValue Amazonka.CloudWatchLogs.Types.LogGroup.LogGroup'{..} =
    Bolt.toValue $
      Map.fromList
        [ "arn" =: arn
        , "storedBytes" =: storedBytes
        , "retentionInDays" =: retentionInDays
        , "kmsKeyId" =: kmsKeyId
        , "metricFilterCount" =: metricFilterCount
        , "logGroupName" =: logGroupName
        ]

instance Bolt.IsValue Aeson.Value where
  toValue :: Aeson.Value -> Bolt.Value
  toValue (Aeson.Object o) = Bolt.M $ Bolt.toValue <$> KeyMap.toMapText o
  toValue (Aeson.Array xs) = Bolt.L $ Bolt.toValue <$> Foldable.toList xs
  toValue (Aeson.String t) = Bolt.T t
  toValue (Aeson.Number s) = case Scientific.floatingOrInteger s of
    Left f -> Bolt.F f
    Right i -> Bolt.I i
  toValue (Aeson.Bool b) = Bolt.B b
  toValue Aeson.Null = Bolt.N ()
  -}