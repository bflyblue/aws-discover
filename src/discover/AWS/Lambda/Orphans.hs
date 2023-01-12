{-# OPTIONS_GHC -Wno-orphans #-}

module AWS.Lambda.Orphans where

import Data.Aeson

import qualified Amazonka.Lambda.Types.EnvironmentError as EnvironmentError
import qualified Amazonka.Lambda.Types.EnvironmentResponse as EnvironmentResponse
import qualified Amazonka.Lambda.Types.FunctionConfiguration as FunctionConfiguration
import qualified Amazonka.Lambda.Types.ImageConfigError as ImageConfigError
import qualified Amazonka.Lambda.Types.ImageConfigResponse as ImageConfigResponse
import qualified Amazonka.Lambda.Types.Layer as Layer
import qualified Amazonka.Lambda.Types.TracingConfigResponse as TracingConfigResponse
import qualified Amazonka.Lambda.Types.VpcConfigResponse as VpcConfigResponse

instance ToJSON FunctionConfiguration.FunctionConfiguration where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON ImageConfigResponse.ImageConfigResponse where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON ImageConfigError.ImageConfigError where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON Layer.Layer where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON VpcConfigResponse.VpcConfigResponse where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON EnvironmentError.EnvironmentError where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON EnvironmentResponse.EnvironmentResponse where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON TracingConfigResponse.TracingConfigResponse where
  toEncoding = genericToEncoding defaultOptions
