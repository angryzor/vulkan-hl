{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Graphics.Vulkan.HL.Core10.LayerDiscovery
  ( enumerateInstanceLayerProperties
  , enumerateDeviceLayerProperties
  ) where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Data.Bits
import Data.Word
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal
import Foreign.Marshal.Array
import Graphics.Vulkan.Core10.Core
import Graphics.Vulkan.Core10.DeviceInitialization
import Graphics.Vulkan.Core10.LayerDiscovery
import Graphics.Vulkan.HL.Exception
import Util

data LayerProperties = LayerProperties { layerName :: String
                                       , specVersion :: Word32
                                       , implementationVersion :: Word32
                                       , description :: String
                                       }

convertLayerProperties :: VkLayerProperties -> IO LayerProperties
convertLayerProperties VkLayerProperties{..} = do
  layerName <- sizedVectorToString vkLayerName
  description <- sizedVectorToString vkDescription
  return LayerProperties { layerName = layerName
                         , specVersion = vkSpecVersion
                         , implementationVersion = vkImplementationVersion
                         , description = description
                         }

enumerateInstanceLayerProperties :: MonadIO m => m [LayerProperties]
enumerateInstanceLayerProperties =
  liftIO $ traverse convertLayerProperties =<< vulkanEnumerationR vkEnumerateInstanceLayerProperties

enumerateDeviceLayerProperties :: MonadIO m => VkPhysicalDevice -> m [LayerProperties]
enumerateDeviceLayerProperties =
  liftIO . traverse convertLayerProperties <=< liftIO . vulkanEnumerationR . vkEnumerateDeviceLayerProperties
