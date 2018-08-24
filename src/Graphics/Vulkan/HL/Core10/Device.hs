{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Graphics.Vulkan.HL.Core10.Device
  ( DeviceQueueCreateInfo (..)
  , createDevice
  , destroyDevice
  , withDevice
  ) where

import Control.Exception
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
import Graphics.Vulkan.Core10.Device
import Graphics.Vulkan.HL.Exception
import Util

data DeviceQueueCreateInfo = DeviceQueueCreateInfo { queueFamilyIndex :: Word32
                                                   , queuePriorities :: [Float]
                                                   }

convertDeviceQueueCreateInfo :: DeviceQueueCreateInfo -> IO VkDeviceQueueCreateInfo
convertDeviceQueueCreateInfo DeviceQueueCreateInfo{..} =
  withArrayLen (CFloat <$> queuePriorities) $ \queuePrioritiesLen queuePrioritiesPtr ->
  return $ VkDeviceQueueCreateInfo { vkSType = VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO
                                   , vkPNext = nullPtr
                                   , vkFlags = zeroBits
                                   , vkQueueFamilyIndex = queueFamilyIndex
                                   , vkQueueCount = fromIntegral queuePrioritiesLen
                                   , vkPQueuePriorities = queuePrioritiesPtr
                                   }

createDevice :: MonadIO m => [DeviceQueueCreateInfo] -> [String] -> [String] -> Maybe VkPhysicalDeviceFeatures -> VkPhysicalDevice -> m VkDevice
createDevice queueCreateInfos layers extensions features physDevice = liftIO $ do
  convertedQueueCreateInfos <- traverse convertDeviceQueueCreateInfo queueCreateInfos
  withArrayLen convertedQueueCreateInfos $ \queueCreateInfosLen queueCreateInfosPtr ->
    withCStringArrayLen layers $ \layersLen layersPtr ->
    withCStringArrayLen extensions $ \extensionsLen extensionsPtr ->
    maybeWith with features $ \featuresPtr ->
    with VkDeviceCreateInfo { vkSType = VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO
                            , vkPNext = nullPtr
                            , vkFlags = zeroBits
                            , vkQueueCreateInfoCount = fromIntegral queueCreateInfosLen
                            , vkPQueueCreateInfos = queueCreateInfosPtr
                            , vkEnabledLayerCount = fromIntegral layersLen
                            , vkPPEnabledLayerNames = layersPtr
                            , vkEnabledExtensionCount = fromIntegral extensionsLen
                            , vkPPEnabledExtensionNames = extensionsPtr
                            , vkPEnabledFeatures = featuresPtr
                            } $ \createInfoPtr ->
    vulkanPtrR $ vkCreateDevice physDevice createInfoPtr nullPtr

destroyDevice :: MonadIO m => VkDevice -> m ()
destroyDevice dev =
  liftIO $ vkDestroyDevice dev nullPtr

withDevice :: MonadBaseControl IO m => [DeviceQueueCreateInfo] -> [String] -> [String] -> Maybe VkPhysicalDeviceFeatures -> VkPhysicalDevice -> (VkDevice -> m a) -> m a
withDevice queueCreateInfos layers extensions features physDevice =
  liftBaseOp $ bracket (createDevice queueCreateInfos layers extensions features physDevice) destroyDevice
