{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Graphics.Vulkan.HL.Core10.Buffer
  ( createBuffer
  , destroyBuffer
  , withBuffer
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
import Graphics.Vulkan.Core10.MemoryManagement
import Graphics.Vulkan.Core10.Buffer
import Graphics.Vulkan.HL.Exception
import Util

createBuffer :: MonadIO m => VkDevice -> VkBufferCreateFlags -> VkDeviceSize -> VkBufferUsageFlags -> VkSharingMode -> [Word32] -> m VkBuffer
createBuffer dev flags size usage sharingMode queueFamilyIndices = liftIO $
  withArrayLen queueFamilyIndices $ \queueFamilyIndicesLen queueFamilyIndicesPtr ->
  with VkBufferCreateInfo { vkSType = VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO
                          , vkPNext = nullPtr
                          , vkFlags = flags
                          , vkSize = size
                          , vkUsage = usage
                          , vkSharingMode = sharingMode
                          , vkQueueFamilyIndexCount = fromIntegral queueFamilyIndicesLen
                          , vkPQueueFamilyIndices = queueFamilyIndicesPtr
                          } $ \createInfoPtr ->
  vulkanPtrR $ vkCreateBuffer dev createInfoPtr nullPtr

destroyBuffer :: MonadIO m => VkDevice -> VkBuffer -> m ()
destroyBuffer dev buffer =
  liftIO $ vkDestroyBuffer dev buffer nullPtr

withBuffer :: MonadBaseControl IO m => VkDevice -> VkBufferCreateFlags -> VkDeviceSize -> VkBufferUsageFlags -> VkSharingMode -> [Word32] -> (VkBuffer -> m a) -> m a
withBuffer dev flags size usage sharingMode queueFamilyIndices =
  liftBaseOp $ bracket (createBuffer dev flags size usage sharingMode queueFamilyIndices) (destroyBuffer dev)
