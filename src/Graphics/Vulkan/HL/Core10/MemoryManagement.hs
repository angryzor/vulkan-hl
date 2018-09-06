{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Graphics.Vulkan.HL.Core10.MemoryManagement
  ( getBufferMemoryRequirements
  , bindBufferMemory
  , getImageMemoryRequirements
  , bindImageMemory
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
import Graphics.Vulkan.Core10.Memory
import Graphics.Vulkan.Core10.MemoryManagement
import Graphics.Vulkan.Core10.Buffer
import Graphics.Vulkan.HL.Exception
import Util

getBufferMemoryRequirements :: MonadIO m => VkDevice -> VkBuffer -> m VkMemoryRequirements
getBufferMemoryRequirements dev buffer =
  liftIO . vulkanPtr $ vkGetBufferMemoryRequirements dev buffer

bindBufferMemory :: MonadIO m => VkDevice -> VkBuffer -> VkDeviceMemory -> VkDeviceSize -> m ()
bindBufferMemory dev buffer memory memoryOffset =
  liftIO $ guardVkResult =<< vkBindBufferMemory dev buffer memory memoryOffset

getImageMemoryRequirements :: MonadIO m => VkDevice -> VkImage -> m VkMemoryRequirements
getImageMemoryRequirements dev buffer =
  liftIO . vulkanPtr $ vkGetImageMemoryRequirements dev buffer

bindImageMemory :: MonadIO m => VkDevice -> VkImage -> VkDeviceMemory -> VkDeviceSize -> m ()
bindImageMemory dev buffer memory memoryOffset =
  liftIO $ guardVkResult =<< vkBindImageMemory dev buffer memory memoryOffset
