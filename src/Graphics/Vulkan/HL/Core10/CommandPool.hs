{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Graphics.Vulkan.HL.Core10.CommandPool
  ( createCommandPool
  , destroyCommandPool
  , withCommandPool
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
import Graphics.Vulkan.Core10.CommandPool
import Graphics.Vulkan.HL.Exception
import Util

createCommandPool :: MonadIO m => VkDevice -> VkCommandPoolCreateFlags -> Word32 -> m VkCommandPool
createCommandPool dev flags queueFamilyIndex = liftIO $
  with VkCommandPoolCreateInfo { vkSType = VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO
                             , vkPNext = nullPtr
                             , vkFlags = flags
                             , vkQueueFamilyIndex = queueFamilyIndex
                             } $ \cpci ->
  vulkanPtrR $ vkCreateCommandPool dev cpci nullPtr

destroyCommandPool :: MonadIO m => VkDevice -> VkCommandPool -> m ()
destroyCommandPool dev imageView =
  liftIO $ vkDestroyCommandPool dev imageView nullPtr

withCommandPool :: MonadBaseControl IO m => VkDevice -> VkCommandPoolCreateFlags -> Word32 -> (VkCommandPool -> m a) -> m a
withCommandPool dev flags queueFamilyIndex =
  liftBaseOp $ bracket (createCommandPool dev flags queueFamilyIndex) (destroyCommandPool dev)
