{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Graphics.Vulkan.HL.Core10.QueueSemaphore
  ( createSemaphore
  , destroySemaphore
  , withSemaphore
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
import Graphics.Vulkan.Core10.Queue
import Graphics.Vulkan.Core10.QueueSemaphore
import Graphics.Vulkan.HL.Exception
import Util

createSemaphore :: MonadIO m => VkDevice -> m VkSemaphore
createSemaphore dev = liftIO $
  with VkSemaphoreCreateInfo { vkSType = VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO
                             , vkPNext = nullPtr
                             , vkFlags = zeroBits
                             } $ \createInfoPtr ->
  vulkanPtrR $ vkCreateSemaphore dev createInfoPtr nullPtr

destroySemaphore :: MonadIO m => VkDevice -> VkSemaphore -> m ()
destroySemaphore dev semaphore =
  liftIO $ vkDestroySemaphore dev semaphore nullPtr

withSemaphore :: MonadBaseControl IO m => VkDevice -> (VkSemaphore -> m a) -> m a
withSemaphore dev =
  liftBaseOp $ bracket (createSemaphore dev) (destroySemaphore dev)
