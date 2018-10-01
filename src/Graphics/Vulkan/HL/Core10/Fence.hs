{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Graphics.Vulkan.HL.Core10.Fence
  ( createFence
  , destroyFence
  , withFence
  , resetFences
  , getFenceStatus
  , waitForFences
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
import Graphics.Vulkan.Core10.Fence
import Graphics.Vulkan.Core10.Queue
import Graphics.Vulkan.HL.Exception
import Util

createFence :: MonadIO m => VkDevice -> VkFenceCreateFlags -> m VkFence
createFence dev flags = liftIO $
  with VkFenceCreateInfo { vkSType = VK_STRUCTURE_TYPE_FENCE_CREATE_INFO
                         , vkPNext = nullPtr
                         , vkFlags = flags
                         } $ \createInfoPtr ->
  vulkanPtrR $ vkCreateFence dev createInfoPtr nullPtr

destroyFence :: MonadIO m => VkDevice -> VkFence -> m ()
destroyFence dev fence =
  liftIO $ vkDestroyFence dev fence nullPtr

withFence :: MonadBaseControl IO m => VkDevice -> VkFenceCreateFlags -> (VkFence -> m a) -> m a
withFence dev flags =
  liftBaseOp $ bracket (createFence dev flags) (destroyFence dev)

resetFences :: MonadIO m => VkDevice -> [VkFence] -> m ()
resetFences dev fences = liftIO $
  withArrayLen fences $ \fencesLen fencesPtr ->
  guardVkResult =<< vkResetFences dev (fromIntegral fencesLen) fencesPtr

getFenceStatus :: MonadIO m => VkDevice -> VkFence -> m VkResult
getFenceStatus dev fence =
  liftIO $ vkGetFenceStatus dev fence

waitForFences :: MonadIO m => VkDevice -> [VkFence] -> VkBool32 -> Word64 -> m ()
waitForFences dev fences waitAll timeout = liftIO $
  withArrayLen fences $ \fencesLen fencesPtr ->
  guardVkResult =<< vkWaitForFences dev (fromIntegral fencesLen) fencesPtr waitAll timeout
