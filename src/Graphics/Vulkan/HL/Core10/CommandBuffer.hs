{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Graphics.Vulkan.HL.Core10.CommandBuffer
  ( allocateCommandBuffers
  , freeCommandBuffers
  , withCommandBuffers
  , beginCommandBuffer
  , endCommandBuffer
  , resetCommandBuffer
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
import Graphics.Vulkan.Core10.CommandBuffer
import Graphics.Vulkan.Core10.CommandPool
import Graphics.Vulkan.Core10.Core
import Graphics.Vulkan.Core10.DeviceInitialization
import Graphics.Vulkan.Core10.Queue
import Graphics.Vulkan.HL.Exception
import Util

allocateCommandBuffers :: MonadIO m => VkDevice -> VkCommandPool -> VkCommandBufferLevel -> Int -> m [VkCommandBuffer]
allocateCommandBuffers dev pool level count  = liftIO $
  with VkCommandBufferAllocateInfo { vkSType = VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO
                                   , vkPNext = nullPtr
                                   , vkCommandPool = pool
                                   , vkLevel = level
                                   , vkCommandBufferCount = fromIntegral count
                                   } $ \allocateInfoPtr ->
  vulkanArrayR count $ vkAllocateCommandBuffers dev allocateInfoPtr

freeCommandBuffers :: MonadIO m => VkDevice -> VkCommandPool -> [VkCommandBuffer] -> m ()
freeCommandBuffers dev pool buffers = liftIO $
  withArrayLen buffers $ \buffersLen buffersPtr ->
  vkFreeCommandBuffers dev pool (fromIntegral buffersLen) buffersPtr

withCommandBuffers :: MonadBaseControl IO m => VkDevice -> VkCommandPool -> VkCommandBufferLevel -> Int -> ([VkCommandBuffer] -> m a) -> m a
withCommandBuffers dev pool level count =
  liftBaseOp $ bracket (allocateCommandBuffers dev pool level count) (freeCommandBuffers dev pool)

beginCommandBuffer :: MonadIO m => VkCommandBufferUsageFlags -> VkCommandBuffer -> m ()
beginCommandBuffer flags buf = liftIO $
  with VkCommandBufferBeginInfo { vkSType = VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO
                                , vkPNext = nullPtr
                                , vkFlags = flags
                                , vkPInheritanceInfo = nullPtr -- TODO: Implement
                                } $ \beginInfoPtr ->
  guardVkResult =<< vkBeginCommandBuffer buf beginInfoPtr

endCommandBuffer :: MonadIO m => VkCommandBuffer -> m ()
endCommandBuffer buf = liftIO $
  guardVkResult =<< vkEndCommandBuffer buf

resetCommandBuffer :: MonadIO m => VkCommandBufferResetFlags -> VkCommandBuffer -> m ()
resetCommandBuffer flags buf = liftIO $
  guardVkResult =<< vkResetCommandBuffer buf flags
