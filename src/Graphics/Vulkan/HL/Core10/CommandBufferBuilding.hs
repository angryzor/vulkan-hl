{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Graphics.Vulkan.HL.Core10.CommandBufferBuilding
  ( cmdBeginRenderPass
  , cmdEndRenderPass
  , cmdBindPipeline
  , cmdDraw
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
import Graphics.Vulkan.Core10.CommandBufferBuilding
import Graphics.Vulkan.Core10.Core
import Graphics.Vulkan.Core10.DeviceInitialization
import Graphics.Vulkan.Core10.Pass
import Graphics.Vulkan.Core10.Pipeline
import Graphics.Vulkan.Core10.Queue
import Graphics.Vulkan.HL.Exception
import Util

cmdBeginRenderPass :: MonadIO m => VkCommandBuffer -> VkRenderPass -> VkFramebuffer -> VkRect2D -> [VkClearValue] -> VkSubpassContents -> m ()
cmdBeginRenderPass buf renderPass frameBuffer renderArea clearValues contents = liftIO $
  withArrayLen clearValues $ \clearValuesLen clearValuesPtr ->
  with VkRenderPassBeginInfo { vkSType = VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO
                             , vkPNext = nullPtr
                             , vkRenderPass = renderPass
                             , vkFramebuffer = frameBuffer
                             , vkRenderArea = renderArea
                             , vkClearValueCount = fromIntegral clearValuesLen
                             , vkPClearValues = clearValuesPtr
                             } $ \rpbi ->
  vkCmdBeginRenderPass buf rpbi contents

cmdEndRenderPass :: MonadIO m => VkCommandBuffer -> m ()
cmdEndRenderPass buf =
  liftIO $ vkCmdEndRenderPass buf

cmdBindPipeline :: MonadIO m => VkCommandBuffer -> VkPipelineBindPoint -> VkPipeline -> m ()
cmdBindPipeline buf pipelineBindPoint pipeline =
  liftIO $ vkCmdBindPipeline buf pipelineBindPoint pipeline

cmdDraw :: MonadIO m => VkCommandBuffer -> Word32 -> Word32 -> Word32 -> Word32 -> m ()
cmdDraw buf vertexCount instanceCount firstVertex firstInstance =
  liftIO $ vkCmdDraw buf vertexCount instanceCount firstVertex firstInstance
