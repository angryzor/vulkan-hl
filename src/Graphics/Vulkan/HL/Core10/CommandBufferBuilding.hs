{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}

module Graphics.Vulkan.HL.Core10.CommandBufferBuilding
  ( Command (..)
  , record
  , recordCommandBuffer
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
import Graphics.Vulkan.Core10.CommandBufferBuilding
import Graphics.Vulkan.Core10.Core
import Graphics.Vulkan.Core10.DeviceInitialization
import Graphics.Vulkan.Core10.Pass
import Graphics.Vulkan.Core10.Pipeline
import Graphics.Vulkan.Core10.Queue
import Graphics.Vulkan.HL.Core10.CommandBuffer
import Graphics.Vulkan.HL.Exception
import Util

data Command
  = BeginRenderPass { renderPass :: VkRenderPass
                    , framebuffer :: VkFramebuffer
                    , renderArea :: VkRect2D
                    , clearValues :: [VkClearValue]
                    , contents :: VkSubpassContents
                    }
  | EndRenderPass
  | BindPipeline { pipelineBindPoint :: VkPipelineBindPoint
                 , pipeline :: VkPipeline
                 }
  | Draw { vertexCount :: Word32
         , instanceCount :: Word32
         , firstVertex :: Word32
         , firstInstance :: Word32
         }

record :: MonadIO m => Command -> VkCommandBuffer -> m ()
record command buf = liftIO $
  case command of
    BeginRenderPass{..} ->
      withArrayLen clearValues $ \clearValuesLen clearValuesPtr ->
      with VkRenderPassBeginInfo { vkSType = VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO
                                , vkPNext = nullPtr
                                , vkRenderPass = renderPass
                                , vkFramebuffer = framebuffer
                                , vkRenderArea = renderArea
                                , vkClearValueCount = fromIntegral clearValuesLen
                                , vkPClearValues = clearValuesPtr
                                } $ \createInfoPtr ->
      vkCmdBeginRenderPass buf createInfoPtr contents

    EndRenderPass ->
      vkCmdEndRenderPass buf

    BindPipeline{..} ->
      vkCmdBindPipeline buf pipelineBindPoint pipeline

    Draw{..} ->
      vkCmdDraw buf vertexCount instanceCount firstVertex firstInstance

recordCommandBuffer :: MonadIO m => VkCommandBufferUsageFlags -> VkCommandBuffer -> [Command] -> m ()
recordCommandBuffer flags buf commands = liftIO $ do
  beginCommandBuffer flags buf
  mapM_ (flip record buf) commands
  endCommandBuffer buf
