{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}

module Graphics.Vulkan.HL.Core10.CommandBufferBuilding
  ( Command (..)
  , VertexBufferBinding (..)
  , record
  , recordCommandBuffer
  ) where

import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Data.Bits
import Data.Int
import Data.Word
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal
import Foreign.Marshal.Array
import Graphics.Vulkan.Core10.CommandBuffer
import Graphics.Vulkan.Core10.CommandBufferBuilding
import Graphics.Vulkan.Core10.Core
import Graphics.Vulkan.Core10.DescriptorSet
import Graphics.Vulkan.Core10.DeviceInitialization
import Graphics.Vulkan.Core10.MemoryManagement
import Graphics.Vulkan.Core10.Pass
import Graphics.Vulkan.Core10.Pipeline
import Graphics.Vulkan.Core10.Queue
import Graphics.Vulkan.HL.Core10.CommandBuffer
import Graphics.Vulkan.HL.Exception
import Util

data VertexBufferBinding = VertexBufferBinding { buffer :: VkBuffer
                                               , offset :: VkDeviceSize
                                               }

data Command
  = BeginRenderPass { renderPass :: VkRenderPass
                    , framebuffer :: VkFramebuffer
                    , renderArea :: VkRect2D
                    , clearValues :: [VkClearValue]
                    , contents :: VkSubpassContents
                    }
  | EndRenderPass
  | BindDescriptorSets { pipelineBindPoint :: VkPipelineBindPoint
                       , layout :: VkPipelineLayout
                       , firstSet :: Word32
                       , descriptorSets :: [VkDescriptorSet]
                       , dynamicOffsets :: [Word32]
                       }
  | BindPipeline { pipelineBindPoint :: VkPipelineBindPoint
                 , pipeline :: VkPipeline
                 }
  | BindIndexBuffer { buffer :: VkBuffer
                    , offset :: VkDeviceSize
                    , indexType :: VkIndexType
                    }
  | BindVertexBuffers { firstBinding :: Word32
                      , bindings :: [VertexBufferBinding]
                      }
  | CopyBuffer { srcBuffer :: VkBuffer
               , dstBuffer :: VkBuffer
               , regions :: [VkBufferCopy]
               }
  | Draw { vertexCount :: Word32
         , instanceCount :: Word32
         , firstVertex :: Word32
         , firstInstance :: Word32
         }
  | DrawIndexed { indexCount :: Word32
                , instanceCount :: Word32
                , firstIndex :: Word32
                , vertexOffset :: Int32
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

    BindDescriptorSets{..} ->
      withArrayLen descriptorSets $ \descriptorSetsLen descriptorSetsPtr ->
      withArrayLen dynamicOffsets $ \dynamicOffsetsLen dynamicOffsetsPtr ->
      vkCmdBindDescriptorSets buf pipelineBindPoint layout firstSet (fromIntegral descriptorSetsLen) descriptorSetsPtr (fromIntegral dynamicOffsetsLen) dynamicOffsetsPtr

    BindPipeline{..} ->
      vkCmdBindPipeline buf pipelineBindPoint pipeline

    BindVertexBuffers{..} ->
      withArrayLen ((buffer :: VertexBufferBinding -> VkBuffer) <$> bindings) $ \bindingsLen buffersPtr ->
      withArrayLen ((offset :: VertexBufferBinding -> VkDeviceSize) <$> bindings) $ \_ offsetsPtr ->
      vkCmdBindVertexBuffers buf firstBinding (fromIntegral bindingsLen) buffersPtr offsetsPtr

    BindIndexBuffer{..} ->
      vkCmdBindIndexBuffer buf buffer offset indexType

    CopyBuffer{..} ->
      withArrayLen regions $ \regionsLen regionsPtr ->
      vkCmdCopyBuffer buf srcBuffer dstBuffer (fromIntegral regionsLen) regionsPtr

    Draw{..} ->
      vkCmdDraw buf vertexCount instanceCount firstVertex firstInstance

    DrawIndexed{..} ->
      vkCmdDrawIndexed buf indexCount instanceCount firstIndex vertexOffset firstInstance

recordCommandBuffer :: MonadIO m => VkCommandBufferUsageFlags -> VkCommandBuffer -> [Command] -> m ()
recordCommandBuffer flags buf commands = liftIO $ do
  beginCommandBuffer flags buf
  mapM_ (flip record buf) commands
  endCommandBuffer buf
