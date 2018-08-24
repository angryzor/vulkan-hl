{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Graphics.Vulkan.HL.Core10.PipelineLayout
  ( createPipelineLayout
  , destroyPipelineLayout
  , withPipelineLayout
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
import Graphics.Vulkan.Core10.Pipeline
import Graphics.Vulkan.Core10.PipelineLayout
import Graphics.Vulkan.HL.Exception
import Util

createPipelineLayout :: MonadIO m => VkDevice -> [VkDescriptorSetLayout] -> [VkPushConstantRange] -> m VkPipelineLayout
createPipelineLayout dev setLayouts pushConstantRanges = liftIO $
  withArrayLen setLayouts $ \setLayoutsLen setLayoutsPtr ->
  withArrayLen pushConstantRanges $ \pushConstantRangesLen pushConstantRangesPtr ->
  with VkPipelineLayoutCreateInfo { vkSType = VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO
                                  , vkPNext = nullPtr
                                  , vkFlags = zeroBits
                                  , vkSetLayoutCount = fromIntegral setLayoutsLen
                                  , vkPSetLayouts = setLayoutsPtr
                                  , vkPushConstantRangeCount = fromIntegral pushConstantRangesLen
                                  , vkPPushConstantRanges = pushConstantRangesPtr
                                  } $ \plci ->
  vulkanPtrR $ vkCreatePipelineLayout dev plci nullPtr

destroyPipelineLayout :: MonadIO m => VkDevice -> VkPipelineLayout -> m ()
destroyPipelineLayout dev layout =
  liftIO $ vkDestroyPipelineLayout dev layout nullPtr

withPipelineLayout :: MonadBaseControl IO m => VkDevice -> [VkDescriptorSetLayout] -> [VkPushConstantRange] -> (VkPipelineLayout -> m a) -> m a
withPipelineLayout dev setLayouts pushConstantRanges =
  liftBaseOp $ bracket (createPipelineLayout dev setLayouts pushConstantRanges) (destroyPipelineLayout dev)
