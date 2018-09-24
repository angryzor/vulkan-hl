{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Graphics.Vulkan.HL.Core10.DescriptorSet
  ( DescriptorSetLayoutBinding (..)
  , createDescriptorSetLayout
  , destroyDescriptorSetLayout
  , withDescriptorSetLayout
  , createDescriptorPool
  , destroyDescriptorPool
  , withDescriptorPool
  , resetDescriptorPool
  , allocateDescriptorSets
  , freeDescriptorSets
  , withDescriptorSets
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
import Graphics.Vulkan.Core10.DescriptorSet
import Graphics.Vulkan.Core10.PipelineLayout
import Graphics.Vulkan.Core10.Sampler
import Graphics.Vulkan.HL.Exception
import Util

data DescriptorSetLayoutBinding = DescriptorSetLayoutBinding { binding :: Word32
                                                             , descriptorType :: VkDescriptorType
                                                             , descriptorCount :: Word32
                                                             , stageFlags :: VkShaderStageFlags
                                                             , immutableSamplers :: [VkSampler]
                                                             }

convertDescriptorSetLayoutBinding :: DescriptorSetLayoutBinding -> IO VkDescriptorSetLayoutBinding
convertDescriptorSetLayoutBinding DescriptorSetLayoutBinding{..} =
  withArray immutableSamplers $ \immutableSamplersPtr ->
  return $ VkDescriptorSetLayoutBinding { vkBinding = binding
                                        , vkDescriptorType = descriptorType
                                        , vkDescriptorCount = descriptorCount
                                        , vkStageFlags = stageFlags
                                        , vkPImmutableSamplers = immutableSamplersPtr
                                        }

createDescriptorSetLayout :: MonadIO m => VkDevice -> VkDescriptorSetLayoutCreateFlags -> [DescriptorSetLayoutBinding] -> m VkDescriptorSetLayout
createDescriptorSetLayout dev flags bindings = liftIO $ do
  convertedDescriptorSetLayoutBindings <- traverse convertDescriptorSetLayoutBinding bindings
  withArrayLen convertedDescriptorSetLayoutBindings $ \convertedDescriptorSetLayoutBindingsLen convertedDescriptorSetLayoutBindingsPtr ->
    with VkDescriptorSetLayoutCreateInfo { vkSType = VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO
                                         , vkPNext = nullPtr
                                         , vkFlags = flags
                                         , vkBindingCount = fromIntegral convertedDescriptorSetLayoutBindingsLen
                                         , vkPBindings = convertedDescriptorSetLayoutBindingsPtr
                                         } $ \createInfoPtr ->
    vulkanPtrR $ vkCreateDescriptorSetLayout dev createInfoPtr nullPtr

destroyDescriptorSetLayout :: MonadIO m => VkDevice -> VkDescriptorSetLayout -> m ()
destroyDescriptorSetLayout dev descriptorSetLayout =
  liftIO $ vkDestroyDescriptorSetLayout dev descriptorSetLayout nullPtr

withDescriptorSetLayout :: MonadBaseControl IO m => VkDevice -> VkDescriptorSetLayoutCreateFlags -> [DescriptorSetLayoutBinding] -> (VkDescriptorSetLayout -> m a) -> m a
withDescriptorSetLayout dev flags bindings =
  liftBaseOp $ bracket (createDescriptorSetLayout dev flags bindings) (destroyDescriptorSetLayout dev)

createDescriptorPool :: MonadIO m => VkDevice -> VkDescriptorPoolCreateFlags -> Word32 -> [VkDescriptorPoolSize] -> m VkDescriptorPool
createDescriptorPool dev flags maxSets poolSizes = liftIO $
  withArrayLen poolSizes $ \poolSizesLen poolSizesPtr ->
  with VkDescriptorPoolCreateInfo { vkSType = VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO
                                  , vkPNext = nullPtr
                                  , vkFlags = flags
                                  , vkMaxSets = maxSets
                                  , vkPoolSizeCount = fromIntegral poolSizesLen
                                  , vkPPoolSizes = poolSizesPtr
                                  } $ \createInfoPtr ->
  vulkanPtrR $ vkCreateDescriptorPool dev createInfoPtr nullPtr

destroyDescriptorPool :: MonadIO m => VkDevice -> VkDescriptorPool -> m ()
destroyDescriptorPool dev descriptorPool =
  liftIO $ vkDestroyDescriptorPool dev descriptorPool nullPtr

withDescriptorPool :: MonadBaseControl IO m => VkDevice -> VkDescriptorPoolCreateFlags -> Word32 -> [VkDescriptorPoolSize] -> (VkDescriptorPool -> m a) -> m a
withDescriptorPool dev flags maxSets poolSizes =
  liftBaseOp $ bracket (createDescriptorPool dev flags maxSets poolSizes) (destroyDescriptorPool dev)

resetDescriptorPool :: MonadIO m => VkDevice -> VkDescriptorPool -> VkDescriptorPoolResetFlags -> m ()
resetDescriptorPool dev descriptorPool flags =
  liftIO $ guardVkResult =<< vkResetDescriptorPool dev descriptorPool flags

allocateDescriptorSets :: MonadIO m => VkDevice -> VkDescriptorPool -> [VkDescriptorSetLayout] -> m [VkDescriptorSet]
allocateDescriptorSets dev descriptorPool setLayouts = liftIO $
  withArrayLen setLayouts $ \setLayoutsLen setLayoutsPtr ->
  with VkDescriptorSetAllocateInfo { vkSType = VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO
                                  , vkPNext = nullPtr
                                  , vkDescriptorPool = descriptorPool
                                  , vkDescriptorSetCount = fromIntegral setLayoutsLen
                                  , vkPSetLayouts = setLayoutsPtr
                                  } $ \createInfoPtr ->
  vulkanArrayR setLayoutsLen $ vkAllocateDescriptorSets dev createInfoPtr

freeDescriptorSets :: MonadIO m => VkDevice -> VkDescriptorPool -> [VkDescriptorSet] -> m ()
freeDescriptorSets dev descriptorPool descriptorSets = liftIO $
  withArrayLen descriptorSets $ \descriptorSetsLen descriptorSetsPtr ->
  guardVkResult =<< vkFreeDescriptorSets dev descriptorPool (fromIntegral descriptorSetsLen) descriptorSetsPtr

withDescriptorSets :: MonadBaseControl IO m => VkDevice -> VkDescriptorPool -> [VkDescriptorSetLayout] -> ([VkDescriptorSet] -> m a) -> m a
withDescriptorSets dev descriptorPool setLayouts =
  liftBaseOp $ bracket (allocateDescriptorSets dev descriptorPool setLayouts) (freeDescriptorSets dev descriptorPool)

-- FIXME: Todo: updateDescriptorSets
