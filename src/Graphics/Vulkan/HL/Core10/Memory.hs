{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Graphics.Vulkan.HL.Core10.Memory
  ( allocateMemory
  , freeMemory
  , withMemory
  , mapMemory
  , unmapMemory
  , mappingMemory
  , flushMappedMemoryRanges
  , invalidateMappedMemoryRanges
  , getDeviceMemoryCommitment
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
import Graphics.Vulkan.HL.Exception
import Util

allocateMemory :: MonadIO m => VkDevice -> Word32 -> VkDeviceSize -> m VkDeviceMemory
allocateMemory dev memoryTypeIndex allocationSize = liftIO $
  with VkMemoryAllocateInfo { vkSType = VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO
                            , vkPNext = nullPtr
                            , vkAllocationSize = allocationSize
                            , vkMemoryTypeIndex = memoryTypeIndex
                            } $ \allocateInfoPtr ->
  vulkanPtrR $ vkAllocateMemory dev allocateInfoPtr nullPtr

freeMemory :: MonadIO m => VkDevice -> VkDeviceMemory -> m ()
freeMemory dev memory =
  liftIO $ vkFreeMemory dev memory nullPtr

withMemory :: MonadBaseControl IO m => VkDevice -> Word32 -> VkDeviceSize -> (VkDeviceMemory -> m a) -> m a
withMemory dev memoryTypeIndex allocationSize =
  liftBaseOp $ bracket (allocateMemory dev memoryTypeIndex allocationSize) (freeMemory dev)

mapMemory :: MonadIO m => VkDevice -> VkDeviceMemory -> VkDeviceSize -> VkDeviceSize -> m (Ptr d)
mapMemory dev memory offset size =
  liftIO . fmap castPtr . vulkanPtrR $ vkMapMemory dev memory offset size zeroBits

unmapMemory :: MonadIO m => VkDevice -> VkDeviceMemory -> m ()
unmapMemory dev memory =
  liftIO $ vkUnmapMemory dev memory

mappingMemory :: MonadBaseControl IO m => VkDevice -> VkDeviceMemory -> VkDeviceSize -> VkDeviceSize -> (Ptr d -> m a) -> m a
mappingMemory dev memory offset size =
  liftBaseOp $ bracket (mapMemory dev memory offset size) (const $ unmapMemory dev memory)

flushMappedMemoryRanges :: MonadIO m => VkDevice -> [VkMappedMemoryRange] -> m ()
flushMappedMemoryRanges dev memoryRanges = liftIO $
  withArrayLen memoryRanges $ \memoryRangesLen memoryRangesPtr ->
  guardVkResult =<< vkFlushMappedMemoryRanges dev (fromIntegral memoryRangesLen) memoryRangesPtr

invalidateMappedMemoryRanges :: MonadIO m => VkDevice -> [VkMappedMemoryRange] -> m ()
invalidateMappedMemoryRanges dev memoryRanges = liftIO $
  withArrayLen memoryRanges $ \memoryRangesLen memoryRangesPtr ->
  guardVkResult =<< vkInvalidateMappedMemoryRanges dev (fromIntegral memoryRangesLen) memoryRangesPtr

getDeviceMemoryCommitment :: MonadIO m => VkDevice -> VkDeviceMemory -> m VkDeviceSize
getDeviceMemoryCommitment dev memory =
  liftIO . vulkanPtr $ vkGetDeviceMemoryCommitment dev memory
