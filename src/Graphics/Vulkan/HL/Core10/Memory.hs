{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Graphics.Vulkan.HL.Core10.Memory
  ( allocateMemory
  , freeMemory
  , withMemory
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

allocateMemory :: MonadIO m => VkDevice -> VkDeviceSize -> Word32 -> m VkDeviceMemory
allocateMemory dev allocationSize memoryTypeIndex = liftIO $
  with VkMemoryAllocateInfo { vkSType = VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO
                            , vkPNext = nullPtr
                            , vkAllocationSize = allocationSize
                            , vkMemoryTypeIndex = memoryTypeIndex
                            } $ \allocateInfoPtr ->
  vulkanPtrR $ vkAllocateMemory dev allocateInfoPtr nullPtr

freeMemory :: MonadIO m => VkDevice -> VkDeviceMemory -> m ()
freeMemory dev memory =
  liftIO $ vkFreeMemory dev memory nullPtr

withMemory :: MonadBaseControl IO m => VkDevice -> VkDeviceSize -> Word32 -> (VkDeviceMemory -> m a) -> m a
withMemory dev allocationSize memoryTypeIndex =
  liftBaseOp $ bracket (allocateMemory dev allocationSize memoryTypeIndex) (freeMemory dev)

mapMemory :: MonadIO m => VkDevice -> VkDeviceMemory -> VkDeviceSize -> VkDeviceSize -> m (Ptr ())
mapMemory dev memory offset size =
  liftIO . vulkanPtrR $ vkMapMemory dev memory offset size zeroBits

unmapMemory :: MonadIO m => VkDevice -> VkDeviceMemory -> m ()
unmapMemory dev memory =
  liftIO $ vkUnmapMemory dev memory

flushMappedMemoryRanges :: MonadIO m => VkDevice -> [VkMappedMemoryRange] -> m ()
flushMappedMemoryRanges dev memoryRanges = liftIO $
  withArrayLen memoryRanges $ \memoryRangesLen memoryRangesPtr ->
  guardVkResult =<< vkFlushMappedMemoryRanges dev (fromIntegral memoryRangesLen) memoryRangesPtr

invalidateMappedMemoryRanges :: MonadIO m => VkDevice -> [VkMappedMemoryRange] -> m ()
invalidateMappedMemoryRanges dev memoryRanges = liftIO $
  withArrayLen memoryRanges $ \memoryRangesLen memoryRangesPtr ->
  guardVkResult =<< vkInvalidateMappedMemoryRanges dev (fromIntegral memoryRangesLen) memoryRangesPtr

deviceMemoryCommitment :: MonadIO m => VkDevice -> VkDeviceMemory -> m VkDeviceSize
deviceMemoryCommitment dev memory =
  liftIO . vulkanPtr $ vkGetDeviceMemoryCommitment dev memory
