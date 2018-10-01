{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Graphics.Vulkan.HL.Core10.Image
  ( ImageCreateInfo (..)
  , createImage
  , destroyImage
  , withImage
  , getImageSubresourceLayout
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
import Graphics.Vulkan.Core10.Buffer
import Graphics.Vulkan.Core10.Core
import Graphics.Vulkan.Core10.DeviceInitialization
import Graphics.Vulkan.Core10.Image
import Graphics.Vulkan.Core10.MemoryManagement
import Graphics.Vulkan.Core10.SparseResourceMemoryManagement
import Graphics.Vulkan.HL.Exception
import Util

data ImageCreateInfo = ImageCreateInfo { flags :: VkImageCreateFlags
                                       , imageType :: VkImageType
                                       , format :: VkFormat
                                       , extent :: VkExtent3D
                                       , mipLevels :: Word32
                                       , arrayLayers :: Word32
                                       , samples :: VkSampleCountFlagBits
                                       , tiling :: VkImageTiling
                                       , usage :: VkImageUsageFlags
                                       , sharingMode :: VkSharingMode
                                       , queueFamilyIndices :: [Word32]
                                       , initialLayout :: VkImageLayout
                                       }

createImage :: MonadIO m => VkDevice -> ImageCreateInfo -> m VkImage
createImage dev ImageCreateInfo{..} = liftIO $
  withArrayLen queueFamilyIndices $ \queueFamilyIndicesLen queueFamilyIndicesPtr ->
  with VkImageCreateInfo { vkSType = VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO
                         , vkPNext = nullPtr
                         , vkFlags = flags
                         , vkImageType = imageType
                         , vkFormat = format
                         , vkExtent = extent
                         , vkMipLevels = mipLevels
                         , vkArrayLayers = arrayLayers
                         , vkSamples = samples
                         , vkTiling = tiling
                         , vkUsage = usage
                         , vkSharingMode = sharingMode
                         , vkQueueFamilyIndexCount = fromIntegral queueFamilyIndicesLen
                         , vkPQueueFamilyIndices = queueFamilyIndicesPtr
                         , vkInitialLayout = initialLayout
                         } $ \createInfoPtr ->
  vulkanPtrR $ vkCreateImage dev createInfoPtr nullPtr

destroyImage :: MonadIO m => VkDevice -> VkImage -> m ()
destroyImage dev image =
  liftIO $ vkDestroyImage dev image nullPtr

withImage :: MonadBaseControl IO m => VkDevice -> ImageCreateInfo -> (VkImage -> m a) -> m a
withImage dev info =
  liftBaseOp $ bracket (createImage dev info) (destroyImage dev)

getImageSubresourceLayout :: MonadIO m => VkDevice -> VkImage -> VkImageAspectFlags -> Word32 -> Word32 -> m VkSubresourceLayout
getImageSubresourceLayout dev image aspectMask mipLevel arrayLayer = liftIO $
  with VkImageSubresource { vkAspectMask = aspectMask
                          , vkMipLevel = mipLevel
                          , vkArrayLayer = arrayLayer
                          } $ \imageSubresourcePtr ->
  vulkanPtr $ vkGetImageSubresourceLayout dev image imageSubresourcePtr
