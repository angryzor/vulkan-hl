{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Graphics.Vulkan.HL.Core10.ImageView
  ( createImageView
  , destroyImageView
  , withImageView
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
import Graphics.Vulkan.Core10.MemoryManagement
import Graphics.Vulkan.Core10.ImageView
import Graphics.Vulkan.HL.Exception
import Util

createImageView :: MonadIO m => VkDevice -> VkImageViewType -> VkFormat -> VkComponentMapping -> VkImageSubresourceRange -> VkImage -> m VkImageView
createImageView dev viewType format components subresourceRange image = liftIO $
  with VkImageViewCreateInfo { vkSType = VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO
                             , vkPNext = nullPtr
                             , vkFlags = zeroBits
                             , vkImage = image
                             , vkViewType = viewType
                             , vkFormat = format
                             , vkComponents = components
                             , vkSubresourceRange = subresourceRange
                             } $ \ivci ->
  vulkanPtrR $ vkCreateImageView dev ivci nullPtr

destroyImageView :: MonadIO m => VkDevice -> VkImageView -> m ()
destroyImageView dev imageView =
  liftIO $ vkDestroyImageView dev imageView nullPtr

withImageView :: MonadBaseControl IO m => VkDevice -> VkImageViewType -> VkFormat -> VkComponentMapping -> VkImageSubresourceRange -> VkImage -> (VkImageView -> m a) -> m a
withImageView dev viewType format components subresourceRange image =
  liftBaseOp $ bracket (createImageView dev viewType format components subresourceRange image) (destroyImageView dev)
