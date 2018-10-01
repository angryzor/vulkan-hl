{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Graphics.Vulkan.HL.Core10.Sampler
  ( SamplerCreateInfo (..)
  , createSampler
  , destroySampler
  , withSampler
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
import Graphics.Vulkan.Core10.Sampler
import Graphics.Vulkan.HL.Exception
import Util

data SamplerCreateInfo = SamplerCreateInfo { flags :: VkSamplerCreateFlags
                                           , magFilter :: VkFilter
                                           , minFilter :: VkFilter
                                           , mipmapMode :: VkSamplerMipmapMode
                                           , addressModeU :: VkSamplerAddressMode
                                           , addressModeV :: VkSamplerAddressMode
                                           , addressModeW :: VkSamplerAddressMode
                                           , mipLodBias :: Float
                                           , anisotropyEnable :: VkBool32
                                           , maxAnisotropy :: Float
                                           , compareEnable :: VkBool32
                                           , compareOp :: VkCompareOp
                                           , minLod :: Float
                                           , maxLod :: Float
                                           , borderColor :: VkBorderColor
                                           , unnormalizedCoordinates :: VkBool32
                                           }

createSampler :: MonadIO m => VkDevice -> SamplerCreateInfo -> m VkSampler
createSampler dev SamplerCreateInfo{..} = liftIO $
  with VkSamplerCreateInfo { vkSType = VK_STRUCTURE_TYPE_SAMPLER_CREATE_INFO
                           , vkPNext = nullPtr
                           , vkFlags = flags
                           , vkMagFilter = magFilter
                           , vkMinFilter = minFilter
                           , vkMipmapMode = mipmapMode
                           , vkAddressModeU = addressModeU
                           , vkAddressModeV = addressModeV
                           , vkAddressModeW = addressModeW
                           , vkMipLodBias = CFloat mipLodBias
                           , vkAnisotropyEnable = anisotropyEnable
                           , vkMaxAnisotropy = CFloat maxAnisotropy
                           , vkCompareEnable = compareEnable
                           , vkCompareOp = compareOp
                           , vkMinLod = CFloat minLod
                           , vkMaxLod = CFloat maxLod
                           , vkBorderColor = borderColor
                           , vkUnnormalizedCoordinates = unnormalizedCoordinates
                           } $ \createInfoPtr ->
  vulkanPtrR $ vkCreateSampler dev createInfoPtr nullPtr

destroySampler :: MonadIO m => VkDevice -> VkSampler -> m ()
destroySampler dev sampler =
  liftIO $ vkDestroySampler dev sampler nullPtr

withSampler :: MonadBaseControl IO m => VkDevice -> SamplerCreateInfo -> (VkSampler -> m a) -> m a
withSampler dev info =
  liftBaseOp $ bracket (createSampler dev info) (destroySampler dev)
