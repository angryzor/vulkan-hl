{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Graphics.Vulkan.HL.Core10.Shader
  ( createShaderModule
  , destroyShaderModule
  , withShaderModule
  ) where

import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Data.Bits
import qualified Data.ByteString as BS
import Data.Word
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal
import Foreign.Marshal.Array
import Graphics.Vulkan.Core10.Core
import Graphics.Vulkan.Core10.DeviceInitialization
import Graphics.Vulkan.Core10.Shader
import Graphics.Vulkan.HL.Exception
import Util

createShaderModule :: MonadIO m => VkDevice -> BS.ByteString -> m VkShaderModule
createShaderModule dev code = liftIO $
  BS.useAsCString code $ \codePtr ->
  with VkShaderModuleCreateInfo { vkSType = VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO
                                , vkPNext = nullPtr
                                , vkFlags = zeroBits
                                , vkCodeSize = fromIntegral $ BS.length code
                                , vkPCode = castPtr codePtr
                                } $ \smci ->
  vulkanPtrR $ vkCreateShaderModule dev smci nullPtr

destroyShaderModule :: MonadIO m => VkDevice -> VkShaderModule -> m ()
destroyShaderModule dev mod =
  liftIO $ vkDestroyShaderModule dev mod nullPtr

withShaderModule :: MonadBaseControl IO m => VkDevice -> BS.ByteString -> (VkShaderModule -> m a) -> m a
withShaderModule dev code =
  liftBaseOp $ bracket (createShaderModule dev code) (destroyShaderModule dev)
