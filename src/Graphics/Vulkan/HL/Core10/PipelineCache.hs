{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}

module Graphics.Vulkan.HL.Core10.PipelineCache
  ( createPipelineCache
  , destroyPipelineCache
  , withPipelineCache
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
import Graphics.Vulkan.Core10.Pipeline
import Graphics.Vulkan.Core10.PipelineCache
import Graphics.Vulkan.HL.Exception
import Util

createPipelineCache :: MonadIO m => VkDevice -> Maybe BS.ByteString -> m VkPipelineCache
createPipelineCache dev maybeData = liftIO $
  withCreateInfo maybeData $ \createInfoPtr ->
  vulkanPtrR $ vkCreatePipelineCache dev createInfoPtr nullPtr
  where
    withCreateInfo Nothing next =
      with VkPipelineCacheCreateInfo { vkSType = VK_STRUCTURE_TYPE_PIPELINE_CACHE_CREATE_INFO
                                     , vkPNext = nullPtr
                                     , vkFlags = zeroBits
                                     , vkInitialDataSize = 0
                                     , vkPInitialData = nullPtr
                                     } next
    withCreateInfo (Just dataStr) next =
      BS.useAsCString dataStr $ \dataStrPtr ->
      with VkPipelineCacheCreateInfo { vkSType = VK_STRUCTURE_TYPE_PIPELINE_CACHE_CREATE_INFO
                                     , vkPNext = nullPtr
                                     , vkFlags = zeroBits
                                     , vkInitialDataSize = fromIntegral $ BS.length dataStr
                                     , vkPInitialData = castPtr dataStrPtr
                                     } next


destroyPipelineCache :: MonadIO m => VkDevice -> VkPipelineCache -> m ()
destroyPipelineCache dev pipelineCache =
  liftIO $ vkDestroyPipelineCache dev pipelineCache nullPtr

withPipelineCache :: MonadBaseControl IO m => VkDevice -> Maybe BS.ByteString -> (VkPipelineCache -> m a) -> m a
withPipelineCache dev maybeData =
  liftBaseOp $ bracket (createPipelineCache dev maybeData) (destroyPipelineCache dev)
