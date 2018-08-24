{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Graphics.Vulkan.HL.Core10.Queue
  ( SubmitInfo (..)
  , getDeviceQueue
  , queueSubmit
  , queueWaitIdle
  , deviceWaitIdle
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Tuple
import Data.Word
import Foreign.Ptr
import Foreign.Marshal.Array
import Graphics.Vulkan.Core10.Core
import Graphics.Vulkan.Core10.DeviceInitialization
import Graphics.Vulkan.Core10.Queue
import Graphics.Vulkan.HL.Exception
import Util

data SubmitInfo = SubmitInfo { waitSemaphores :: [(VkSemaphore, VkPipelineStageFlags)]
                             , commandBuffers :: [VkCommandBuffer]
                             , signalSemaphores :: [VkSemaphore]
                             }

convertSubmit :: SubmitInfo -> IO VkSubmitInfo
convertSubmit SubmitInfo{..} =
  withArrayLen (fst <$> waitSemaphores) $ \waitSemaphoresLen waitSemaphoresPtr ->
  withArrayLen (snd <$> waitSemaphores) $ \waitDstStageMasksLen waitDstStageMasksPtr ->
  withArrayLen commandBuffers $ \commandBuffersLen commandBuffersPtr ->
  withArrayLen signalSemaphores $ \signalSemaphoresLen signalSemaphoresPtr ->
  return $ VkSubmitInfo { vkSType = VK_STRUCTURE_TYPE_SUBMIT_INFO
                        , vkPNext = nullPtr
                        , vkWaitSemaphoreCount = fromIntegral waitSemaphoresLen
                        , vkPWaitSemaphores = waitSemaphoresPtr
                        , vkPWaitDstStageMask = waitDstStageMasksPtr
                        , vkCommandBufferCount = fromIntegral commandBuffersLen
                        , vkPCommandBuffers = commandBuffersPtr
                        , vkSignalSemaphoreCount = fromIntegral signalSemaphoresLen
                        , vkPSignalSemaphores = signalSemaphoresPtr
                        }

getDeviceQueue :: MonadIO m => Word32 -> Word32 -> VkDevice -> m VkQueue
getDeviceQueue queueFamilyIndex queueIndex dev =
  liftIO . vulkanPtr $ vkGetDeviceQueue dev queueFamilyIndex queueIndex

queueSubmit :: MonadIO m => [SubmitInfo] -> VkFence -> VkQueue -> m ()
queueSubmit submits fence queue = liftIO $ do
  convertedSubmits <- traverse convertSubmit submits
  withArrayLen convertedSubmits $ \submitsLen submitsPtr ->
    guardVkResult =<< vkQueueSubmit queue (fromIntegral submitsLen) submitsPtr fence

queueWaitIdle :: MonadIO m => VkQueue -> m ()
queueWaitIdle =
  liftIO . guardVkResult <=< liftIO . vkQueueWaitIdle

deviceWaitIdle :: MonadIO m => VkDevice -> m ()
deviceWaitIdle =
  liftIO . guardVkResult <=< liftIO . vkDeviceWaitIdle

