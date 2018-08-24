{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Graphics.Vulkan.HL.Extensions.VK_KHR_swapchain
  ( SwapchainCreateInfo (..)
  , createSwapchain
  , destroySwapchain
  , withSwapchain
  , getSwapchainImages
  , acquireNextImage
  , queuePresent
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
import Graphics.Vulkan.Core10.MemoryManagement
import Graphics.Vulkan.Core10.Pipeline
import Graphics.Vulkan.Core10.Queue
import Graphics.Vulkan.Extensions.VK_KHR_surface
import Graphics.Vulkan.Extensions.VK_KHR_swapchain
import Graphics.Vulkan.HL.Exception
import Util

data SwapchainCreateInfo = SwapchainCreateInfo { flags :: VkSwapchainCreateFlagsKHR
                                               , minImageCount :: Word32
                                               , imageFormat :: VkFormat
                                               , imageColorSpace :: VkColorSpaceKHR
                                               , imageExtent :: VkExtent2D
                                               , imageArrayLayers :: Word32
                                               , imageUsage :: VkImageUsageFlags
                                               , imageSharingMode :: VkSharingMode
                                               , queueFamilyIndices :: [Word32]
                                               , preTransform :: VkSurfaceTransformFlagBitsKHR
                                               , compositeAlpha :: VkCompositeAlphaFlagBitsKHR
                                               , presentMode :: VkPresentModeKHR
                                               , clipped :: VkBool32
                                               , oldSwapChain :: VkSwapchainKHR
                                               }

createSwapchain :: MonadIO m => SwapchainCreateInfo -> VkSurfaceKHR -> VkDevice -> m VkSwapchainKHR
createSwapchain SwapchainCreateInfo{..} surface dev = liftIO $
  withArrayLen queueFamilyIndices $ \queueFamilyIndicesLen queueFamilyIndicesPtr ->
  with VkSwapchainCreateInfoKHR { vkSType = VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR
                                , vkPNext = nullPtr
                                , vkFlags = flags
                                , vkSurface = surface
                                , vkMinImageCount = minImageCount
                                , vkImageFormat = imageFormat
                                , vkImageColorSpace = imageColorSpace
                                , vkImageExtent = imageExtent
                                , vkImageArrayLayers = imageArrayLayers
                                , vkImageUsage = imageUsage
                                , vkImageSharingMode = imageSharingMode
                                , vkQueueFamilyIndexCount = fromIntegral queueFamilyIndicesLen
                                , vkPQueueFamilyIndices = queueFamilyIndicesPtr
                                , vkPreTransform = preTransform
                                , vkCompositeAlpha = compositeAlpha
                                , vkPresentMode = presentMode
                                , vkClipped = clipped
                                , vkOldSwapchain = oldSwapChain
                                } $ \sciPtr ->
  vulkanPtrR $ vkCreateSwapchainKHR dev sciPtr nullPtr

destroySwapchain :: MonadIO m => VkDevice -> VkSwapchainKHR -> m ()
destroySwapchain dev swapchain =
  liftIO $ vkDestroySwapchainKHR dev swapchain nullPtr

withSwapchain :: MonadBaseControl IO m => SwapchainCreateInfo -> VkSurfaceKHR -> VkDevice -> (VkSwapchainKHR -> m a) -> m a
withSwapchain sci surface dev =
  liftBaseOp (bracket (createSwapchain sci surface dev) (destroySwapchain dev))

getSwapchainImages :: MonadIO m => VkDevice -> VkSwapchainKHR -> m [VkImage]
getSwapchainImages dev swapchain =
  liftIO . vulkanEnumerationR $ vkGetSwapchainImagesKHR dev swapchain

acquireNextImage :: MonadIO m => VkDevice -> VkSwapchainKHR -> Word64 -> VkSemaphore -> VkFence -> m Word32
acquireNextImage dev swapchain timeout semaphore fence =
  liftIO . vulkanPtrR $ vkAcquireNextImageKHR dev swapchain timeout semaphore fence

queuePresent :: MonadIO m => [VkSemaphore] -> [(VkSwapchainKHR, Word32)] -> VkQueue -> m ()
queuePresent waitSemaphores swapchainImages queue = liftIO $
  withArrayLen waitSemaphores $ \waitSemaphoresLen waitSemaphoresPtr ->
  withArrayLen (fst <$> swapchainImages) $ \swapchainsLen swapchainsPtr ->
  withArrayLen (snd <$> swapchainImages) $ \imagesLen imagesPtr ->
  with VkPresentInfoKHR { vkSType = VK_STRUCTURE_TYPE_PRESENT_INFO_KHR
                        , vkPNext = nullPtr
                        , vkWaitSemaphoreCount = fromIntegral waitSemaphoresLen
                        , vkPWaitSemaphores = waitSemaphoresPtr
                        , vkSwapchainCount = fromIntegral swapchainsLen
                        , vkPSwapchains = swapchainsPtr
                        , vkPImageIndices = imagesPtr
                        , vkPResults = nullPtr -- TODO: Retrieve VK_RESULTs
                        } $ \presentInfoPtr ->
  guardVkResult =<< vkQueuePresentKHR queue presentInfoPtr

-- TODO
-- getDeviceGroupPresentCapabilities :: MonadIO m => VkDevice -> m
-- getDeviceGroupSurfacePresentModesKHR
-- acquireNextImage2KHR
-- getPhysicalDevicePresentRectanglesKHR
