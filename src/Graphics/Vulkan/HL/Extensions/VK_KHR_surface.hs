{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Graphics.Vulkan.HL.Extensions.VK_KHR_surface
  ( destroySurface
  , getPhysicalDeviceSurfaceSupport
  , getPhysicalDeviceSurfaceCapabilities
  , getPhysicalDeviceSurfaceFormats
  , getPhysicalDeviceSurfacePresentModes
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
import Graphics.Vulkan.Extensions.VK_KHR_surface
import Graphics.Vulkan.HL.Exception
import Util

destroySurface :: MonadIO m => VkInstance -> VkSurfaceKHR -> m ()
destroySurface inst dev =
  liftIO $ vkDestroySurfaceKHR inst dev nullPtr

getPhysicalDeviceSurfaceSupport :: MonadIO m => VkSurfaceKHR -> VkPhysicalDevice -> Word32 -> m VkBool32
getPhysicalDeviceSurfaceSupport surface dev queueIndex =
  liftIO . vulkanPtrR $ vkGetPhysicalDeviceSurfaceSupportKHR dev queueIndex surface

getPhysicalDeviceSurfaceCapabilities :: MonadIO m => VkSurfaceKHR -> VkPhysicalDevice -> m VkSurfaceCapabilitiesKHR
getPhysicalDeviceSurfaceCapabilities surface dev =
  liftIO . vulkanPtrR $ vkGetPhysicalDeviceSurfaceCapabilitiesKHR dev surface

getPhysicalDeviceSurfaceFormats :: MonadIO m => VkSurfaceKHR -> VkPhysicalDevice -> m [VkSurfaceFormatKHR]
getPhysicalDeviceSurfaceFormats surface dev =
  liftIO . vulkanEnumerationR $ vkGetPhysicalDeviceSurfaceFormatsKHR dev surface

getPhysicalDeviceSurfacePresentModes :: MonadIO m => VkSurfaceKHR -> VkPhysicalDevice -> m [VkPresentModeKHR]
getPhysicalDeviceSurfacePresentModes surface dev =
  liftIO . vulkanEnumerationR $ vkGetPhysicalDeviceSurfacePresentModesKHR dev surface
