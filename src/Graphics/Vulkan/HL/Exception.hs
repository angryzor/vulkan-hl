module Graphics.Vulkan.HL.Exception where

import Control.Exception
import Data.Typeable
import Graphics.Vulkan.Core10.Core

data VulkanException = VulkanException VkResult deriving (Show, Typeable)

instance Exception VulkanException

guardVkResult :: VkResult -> IO ()
guardVkResult VK_SUCCESS = return ()
guardVkResult result = throwIO $ VulkanException result
