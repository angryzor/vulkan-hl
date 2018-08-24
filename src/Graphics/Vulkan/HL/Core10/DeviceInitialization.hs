{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Graphics.Vulkan.HL.Core10.DeviceInitialization
  ( createInstance
  , destroyInstance
  , withInstance
  , PhysicalDeviceProperties (..)
  , PhysicalDeviceMemoryProperties (..)
  , enumeratePhysicalDevices
  , getPhysicalDeviceProperties
  , getPhysicalDeviceFeatures
  , getPhysicalDeviceQueueFamilyProperties
  , getPhysicalDeviceMemoryProperties
  ) where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Data.Bits
import qualified Data.ByteString.Lazy as BSL
import Data.UUID
import qualified Data.Vector.Storable.Sized as SV
import Data.Word
import Foreign.Ptr
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import Graphics.Vulkan.Core10.Core
import Graphics.Vulkan.Core10.DeviceInitialization
import Graphics.Vulkan.Core10.Version
import Graphics.Vulkan.HL.Exception
import Util

-- data InstanceCreateInfo where
--   InstanceCreateInfo :: (Foldable f1, Foldable f2) =>
--     { flags :: VkInstanceCreateFlags
--     -- , pApplicationInfo :: ?
--     , enabledLayers :: f1 String
--     , enabledExtensions :: f2 String
--     } -> InstanceCreateInfo

createInstance :: MonadIO m => String -> Word32 -> String -> Word32 -> [String] -> [String] -> m VkInstance
createInstance appName appVersion engineName engineVersion layers extensions = liftIO $
  withCString appName $ \appNamePtr ->
  withCString engineName $ \engineNamePtr ->
  withCStringArrayLen extensions $ \extensionCount extensionsPtr ->
  with VkApplicationInfo { vkSType = VK_STRUCTURE_TYPE_APPLICATION_INFO
                         , vkPNext = nullPtr
                         , vkPApplicationName = appNamePtr
                         , vkApplicationVersion = appVersion
                         , vkPEngineName = engineNamePtr
                         , vkEngineVersion = engineVersion
                         , vkApiVersion = VK_API_VERSION_1_1
                         } $ \appInfoPtr ->
  withCStringArrayLen layers $ \layerCount layerNamesPtr ->
  with VkInstanceCreateInfo { vkSType = VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO
                            , vkPNext = nullPtr
                            , vkFlags = zeroBits
                            , vkPApplicationInfo = appInfoPtr
                            , vkEnabledLayerCount = fromIntegral layerCount
                            , vkPPEnabledLayerNames = layerNamesPtr
                            , vkEnabledExtensionCount = fromIntegral extensionCount
                            , vkPPEnabledExtensionNames = extensionsPtr
                            } $ \instanceCreateInfoPtr ->
  vulkanPtrR $ vkCreateInstance instanceCreateInfoPtr nullPtr

destroyInstance :: MonadIO m => VkInstance -> m ()
destroyInstance inst =
  liftIO $ vkDestroyInstance inst nullPtr

withInstance :: MonadBaseControl IO m => String -> Word32 -> String -> Word32 -> [String] -> [String] -> (VkInstance -> m a) -> m a
withInstance appName appVersion engineName engineVersion layers extensions =
  liftBaseOp $ bracket (createInstance appName appVersion engineName engineVersion layers extensions) destroyInstance


--
-- Devices
--

data PhysicalDeviceProperties = PhysicalDeviceProperties { apiVersion :: Word32
                                                         , driverVersion :: Word32
                                                         , vendorID :: Word32
                                                         , deviceID :: Word32
                                                         , deviceType :: VkPhysicalDeviceType
                                                         , deviceName :: String
                                                         , pipelineCacheUUID :: UUID
                                                         , limits :: VkPhysicalDeviceLimits
                                                         , sparseProperties :: VkPhysicalDeviceSparseProperties
                                                         } deriving (Show)

data PhysicalDeviceMemoryProperties = PhysicalDeviceMemoryProperties { memoryTypes :: [VkMemoryType]
                                                                     , memoryHeaps :: [VkMemoryHeap]
                                                                     } deriving (Show)

convertPhysicalDeviceProperties :: VkPhysicalDeviceProperties -> IO PhysicalDeviceProperties
convertPhysicalDeviceProperties VkPhysicalDeviceProperties{..} = do
  let Just pipelineCacheUUID = fromByteString . BSL.pack . SV.toList $ vkPipelineCacheUUID
  deviceName <- sizedVectorToString vkDeviceName
  return PhysicalDeviceProperties { apiVersion = vkApiVersion
                                  , driverVersion = vkDriverVersion
                                  , vendorID = vkVendorID
                                  , deviceID = vkDeviceID
                                  , deviceType = vkDeviceType
                                  , deviceName = deviceName
                                  , pipelineCacheUUID = pipelineCacheUUID
                                  , limits = vkLimits
                                  , sparseProperties = vkSparseProperties
                                  }

convertPhysicalDeviceMemoryProperties :: VkPhysicalDeviceMemoryProperties -> PhysicalDeviceMemoryProperties
convertPhysicalDeviceMemoryProperties VkPhysicalDeviceMemoryProperties{..} =
  PhysicalDeviceMemoryProperties { memoryTypes = sizedVectorToList vkMemoryTypeCount vkMemoryTypes
                                 , memoryHeaps = sizedVectorToList vkMemoryHeapCount vkMemoryHeaps
                                 }

enumeratePhysicalDevices :: MonadIO m => VkInstance -> m [VkPhysicalDevice]
enumeratePhysicalDevices =
  liftIO . vulkanEnumerationR . vkEnumeratePhysicalDevices

getPhysicalDeviceProperties :: MonadIO m => VkPhysicalDevice -> m PhysicalDeviceProperties
getPhysicalDeviceProperties =
  liftIO . convertPhysicalDeviceProperties <=< liftIO . vulkanPtr . vkGetPhysicalDeviceProperties

getPhysicalDeviceQueueFamilyProperties :: MonadIO m => VkPhysicalDevice -> m [VkQueueFamilyProperties]
getPhysicalDeviceQueueFamilyProperties =
  liftIO . vulkanEnumeration . vkGetPhysicalDeviceQueueFamilyProperties

getPhysicalDeviceMemoryProperties :: MonadIO m => VkPhysicalDevice -> m PhysicalDeviceMemoryProperties
getPhysicalDeviceMemoryProperties =
  fmap convertPhysicalDeviceMemoryProperties . liftIO . vulkanPtr . vkGetPhysicalDeviceMemoryProperties

getPhysicalDeviceFeatures :: MonadIO m => VkPhysicalDevice -> m VkPhysicalDeviceFeatures
getPhysicalDeviceFeatures =
  liftIO . vulkanPtr . vkGetPhysicalDeviceFeatures

getPhysicalDeviceFormatProperties :: MonadIO m => VkFormat -> VkPhysicalDevice -> m VkFormatProperties
getPhysicalDeviceFormatProperties format dev =
  liftIO . vulkanPtr $ vkGetPhysicalDeviceFormatProperties dev format

getPhysicalDeviceImageFormatProperties :: MonadIO m => VkFormat -> VkImageType -> VkImageTiling -> VkImageUsageFlags -> VkImageCreateFlags -> VkPhysicalDevice -> m VkImageFormatProperties
getPhysicalDeviceImageFormatProperties format imageType tiling usage flags dev =
  liftIO . vulkanPtrR $ vkGetPhysicalDeviceImageFormatProperties dev format imageType tiling usage flags
