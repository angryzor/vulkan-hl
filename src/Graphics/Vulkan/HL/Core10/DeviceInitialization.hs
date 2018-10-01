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
import Data.Default.Class
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

instance Default VkPhysicalDeviceFeatures where
  def =
    VkPhysicalDeviceFeatures { vkRobustBufferAccess = VK_FALSE
                             , vkFullDrawIndexUint32 = VK_FALSE
                             , vkImageCubeArray = VK_FALSE
                             , vkIndependentBlend = VK_FALSE
                             , vkGeometryShader = VK_FALSE
                             , vkTessellationShader = VK_FALSE
                             , vkSampleRateShading = VK_FALSE
                             , vkDualSrcBlend = VK_FALSE
                             , vkLogicOp = VK_FALSE
                             , vkMultiDrawIndirect = VK_FALSE
                             , vkDrawIndirectFirstInstance = VK_FALSE
                             , vkDepthClamp = VK_FALSE
                             , vkDepthBiasClamp = VK_FALSE
                             , vkFillModeNonSolid = VK_FALSE
                             , vkDepthBounds = VK_FALSE
                             , vkWideLines = VK_FALSE
                             , vkLargePoints = VK_FALSE
                             , vkAlphaToOne = VK_FALSE
                             , vkMultiViewport = VK_FALSE
                             , vkSamplerAnisotropy = VK_FALSE
                             , vkTextureCompressionETC2 = VK_FALSE
                             , vkTextureCompressionASTC_LDR = VK_FALSE
                             , vkTextureCompressionBC = VK_FALSE
                             , vkOcclusionQueryPrecise = VK_FALSE
                             , vkPipelineStatisticsQuery = VK_FALSE
                             , vkVertexPipelineStoresAndAtomics = VK_FALSE
                             , vkFragmentStoresAndAtomics = VK_FALSE
                             , vkShaderTessellationAndGeometryPointSize = VK_FALSE
                             , vkShaderImageGatherExtended = VK_FALSE
                             , vkShaderStorageImageExtendedFormats = VK_FALSE
                             , vkShaderStorageImageMultisample = VK_FALSE
                             , vkShaderStorageImageReadWithoutFormat = VK_FALSE
                             , vkShaderStorageImageWriteWithoutFormat = VK_FALSE
                             , vkShaderUniformBufferArrayDynamicIndexing = VK_FALSE
                             , vkShaderSampledImageArrayDynamicIndexing = VK_FALSE
                             , vkShaderStorageBufferArrayDynamicIndexing = VK_FALSE
                             , vkShaderStorageImageArrayDynamicIndexing = VK_FALSE
                             , vkShaderClipDistance = VK_FALSE
                             , vkShaderCullDistance = VK_FALSE
                             , vkShaderFloat64 = VK_FALSE
                             , vkShaderInt64 = VK_FALSE
                             , vkShaderInt16 = VK_FALSE
                             , vkShaderResourceResidency = VK_FALSE
                             , vkShaderResourceMinLod = VK_FALSE
                             , vkSparseBinding = VK_FALSE
                             , vkSparseResidencyBuffer = VK_FALSE
                             , vkSparseResidencyImage2D = VK_FALSE
                             , vkSparseResidencyImage3D = VK_FALSE
                             , vkSparseResidency2Samples = VK_FALSE
                             , vkSparseResidency4Samples = VK_FALSE
                             , vkSparseResidency8Samples = VK_FALSE
                             , vkSparseResidency16Samples = VK_FALSE
                             , vkSparseResidencyAliased = VK_FALSE
                             , vkVariableMultisampleRate = VK_FALSE
                             , vkInheritedQueries = VK_FALSE
                             }

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
                            } $ \createInfoPtr ->
  vulkanPtrR $ vkCreateInstance createInfoPtr nullPtr

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
