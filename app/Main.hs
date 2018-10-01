{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Codec.Picture hiding (withImage)
import Control.Lens ((&), (%~))
import Control.Monad
import Control.Monad.Cont
import Control.Monad.Loops
import Data.Bits
import qualified Data.ByteString as BS
import Data.Default.Class
import Data.List
import Data.Time.Clock
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Storable.Sized as SSV
import Data.Word
import Graphics.UI.GLFW as GLFW
import Graphics.Vulkan.Core10.Buffer
import Graphics.Vulkan.Core10.CommandBuffer
import Graphics.Vulkan.Core10.CommandBufferBuilding
import Graphics.Vulkan.Core10.CommandPool
import Graphics.Vulkan.Core10.Constants
import Graphics.Vulkan.Core10.Core
import Graphics.Vulkan.Core10.DescriptorSet
import Graphics.Vulkan.Core10.DeviceInitialization
import Graphics.Vulkan.Core10.Fence
import Graphics.Vulkan.Core10.Image
import Graphics.Vulkan.Core10.Memory
import Graphics.Vulkan.Core10.MemoryManagement
import Graphics.Vulkan.Core10.Pass
import Graphics.Vulkan.Core10.Pipeline
import Graphics.Vulkan.Core10.PipelineCache
import Graphics.Vulkan.Core10.PipelineLayout
import Graphics.Vulkan.Core10.Queue
import Graphics.Vulkan.Core10.Sampler
import Graphics.Vulkan.Core10.SparseResourceMemoryManagement
import qualified Graphics.Vulkan.Core10.ImageView as IV
import Graphics.Vulkan.Extensions.VK_EXT_debug_utils
import Graphics.Vulkan.Extensions.VK_KHR_surface as S
import Graphics.Vulkan.Extensions.VK_KHR_swapchain
import Graphics.Vulkan.HL.Core10.Buffer
import Graphics.Vulkan.HL.Core10.CommandBuffer
import Graphics.Vulkan.HL.Core10.CommandBufferBuilding
import Graphics.Vulkan.HL.Core10.CommandPool
import Graphics.Vulkan.HL.Core10.DescriptorSet
import Graphics.Vulkan.HL.Core10.Device
import Graphics.Vulkan.HL.Core10.DeviceInitialization
import Graphics.Vulkan.HL.Core10.Fence
import Graphics.Vulkan.HL.Core10.Image
import Graphics.Vulkan.HL.Core10.ImageView
import Graphics.Vulkan.HL.Core10.Memory
import Graphics.Vulkan.HL.Core10.MemoryManagement
import Graphics.Vulkan.HL.Core10.Pass
import Graphics.Vulkan.HL.Core10.Pipeline as HP
import Graphics.Vulkan.HL.Core10.PipelineCache
import Graphics.Vulkan.HL.Core10.PipelineLayout
import Graphics.Vulkan.HL.Core10.Queue
import Graphics.Vulkan.HL.Core10.QueueSemaphore
import Graphics.Vulkan.HL.Core10.Sampler
import Graphics.Vulkan.HL.Core10.Shader
import Graphics.Vulkan.HL.Extensions.VK_EXT_debug_utils (withDebugUtilsMessenger, DebugUtilsMessengerCallback, DebugUtilsMessengerCallbackData (..))
import Graphics.Vulkan.HL.Extensions.VK_KHR_surface
import Graphics.Vulkan.HL.Extensions.VK_KHR_swapchain as HSW
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Linear.Matrix
import Linear.V2
import Linear.V3
import Linear.V4
import Linear.Quaternion
import Linear.Projection
import System.Console.ANSI
import Util

deriving instance Enum VkResult

testBit' :: Bits a => a -> a -> Bool
testBit' a b = a .&. b /= zeroBits

debugMessage :: DebugUtilsMessengerCallback
debugMessage mType mSeverity cbData = do
  setSGR $ severityToANSI mSeverity
  putStrLn $ severityToString mSeverity <> " " <> message cbData
  setSGR [Reset]
  where
    severityToANSI :: VkDebugUtilsMessageSeverityFlagsEXT -> [SGR]
    severityToANSI VK_DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT = [SetColor Foreground Dull White]
    severityToANSI VK_DEBUG_UTILS_MESSAGE_SEVERITY_INFO_BIT_EXT = [SetColor Foreground Vivid White]
    severityToANSI VK_DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT = [SetColor Foreground Dull Yellow]
    severityToANSI VK_DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT = [SetColor Foreground Vivid Red]

    severityToString VK_DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT = "[VERBOSE]"
    severityToString VK_DEBUG_UTILS_MESSAGE_SEVERITY_INFO_BIT_EXT = "[INFO]"
    severityToString VK_DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT = "[WARNING]"
    severityToString VK_DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT = "[ERROR]"

-- showDeviceInfo :: VkPhysicalDevice -> IO ()
-- showDeviceInfo dev = do
--   print . deviceName =<< getPhysicalDeviceProperties dev
--   print =<< getPhysicalDeviceFeatures dev
--   print =<< getPhysicalDeviceQueueFamilyProperties dev
--   print =<< getPhysicalDeviceMemoryProperties dev

data MyVert = MyVert { position :: V3 CFloat
                     , color :: V3 CFloat
                     , texCoord :: V2 CFloat
                     }

instance Storable MyVert where
  sizeOf _ = sizeOf (undefined :: V3 CFloat) + sizeOf (undefined :: V3 CFloat) + sizeOf (undefined :: V2 CFloat)
  alignment _ = max (alignment (undefined :: V2 CFloat)) (alignment (undefined :: V3 CFloat))
  peek ptr = MyVert <$> peek (plusPtr ptr 0)
                    <*> peek (plusPtr ptr (sizeOf (undefined :: V3 CFloat)))
                    <*> peek (plusPtr ptr (2 * sizeOf (undefined :: V3 CFloat)))
  poke ptr poked = poke (plusPtr ptr 0) (position (poked :: MyVert))
                *> poke (plusPtr ptr (sizeOf (undefined :: V3 CFloat))) (color (poked :: MyVert))
                *> poke (plusPtr ptr (2 * sizeOf (undefined :: V3 CFloat))) (texCoord (poked :: MyVert))

data UniformBufferObject = UniformBufferObject { model :: M44 CFloat
                                               , view :: M44 CFloat
                                               , proj :: M44 CFloat
                                               } deriving (Show)

instance Storable UniformBufferObject where
  sizeOf _ = 3 * sizeOf (undefined :: M44 CFloat)
  alignment _ = sizeOf (undefined :: M44 CFloat)
  peek ptr = UniformBufferObject <$> peek (plusPtr ptr 0)
                                 <*> peek (plusPtr ptr (sizeOf (undefined :: M44 CFloat)))
                                 <*> peek (plusPtr ptr (2 * sizeOf (undefined :: M44 CFloat)))
  poke ptr poked = poke (plusPtr ptr 0) (model (poked :: UniformBufferObject))
                *> poke (plusPtr ptr (sizeOf (undefined :: M44 CFloat))) (view (poked :: UniformBufferObject))
                *> poke (plusPtr ptr (2 * sizeOf (undefined :: M44 CFloat))) (proj (poked :: UniformBufferObject))

-- triangle :: [MyVert]
-- triangle = [ MyVert { position = V2 0.0 (-0.5)
--                     , color = V3 1.0 0.0 0.0
--                     }
--            , MyVert { position = V2 0.5 0.5
--                     , color = V3 0.0 1.0 0.0
--                     }
--            , MyVert { position = V2 (-0.5) 0.5
--                     , color = V3 0.0 0.0 1.0
--                     }
--            ]

quadVerts :: [MyVert]
quadVerts = [ MyVert { position = V3 (-0.5) (-0.5) 0.0
                     , color = V3 1.0 0.0 0.0
                     , texCoord = V2 1.0 0.0
                     }
            , MyVert { position = V3 0.5 (-0.5) 0.0
                     , color = V3 0.0 1.0 0.0
                     , texCoord = V2 0.0 0.0
                     }
            , MyVert { position = V3 0.5 0.5 0.0
                     , color = V3 0.0 0.0 1.0
                     , texCoord = V2 0.0 1.0
                     }
            , MyVert { position = V3 (-0.5) 0.5 0.0
                     , color = V3 1.0 1.0 1.0
                     , texCoord = V2 1.0 1.0
                     }

            , MyVert { position = V3 (-0.5) (-0.5) (-0.5)
                     , color = V3 1.0 0.0 0.0
                     , texCoord = V2 1.0 0.0
                     }
            , MyVert { position = V3 0.5 (-0.5) (-0.5)
                     , color = V3 0.0 1.0 0.0
                     , texCoord = V2 0.0 0.0
                     }
            , MyVert { position = V3 0.5 0.5 (-0.5)
                     , color = V3 0.0 0.0 1.0
                     , texCoord = V2 0.0 1.0
                     }
            , MyVert { position = V3 (-0.5) 0.5 (-0.5)
                     , color = V3 1.0 1.0 1.0
                     , texCoord = V2 1.0 1.0
                     }
            ]

quadIndices :: [Word16]
quadIndices = [ 0,1,2,2,3,0
              , 4,5,6,6,7,4
              ]


vertexBufferSize = (fromIntegral ((*) <$> length <*> sizeOf . head $ quadVerts))
indexBufferSize = (fromIntegral ((*) <$> length <*> sizeOf . head $ quadIndices))
uniformBufferSize = sizeOf (undefined :: UniformBufferObject)

type Scope a = ContT a IO a

scope :: MonadIO m => Scope a -> m a
scope = liftIO . flip runContT return
track = ContT

withMyPipelines :: VkDevice -> VkExtent2D -> VkRenderPass -> VkPipelineLayout -> ([VkPipeline] -> IO a) -> IO a
withMyPipelines dev extent renderPass pipelineLayout next = scope $ do
  vertShaderCode <- liftIO $ BS.readFile "shaders/triangle.vert.spv"
  fragShaderCode <- liftIO $ BS.readFile "shaders/triangle.frag.spv"

  vertShader <- track $ withShaderModule dev vertShaderCode
  fragShader <- track $ withShaderModule dev fragShaderCode

  cache <- track $ withPipelineCache dev Nothing

  liftIO $ withGraphicsPipelines dev cache [ GraphicsPipelineCreateInfo { HP.flags = zeroBits
                                                                        , stages = [ PipelineShaderStageCreateInfo { stage = VK_SHADER_STAGE_VERTEX_BIT
                                                                                                                   , shaderModule = vertShader
                                                                                                                   , name = "main"
                                                                                                                   }
                                                                                   , PipelineShaderStageCreateInfo { stage = VK_SHADER_STAGE_FRAGMENT_BIT
                                                                                                                   , shaderModule = fragShader
                                                                                                                   , name = "main"
                                                                                                                   }
                                                                                   ]
                                                                        , vertexInputState = PipelineVertexInputStateCreateInfo { vertexBindingDescriptions = [ VkVertexInputBindingDescription { vkBinding = 0
                                                                                                                                                                                                , vkStride = fromIntegral . sizeOf . head $ quadVerts
                                                                                                                                                                                                , vkInputRate = VK_VERTEX_INPUT_RATE_VERTEX
                                                                                                                                                                                                }
                                                                                                                                                              ]
                                                                                                                                , vertexAttributeDescriptions = [ VkVertexInputAttributeDescription { vkBinding = 0
                                                                                                                                                                                                    , vkLocation = 0
                                                                                                                                                                                                    , vkFormat = VK_FORMAT_R32G32B32_SFLOAT
                                                                                                                                                                                                    , vkOffset = 0
                                                                                                                                                                                                    }
                                                                                                                                                                , VkVertexInputAttributeDescription { vkBinding = 0
                                                                                                                                                                                                    , vkLocation = 1
                                                                                                                                                                                                    , vkFormat = VK_FORMAT_R32G32B32_SFLOAT
                                                                                                                                                                                                    , vkOffset = fromIntegral $ sizeOf . position $ head quadVerts
                                                                                                                                                                                                    }
                                                                                                                                                                , VkVertexInputAttributeDescription { vkBinding = 0
                                                                                                                                                                                                    , vkLocation = 2
                                                                                                                                                                                                    , vkFormat = VK_FORMAT_R32G32_SFLOAT
                                                                                                                                                                                                    , vkOffset = fromIntegral $ (+) <$> sizeOf . position <*> sizeOf . color $ head quadVerts
                                                                                                                                                                                                    }
                                                                                                                                                                ]
                                                                                                                                }
                                                                        , inputAssemblyState = PipelineInputAssemblyStateCreateInfo { topology = VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST
                                                                                                                                    , primitiveRestartEnable = VK_FALSE
                                                                                                                                    }
                                                                        , viewportState = Just PipelineViewportStateCreateInfo { viewports = [ VkViewport (CFloat 0.0) (CFloat 0.0) (CFloat . fromIntegral $ vkWidth (extent :: VkExtent2D)) (CFloat  . fromIntegral $ vkHeight (extent :: VkExtent2D)) (CFloat 0.0) (CFloat 1.0) ]
                                                                                                                               , scissors = [ VkRect2D (VkOffset2D 0 0) extent ]
                                                                                                                               }
                                                                        , rasterizationState = PipelineRasterizationStateCreateInfo { depthClampEnable = VK_FALSE
                                                                                                                                    , rasterizerDiscardEnable = VK_FALSE
                                                                                                                                    , polygonMode = VK_POLYGON_MODE_FILL
                                                                                                                                    , lineWidth = 1.0
                                                                                                                                    , cullMode = VK_CULL_MODE_BACK_BIT
                                                                                                                                    , frontFace = VK_FRONT_FACE_COUNTER_CLOCKWISE
                                                                                                                                    , depthBiasEnable = VK_FALSE
                                                                                                                                    , depthBiasConstantFactor = 0.0
                                                                                                                                    , depthBiasClamp = 0.0
                                                                                                                                    , depthBiasSlopeFactor = 0.0
                                                                                                                                    }
                                                                        , multisampleState = Just PipelineMultisampleStateCreateInfo { sampleShadingEnable = VK_FALSE
                                                                                                                                     , rasterizationSamples = VK_SAMPLE_COUNT_1_BIT
                                                                                                                                     , minSampleShading = 1.0
                                                                                                                                     , sampleMask = Nothing
                                                                                                                                     , alphaToCoverageEnable = VK_FALSE
                                                                                                                                     , alphaToOneEnable = VK_FALSE
                                                                                                                                     }
                                                                        , colorBlendState = Just PipelineColorBlendStateCreateInfo { logicOpEnable = VK_FALSE
                                                                                                                                   , logicOp = VK_LOGIC_OP_COPY
                                                                                                                                   , attachments = [ VkPipelineColorBlendAttachmentState { vkColorWriteMask = VK_COLOR_COMPONENT_R_BIT .|. VK_COLOR_COMPONENT_G_BIT .|. VK_COLOR_COMPONENT_B_BIT .|. VK_COLOR_COMPONENT_A_BIT
                                                                                                                                                                                         , vkBlendEnable = VK_FALSE
                                                                                                                                                                                         , vkSrcColorBlendFactor = VK_BLEND_FACTOR_ONE
                                                                                                                                                                                         , vkDstColorBlendFactor = VK_BLEND_FACTOR_ZERO
                                                                                                                                                                                         , vkColorBlendOp = VK_BLEND_OP_ADD
                                                                                                                                                                                         , vkSrcAlphaBlendFactor = VK_BLEND_FACTOR_ONE
                                                                                                                                                                                         , vkDstAlphaBlendFactor = VK_BLEND_FACTOR_ZERO
                                                                                                                                                                                         , vkAlphaBlendOp = VK_BLEND_OP_ADD
                                                                                                                                                                                         }
                                                                                                                                                   ]
                                                                                                                                   , blendConstants = V4 0.0 0.0 0.0 0.0
                                                                                                                                   }
                                                                        , depthStencilState = Nothing
                                                                        , tessellationState = Nothing
                                                                        , dynamicState = Nothing
                                                                        , layout = pipelineLayout
                                                                        , renderPass = renderPass
                                                                        , subpass = 0
                                                                        , basePipelineHandle = VK_NULL_HANDLE
                                                                        , basePipelineIndex = -1
                                                                        }
                                  ] next

  -- let berts = S.fromList' [ V3 2.3 1.2 3.2 ] :: Maybe (S.Sized V.Vector 1 (V3 Double))
  -- let verts = V3 3.4 2.0 1.3 <| V3 2.3 1.5 2.1 <| V3 2.3 1.5 2.1 <| V3 2.3 1.5 2.1 <| S.empty
  -- let faces = [ Face [od| 0 |] [od| 1 |] [od| 2 |]
  --             ]
  -- let geo = fromIndexedVerts verts faces
  -- someFunc

data Frame = Frame { commandBuffer :: VkCommandBuffer
                   , image :: VkImage
                   , imageView :: IV.VkImageView
                   , framebuffer :: VkFramebuffer
                   , uniformBuffer :: VkBuffer
                   , uniformBufferMemory :: VkDeviceMemory
                   , descriptorSet :: VkDescriptorSet
                   }

withFrames :: VkPhysicalDevice -> VkDevice -> VkSwapchainKHR -> VkFormat -> VkCommandPool -> VkDescriptorSetLayout -> VkRenderPass -> VkExtent2D -> IV.VkImageView -> VkSampler -> ([Frame] -> IO a) -> IO a
withFrames physDev dev swapchain format commandPool descriptorSetLayout renderPass extent imageImageView imageSampler next = scope $ do
  images <- getSwapchainImages dev swapchain
  descriptorPool <- track $ withDescriptorPool dev zeroBits (fromIntegral $ length images) [ VkDescriptorPoolSize { vkType = VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER
                                                                                                                  , vkDescriptorCount = fromIntegral $ length images
                                                                                                                  }
                                                                                           , VkDescriptorPoolSize { vkType = VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
                                                                                                                  , vkDescriptorCount = fromIntegral $ length images
                                                                                                                  }
                                                                                           ]
  commandBuffers <- track $ withCommandBuffers dev commandPool VK_COMMAND_BUFFER_LEVEL_PRIMARY (length images)
  descriptorSets <- allocateDescriptorSets dev descriptorPool $ replicate (length images) descriptorSetLayout
  frames <- sequenceA $ zipWith3 mkFrame images commandBuffers descriptorSets
  liftIO $ next frames
  where
    mkFrame image commandBuffer descriptorSet = do
      imageView <- track $ withImageView dev
                                         IV.VK_IMAGE_VIEW_TYPE_2D format
                                         (IV.VkComponentMapping IV.VK_COMPONENT_SWIZZLE_IDENTITY IV.VK_COMPONENT_SWIZZLE_IDENTITY IV.VK_COMPONENT_SWIZZLE_IDENTITY IV.VK_COMPONENT_SWIZZLE_IDENTITY)
                                         IV.VkImageSubresourceRange { IV.vkAspectMask = VK_IMAGE_ASPECT_COLOR_BIT
                                                                    , IV.vkBaseMipLevel = 0
                                                                    , IV.vkLevelCount = 1
                                                                    , IV.vkBaseArrayLayer = 0
                                                                    , IV.vkLayerCount = 1
                                                                    }
                                         image
      framebuffer <- track $ withFramebuffer dev renderPass (vkWidth (extent :: VkExtent2D)) (vkHeight (extent :: VkExtent2D)) 1 [imageView]
      (uniformBuffer, uniformBufferMemory) <- track $ withBufferAndMemory physDev dev (fromIntegral uniformBufferSize) VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT (VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. VK_MEMORY_PROPERTY_HOST_COHERENT_BIT) . curry
      updateDescriptorSets dev [ WriteDescriptorSet { dstSet = descriptorSet
                                                    , dstBinding = 0
                                                    , dstArrayElement = 0
                                                    , batch = BufferDescriptorBatch { bufferDescriptorType = UniformBuffer
                                                                                    , bufferDescriptors = [ VkDescriptorBufferInfo { vkBuffer = uniformBuffer
                                                                                                                                   , vkOffset = 0
                                                                                                                                   , vkRange = fromIntegral uniformBufferSize
                                                                                                                                   }
                                                                                                          ]
                                                                                    }
                                                    }
                               , WriteDescriptorSet { dstSet = descriptorSet
                                                    , dstBinding = 1
                                                    , dstArrayElement = 0
                                                    , batch = ImageDescriptorBatch { imageDescriptorType = CombinedImageSampler
                                                                                   , imageDescriptors = [ VkDescriptorImageInfo { vkImageLayout = VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
                                                                                                                                , vkImageView = imageImageView
                                                                                                                                , vkSampler = imageSampler
                                                                                                                                }
                                                                                                        ]
                                                                                   }
                                                    }
                               ] []
      return Frame { commandBuffer = commandBuffer
                   , image = image
                   , imageView = imageView
                   , framebuffer = framebuffer
                   , uniformBuffer = uniformBuffer
                   , uniformBufferMemory = uniformBufferMemory
                   , descriptorSet = descriptorSet
                   }

data FrameRenderContext = FrameRenderContext { imageAvailable :: VkSemaphore
                                             , renderFinished :: VkSemaphore
                                             , inFlight :: VkFence
                                             }

withFrameRenderContexts :: VkDevice -> Int -> ([FrameRenderContext] -> IO a) -> IO a
withFrameRenderContexts dev count next = scope $
  liftIO . next =<< replicateM count mkFrameRenderContext
  where
    mkFrameRenderContext = do
      imageAvailable <- track $ withSemaphore dev
      renderFinished <- track $ withSemaphore dev
      inFlight <- track $ withFence dev VK_FENCE_CREATE_SIGNALED_BIT
      return FrameRenderContext { imageAvailable = imageAvailable
                                , renderFinished = renderFinished
                                , inFlight = inFlight
                                }

findMemory :: (MonadIO m, Num a) => VkPhysicalDevice -> VkMemoryRequirements -> VkMemoryPropertyFlags -> m (Maybe a)
findMemory physDev memoryReq properties = do
  deviceMemProps <- getPhysicalDeviceMemoryProperties physDev
  return $ fmap fromIntegral . findIndex (\(memoryType,idx) -> testBit (vkMemoryTypeBits memoryReq) idx && memoryType .&. properties == properties) $ zip (vkPropertyFlags <$> memoryTypes deviceMemProps) [0..]

withBufferAndMemory :: VkPhysicalDevice -> VkDevice -> VkDeviceSize -> VkBufferUsageFlags -> VkMemoryPropertyFlags -> (VkBuffer -> VkDeviceMemory -> IO a) -> IO a
withBufferAndMemory physDev dev size usage properties next = scope $ do
  buffer <- track $ withBuffer dev zeroBits size usage VK_SHARING_MODE_EXCLUSIVE []
  memoryReq <- getBufferMemoryRequirements dev buffer

  Just memoryType <- findMemory physDev memoryReq properties

  bufferMemory <- track $ withMemory dev memoryType (Graphics.Vulkan.Core10.MemoryManagement.vkSize memoryReq)
  bindBufferMemory dev buffer bufferMemory 0

  liftIO $ next buffer bufferMemory

withImageAndNoMemory :: VkPhysicalDevice -> VkDevice -> Word32 -> Word32 -> VkFormat -> VkImageTiling -> VkImageUsageFlags -> VkMemoryPropertyFlags -> (VkImage -> IO a) -> IO a
withImageAndNoMemory physDev dev width height format tiling usage properties next = scope $ do
  image <- track $ withImage dev ImageCreateInfo { flags = zeroBits
                                                 , imageType = VK_IMAGE_TYPE_2D
                                                 , extent = VkExtent3D width height 1
                                                 , mipLevels = 1
                                                 , arrayLayers = 1
                                                 , format = format
                                                 , tiling = tiling
                                                 , initialLayout = VK_IMAGE_LAYOUT_UNDEFINED
                                                 , usage = usage
                                                 , sharingMode = VK_SHARING_MODE_EXCLUSIVE
                                                 , samples = VK_SAMPLE_COUNT_1_BIT
                                                 , queueFamilyIndices = []
                                                 }
  memoryReq <- getImageMemoryRequirements dev image

  Just memoryType <- findMemory physDev memoryReq properties

  imageMemory <- track $ withMemory dev memoryType (Graphics.Vulkan.Core10.MemoryManagement.vkSize memoryReq)
  bindImageMemory dev image imageMemory 0

  liftIO $ next image

oneshot :: MonadIO m => VkDevice -> VkQueue -> VkCommandPool -> [Command] -> m ()
oneshot dev queue commandPool commands = scope $ do
  [copyBuffer] <- track $ withCommandBuffers dev commandPool VK_COMMAND_BUFFER_LEVEL_PRIMARY 1
  recordCommandBuffer VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT copyBuffer commands

  queueSubmit [ SubmitInfo { waitSemaphores = []
                           , commandBuffers = [ copyBuffer ]
                           , signalSemaphores = []
                           }
              ] VK_NULL_HANDLE queue
  queueWaitIdle queue

main :: IO ()
main = scope $ do
  liftIO $ GLFW.init
  glfwExts <- liftIO $ getRequiredInstanceExtensions
  glfwExtsAsStr <- liftIO $ traverse peekCString glfwExts

  scope $ do
    inst <- track $ withInstance "Test" 0 "Vulkan-HL Test Engine" 0 ["VK_LAYER_LUNARG_standard_validation"] (VK_EXT_DEBUG_UTILS_EXTENSION_NAME:glfwExtsAsStr)
    messenger <- track $ withDebugUtilsMessenger (VK_DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT .|. VK_DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT .|. VK_DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT) (VK_DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT .|. VK_DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT .|. VK_DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT) debugMessage inst

    physicalDevices <- enumeratePhysicalDevices inst

    -- traverse showDeviceInfo physicalDevices

    let physDev = head physicalDevices

    liftIO . windowHint $ WindowHint'ClientAPI ClientAPI'NoAPI
    liftIO . windowHint $ WindowHint'Resizable False
    Just wnd <- liftIO $ createWindow 800 600 "Vulkan HL Test" Nothing Nothing

    surf <- liftIO . vulkanPtrR $ createWindowSurface inst wnd nullPtr

    -- print =<< getPhysicalDeviceSurfaceSupport surf physDev 0
    -- print =<< getPhysicalDeviceSurfaceCapabilities surf physDev
    -- print =<< getPhysicalDeviceSurfaceFormats surf physDev
    -- print =<< getPhysicalDeviceSurfacePresentModes surf physDev

    deviceSurfaceCapabilities <- getPhysicalDeviceSurfaceCapabilities surf physDev
    deviceSurfaceFormats <- getPhysicalDeviceSurfaceFormats surf physDev
    devicePresentModes <- getPhysicalDeviceSurfacePresentModes surf physDev

    let surfaceFormat = last deviceSurfaceFormats
    let presentMode = head devicePresentModes
    let extent = vkCurrentExtent deviceSurfaceCapabilities
    -- liftIO . print $ surfaceFormat

    availQueues <- getPhysicalDeviceQueueFamilyProperties physDev
    presentSupports <- traverse (getPhysicalDeviceSurfaceSupport surf physDev) [0..fromIntegral (length availQueues) - 1]

    let Just graphicsQueueFamilyIdx = fromIntegral <$> findIndex (testBit' VK_QUEUE_GRAPHICS_BIT . vkQueueFlags) availQueues
    let Just presentQueueFamilyIdx = fromIntegral <$> elemIndex VK_TRUE presentSupports

    scope $ do
      dev <- track $ withDevice (map (flip DeviceQueueCreateInfo [1.0]) ([graphicsQueueFamilyIdx] `union` [presentQueueFamilyIdx])) ["VK_LAYER_LUNARG_standard_validation"] [VK_KHR_SWAPCHAIN_EXTENSION_NAME] (Just def { vkSamplerAnisotropy = VK_TRUE }) physDev
      swapchain <- track $ withSwapchain SwapchainCreateInfo { HSW.flags = zeroBits
                                                             , minImageCount = vkMaxImageCount deviceSurfaceCapabilities
                                                             , imageFormat = S.vkFormat surfaceFormat
                                                             , imageColorSpace = vkColorSpace surfaceFormat
                                                             , imageExtent = extent
                                                             , imageArrayLayers = 1
                                                             , imageUsage = VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT
                                                             , imageSharingMode = VK_SHARING_MODE_EXCLUSIVE
                                                             , queueFamilyIndices = []
                                                             , preTransform = vkCurrentTransform deviceSurfaceCapabilities
                                                             , compositeAlpha = VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR
                                                             , presentMode = presentMode
                                                             , clipped = VK_TRUE
                                                             , oldSwapChain = VK_NULL_HANDLE
                                                             } surf dev

      graphicsQueue <- getDeviceQueue graphicsQueueFamilyIdx 0 dev
      presentQueue <- getDeviceQueue presentQueueFamilyIdx 0 dev

      (vertexStagingBuffer, vertexStagingBufferMemory) <- track $ withBufferAndMemory physDev dev vertexBufferSize VK_BUFFER_USAGE_TRANSFER_SRC_BIT (VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. VK_MEMORY_PROPERTY_HOST_COHERENT_BIT) . curry
      (vertexBuffer, vertexBufferMemory) <- track $ withBufferAndMemory physDev dev vertexBufferSize (VK_BUFFER_USAGE_TRANSFER_DST_BIT .|. VK_BUFFER_USAGE_VERTEX_BUFFER_BIT) VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT . curry

      (indexStagingBuffer, indexStagingBufferMemory) <- track $ withBufferAndMemory physDev dev indexBufferSize VK_BUFFER_USAGE_TRANSFER_SRC_BIT (VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. VK_MEMORY_PROPERTY_HOST_COHERENT_BIT) . curry
      (indexBuffer, indexBufferMemory) <- track $ withBufferAndMemory physDev dev indexBufferSize (VK_BUFFER_USAGE_TRANSFER_DST_BIT .|. VK_BUFFER_USAGE_INDEX_BUFFER_BIT) VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT . curry

      scope $ do
        memoryPtr <- track $ mappingMemory dev vertexStagingBufferMemory 0 vertexBufferSize
        liftIO $ pokeArray memoryPtr quadVerts

      scope $ do
        memoryPtr <- track $ mappingMemory dev indexStagingBufferMemory 0 indexBufferSize
        liftIO $ pokeArray memoryPtr quadIndices

      Right sampleImageData <- liftIO $ fmap convertRGBA8 <$> readImage "test-image.png"
      let imageBufferSize = fromIntegral $ (SV.length . imageData $ sampleImageData) * (sizeOf . SV.head . imageData $ sampleImageData)

      (imageStagingBuffer, imageStagingBufferMemory) <- track $ withBufferAndMemory physDev dev imageBufferSize VK_BUFFER_USAGE_TRANSFER_SRC_BIT (VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. VK_MEMORY_PROPERTY_HOST_COHERENT_BIT) . curry

      imageImage <- track $ withImageAndNoMemory physDev dev (fromIntegral $ imageWidth sampleImageData) (fromIntegral $ imageHeight sampleImageData) VK_FORMAT_R8G8B8A8_UNORM VK_IMAGE_TILING_OPTIMAL (VK_IMAGE_USAGE_TRANSFER_DST_BIT .|. VK_IMAGE_USAGE_SAMPLED_BIT) VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT

      scope $ do
        memoryPtr <- track $ mappingMemory dev imageStagingBufferMemory 0 imageBufferSize
        liftIO . pokeArray memoryPtr . SV.toList . imageData $ sampleImageData

      descriptorSetLayout <- track $ withDescriptorSetLayout dev zeroBits [ DescriptorSetLayoutBinding { binding = 0
                                                                                                       , descriptorType = VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER
                                                                                                       , descriptorCount = 1
                                                                                                       , stageFlags = VK_SHADER_STAGE_VERTEX_BIT
                                                                                                       , immutableSamplers = Nothing
                                                                                                       }
                                                                          , DescriptorSetLayoutBinding { binding = 1
                                                                                                       , descriptorType = VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
                                                                                                       , descriptorCount = 1
                                                                                                       , stageFlags = VK_SHADER_STAGE_FRAGMENT_BIT
                                                                                                       , immutableSamplers = Nothing
                                                                                                       }
                                                                          ]

      renderPass <- track $ withRenderPass dev [ VkAttachmentDescription { vkFlags = zeroBits
                                                                         , vkFormat = S.vkFormat surfaceFormat
                                                                         , vkSamples = VK_SAMPLE_COUNT_1_BIT
                                                                         , vkLoadOp = VK_ATTACHMENT_LOAD_OP_CLEAR
                                                                         , vkStoreOp = VK_ATTACHMENT_STORE_OP_STORE
                                                                         , vkStencilLoadOp = VK_ATTACHMENT_LOAD_OP_DONT_CARE
                                                                         , vkStencilStoreOp = VK_ATTACHMENT_STORE_OP_DONT_CARE
                                                                         , vkInitialLayout = VK_IMAGE_LAYOUT_UNDEFINED
                                                                         , vkFinalLayout = VK_IMAGE_LAYOUT_PRESENT_SRC_KHR
                                                                         }
                                               ]
                                               [ SubpassDescription { flags = zeroBits
                                                                    , pipelineBindPoint = VK_PIPELINE_BIND_POINT_GRAPHICS
                                                                    , colorAttachments = [ VkAttachmentReference { vkAttachment = 0
                                                                                                                 , vkLayout = VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
                                                                                                                 }
                                                                                         ]
                                                                    , inputAttachments = []
                                                                    , resolveAttachments = Nothing
                                                                    , depthStencilAttachment = Nothing
                                                                    , preserveAttachments = []
                                                                    }
                                               ]
                                               [ VkSubpassDependency { vkDependencyFlags = zeroBits
                                                                     , vkSrcSubpass = VK_SUBPASS_EXTERNAL
                                                                     , vkDstSubpass = 0
                                                                     , vkSrcStageMask = VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
                                                                     , vkSrcAccessMask = zeroBits
                                                                     , vkDstStageMask = VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
                                                                     , vkDstAccessMask = VK_ACCESS_COLOR_ATTACHMENT_READ_BIT .|. VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT
                                                                     }
                                               ]

      pipelineLayout <- track $ withPipelineLayout dev [descriptorSetLayout] []
      [pipeline] <- track $ withMyPipelines dev extent renderPass pipelineLayout

      commandPool <- track $ withCommandPool dev zeroBits 0

      oneshot dev graphicsQueue commandPool
        [ CopyBuffer vertexStagingBuffer vertexBuffer [ VkBufferCopy { vkSrcOffset = 0, vkDstOffset = 0, vkSize = vertexBufferSize } ]
        ]

      oneshot dev graphicsQueue commandPool
        [ CopyBuffer indexStagingBuffer indexBuffer [ VkBufferCopy { vkSrcOffset = 0, vkDstOffset = 0, vkSize = indexBufferSize } ]
        ]

      oneshot dev graphicsQueue commandPool
        [ PipelineBarrier VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT
                          VK_PIPELINE_STAGE_TRANSFER_BIT
                          zeroBits
                          []
                          []
                          [ ImageMemoryBarrier { srcAccessMask = zeroBits
                                               , dstAccessMask = VK_ACCESS_TRANSFER_WRITE_BIT
                                               , oldLayout = VK_IMAGE_LAYOUT_UNDEFINED
                                               , newLayout = VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
                                               , srcQueueFamilyIndex = VK_QUEUE_FAMILY_IGNORED
                                               , dstQueueFamilyIndex = VK_QUEUE_FAMILY_IGNORED
                                               , image = imageImage
                                               , subresourceRange = IV.VkImageSubresourceRange { vkAspectMask = VK_IMAGE_ASPECT_COLOR_BIT
                                                                                               , vkBaseMipLevel = 0
                                                                                               , vkLevelCount = 1
                                                                                               , vkBaseArrayLayer = 0
                                                                                               , vkLayerCount = 1
                                                                                               }
                                               }
                          ]
        ]

      oneshot dev graphicsQueue commandPool
        [ CopyBufferToImage imageStagingBuffer imageImage VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL [ VkBufferImageCopy { vkBufferOffset = 0
                                                                                                                   , vkBufferRowLength = 0
                                                                                                                   , vkBufferImageHeight = 0
                                                                                                                   , vkImageSubresource = VkImageSubresourceLayers { vkAspectMask = VK_IMAGE_ASPECT_COLOR_BIT
                                                                                                                                                                   , vkMipLevel = 0
                                                                                                                                                                   , vkBaseArrayLayer = 0
                                                                                                                                                                   , vkLayerCount = 1
                                                                                                                                                                   }
                                                                                                                   , vkImageOffset = VkOffset3D 0 0 0
                                                                                                                   , vkImageExtent = VkExtent3D (fromIntegral $ imageWidth sampleImageData) (fromIntegral $ imageHeight sampleImageData) 1
                                                                                                                   }
                                                                                               ]
        ]

      oneshot dev graphicsQueue commandPool
        [ PipelineBarrier VK_PIPELINE_STAGE_TRANSFER_BIT
                          VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT
                          zeroBits
                          []
                          []
                          [ ImageMemoryBarrier { srcAccessMask = VK_ACCESS_TRANSFER_WRITE_BIT
                                               , dstAccessMask = VK_ACCESS_SHADER_READ_BIT
                                               , oldLayout = VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
                                               , newLayout = VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
                                               , srcQueueFamilyIndex = VK_QUEUE_FAMILY_IGNORED
                                               , dstQueueFamilyIndex = VK_QUEUE_FAMILY_IGNORED
                                               , image = imageImage
                                               , subresourceRange = IV.VkImageSubresourceRange { vkAspectMask = VK_IMAGE_ASPECT_COLOR_BIT
                                                                                               , vkBaseMipLevel = 0
                                                                                               , vkLevelCount = 1
                                                                                               , vkBaseArrayLayer = 0
                                                                                               , vkLayerCount = 1
                                                                                               }
                                               }
                          ]
        ]

      imageImageView <- track $ withImageView dev
                                              IV.VK_IMAGE_VIEW_TYPE_2D
                                              VK_FORMAT_R8G8B8A8_UNORM
                                              (IV.VkComponentMapping IV.VK_COMPONENT_SWIZZLE_IDENTITY IV.VK_COMPONENT_SWIZZLE_IDENTITY IV.VK_COMPONENT_SWIZZLE_IDENTITY IV.VK_COMPONENT_SWIZZLE_IDENTITY)
                                              IV.VkImageSubresourceRange { IV.vkAspectMask = VK_IMAGE_ASPECT_COLOR_BIT
                                                                         , IV.vkBaseMipLevel = 0
                                                                         , IV.vkLevelCount = 1
                                                                         , IV.vkBaseArrayLayer = 0
                                                                         , IV.vkLayerCount = 1
                                                                         }
                                              imageImage

      imageSampler <- track $ withSampler dev SamplerCreateInfo { flags = zeroBits
                                                                , magFilter = VK_FILTER_LINEAR
                                                                , minFilter = VK_FILTER_LINEAR
                                                                , addressModeU = VK_SAMPLER_ADDRESS_MODE_REPEAT
                                                                , addressModeV = VK_SAMPLER_ADDRESS_MODE_REPEAT
                                                                , addressModeW = VK_SAMPLER_ADDRESS_MODE_REPEAT
                                                                , anisotropyEnable = VK_TRUE
                                                                , maxAnisotropy = 16
                                                                , borderColor = VK_BORDER_COLOR_INT_OPAQUE_BLACK
                                                                , unnormalizedCoordinates = VK_FALSE
                                                                , compareEnable = VK_FALSE
                                                                , compareOp = VK_COMPARE_OP_ALWAYS
                                                                , mipmapMode = VK_SAMPLER_MIPMAP_MODE_LINEAR
                                                                , mipLodBias = 0.0
                                                                , minLod = 0.0
                                                                , maxLod = 0.0
                                                                }

      frames <- track $ withFrames physDev dev swapchain (S.vkFormat surfaceFormat) commandPool descriptorSetLayout renderPass extent imageImageView imageSampler

      forM_ frames $ \Frame{..} ->
        recordCommandBuffer VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT commandBuffer
          [ BeginRenderPass renderPass framebuffer (VkRect2D (VkOffset2D 0 0) extent) [ VkColor . VkFloat32 $ SSV.fromTuple (CFloat 0.0, CFloat 0.0, CFloat 0.0, CFloat 0.0) ] VK_SUBPASS_CONTENTS_INLINE
          , BindPipeline VK_PIPELINE_BIND_POINT_GRAPHICS pipeline
          , BindVertexBuffers 0 [ VertexBufferBinding { buffer = vertexBuffer, offset = 0 } ]
          , BindIndexBuffer indexBuffer 0 VK_INDEX_TYPE_UINT16
          , BindDescriptorSets VK_PIPELINE_BIND_POINT_GRAPHICS pipelineLayout 0 [descriptorSet] []
          , DrawIndexed (fromIntegral . length $ quadIndices) 1 0 0 0
          , EndRenderPass
          ]

      frameRenderContexts <- track $ withFrameRenderContexts dev 2

      startTime <- liftIO getCurrentTime

      whileM_ (not <$> liftIO (windowShouldClose wnd)) $ do
        liftIO $ pollEvents

        forM_ frameRenderContexts $ \FrameRenderContext{..} -> do
          waitForFences dev [inFlight] VK_TRUE (maxBound :: Word64)
          resetFences dev [inFlight]

          imgIdx <- acquireNextImage dev swapchain (maxBound :: Word64) imageAvailable VK_NULL_HANDLE
          let Frame{..} = frames !! fromIntegral imgIdx

          currentTime <- liftIO getCurrentTime
          let elapsed = diffUTCTime currentTime startTime

          scope $ do
            memoryPtr <- track $ mappingMemory dev uniformBufferMemory 0 (fromIntegral uniformBufferSize)
            liftIO $ poke memoryPtr UniformBufferObject { model = Linear.Matrix.transpose $ flip mkTransformation (V3 0.0 0.0 0.0) . axisAngle (V3 0.0 0.0 1.0) $ realToFrac elapsed * pi / 2
                                                        , view = Linear.Matrix.transpose $ lookAt (V3 2.0 2.0 2.0) (V3 0.0 0.0 0.0) (V3 0.0 0.0 1.0)
                                                        , proj = (Linear.Matrix.transpose $ perspective (pi / 4.0) (fromIntegral (vkWidth (extent :: VkExtent2D)) / fromIntegral (vkHeight (extent :: VkExtent2D))) 0.1 10.0) & _y._y %~ (*(-1))
                                                        }

          queueSubmit [ SubmitInfo { waitSemaphores = [ (imageAvailable, VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT) ]
                                   , commandBuffers = [ commandBuffer ]
                                   , signalSemaphores = [ renderFinished ]
                                   }
                      ] inFlight graphicsQueue
          queuePresent [ renderFinished ] [ (swapchain, imgIdx) ] graphicsQueue

      deviceWaitIdle dev

    liftIO $ destroyWindow wnd

  liftIO $ terminate
