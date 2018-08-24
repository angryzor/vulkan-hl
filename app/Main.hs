{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import Control.Monad
import Control.Monad.Cont
import Control.Monad.Loops
import Data.Bits
import qualified Data.ByteString as BS
import Data.List
import Data.Word
import qualified Data.Vector.Storable.Sized as SV
import Graphics.UI.GLFW as GLFW
import Graphics.Vulkan.Core10.Buffer
import Graphics.Vulkan.Core10.CommandBuffer
import Graphics.Vulkan.Core10.CommandBufferBuilding
import Graphics.Vulkan.Core10.Constants
import Graphics.Vulkan.Core10.Core
import Graphics.Vulkan.Core10.DeviceInitialization
import Graphics.Vulkan.Core10.Image
import Graphics.Vulkan.Core10.Pass
import Graphics.Vulkan.Core10.Pipeline
import Graphics.Vulkan.Core10.PipelineCache
import Graphics.Vulkan.Core10.PipelineLayout
import Graphics.Vulkan.Core10.Queue
import Graphics.Vulkan.Core10.SparseResourceMemoryManagement
import qualified Graphics.Vulkan.Core10.ImageView as IV
import Graphics.Vulkan.Extensions.VK_EXT_debug_utils
import Graphics.Vulkan.Extensions.VK_KHR_surface as S
import Graphics.Vulkan.Extensions.VK_KHR_swapchain
import Graphics.Vulkan.HL.Core10.CommandBuffer
import Graphics.Vulkan.HL.Core10.CommandBufferBuilding
import Graphics.Vulkan.HL.Core10.CommandPool
import Graphics.Vulkan.HL.Core10.Device
import Graphics.Vulkan.HL.Core10.DeviceInitialization
import Graphics.Vulkan.HL.Core10.ImageView
import Graphics.Vulkan.HL.Core10.Pass
import Graphics.Vulkan.HL.Core10.Pipeline as HP
import Graphics.Vulkan.HL.Core10.PipelineCache
import Graphics.Vulkan.HL.Core10.PipelineLayout
import Graphics.Vulkan.HL.Core10.Queue
import Graphics.Vulkan.HL.Core10.QueueSemaphore
import Graphics.Vulkan.HL.Core10.Shader
import Graphics.Vulkan.HL.Extensions.VK_EXT_debug_utils
import Graphics.Vulkan.HL.Extensions.VK_KHR_surface
import Graphics.Vulkan.HL.Extensions.VK_KHR_swapchain as HSW
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign.Ptr
import Linear.V4
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

type Scope a = ContT a IO a

scope :: MonadIO m => Scope a -> m a
scope = liftIO . flip runContT return
track = ContT

withMyPipelines :: VkDevice -> VkExtent2D -> VkRenderPass -> ([VkPipeline] -> IO a) -> IO a
withMyPipelines dev extent renderPass next = scope $ do
  vertShaderCode <- liftIO $ BS.readFile "shaders/triangle.vert.spv"
  fragShaderCode <- liftIO $ BS.readFile "shaders/triangle.frag.spv"

  vertShader <- track $ withShaderModule dev vertShaderCode
  fragShader <- track $ withShaderModule dev fragShaderCode

  cache <- track $ withPipelineCache dev Nothing
  layout <- track $ withPipelineLayout dev [] []

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
                                                                        , vertexInputState = PipelineVertexInputStateCreateInfo { vertexBindingDescriptions = []
                                                                                                                                , vertexAttributeDescriptions = []
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
                                                                                                                                    , frontFace = VK_FRONT_FACE_CLOCKWISE
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
                                                                        , layout = layout
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

    let surfaceFormat = head deviceSurfaceFormats
    let presentMode = head devicePresentModes
    let extent = vkCurrentExtent deviceSurfaceCapabilities

    availQueues <- getPhysicalDeviceQueueFamilyProperties physDev
    presentSupports <- traverse (getPhysicalDeviceSurfaceSupport surf physDev) [0..fromIntegral (length availQueues) - 1]

    let Just graphicsQueueFamilyIdx = fromIntegral <$> findIndex (testBit' VK_QUEUE_GRAPHICS_BIT . vkQueueFlags) availQueues
    let Just presentQueueFamilyIdx = fromIntegral <$> elemIndex VK_TRUE presentSupports

    scope $ do
      dev <- track $ withDevice (map (flip DeviceQueueCreateInfo [1.0]) ([graphicsQueueFamilyIdx] `union` [presentQueueFamilyIdx])) ["VK_LAYER_LUNARG_standard_validation"] [VK_KHR_SWAPCHAIN_EXTENSION_NAME] Nothing physDev
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
      images <- getSwapchainImages dev swapchain
      imageViews <- traverse (track . mkImageView dev (S.vkFormat surfaceFormat)) images

      liftIO $ print imageViews

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

      [pipeline] <- track $ withMyPipelines dev extent renderPass
      frameBuffers <- traverse (track . withFramebuffer dev renderPass (vkWidth (extent :: VkExtent2D)) (vkHeight (extent :: VkExtent2D)) 1 . (:[])) imageViews

      commandPool <- track $ withCommandPool dev zeroBits 0
      commandBuffers <- track $ withCommandBuffers dev commandPool VK_COMMAND_BUFFER_LEVEL_PRIMARY (length imageViews)

      flip traverse (zip commandBuffers frameBuffers) $ \(buf, frameBuffer) -> do
        beginCommandBuffer VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT buf
        cmdBeginRenderPass buf renderPass frameBuffer (VkRect2D (VkOffset2D 0 0) extent) [ VkColor . VkFloat32 $ SV.fromTuple (CFloat 0.0, CFloat 0.0, CFloat 0.0, CFloat 0.0) ] VK_SUBPASS_CONTENTS_INLINE
        cmdBindPipeline buf VK_PIPELINE_BIND_POINT_GRAPHICS pipeline
        cmdDraw buf 3 1 0 0
        cmdEndRenderPass buf
        endCommandBuffer buf

      imageAvailable <- track $ withSemaphore dev
      renderFinished <- track $ withSemaphore dev

      whileM_ (not <$> liftIO (windowShouldClose wnd)) $ do
        liftIO $ pollEvents

        imgIdx <- acquireNextImage dev swapchain (maxBound :: Word64) imageAvailable VK_NULL_HANDLE

        queueSubmit [ SubmitInfo { waitSemaphores = [ (imageAvailable, VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT) ]
                                 , commandBuffers = [ commandBuffers !! fromIntegral imgIdx ]
                                 , signalSemaphores = [ renderFinished ]
                                 }
                    ] VK_NULL_HANDLE graphicsQueue
        queuePresent [ renderFinished ] [ (swapchain, imgIdx) ] graphicsQueue

    liftIO $ destroyWindow wnd

  liftIO $ terminate
  where
    mkImageView dev format = withImageView
      dev
      IV.VK_IMAGE_VIEW_TYPE_2D
      format
      (IV.VkComponentMapping IV.VK_COMPONENT_SWIZZLE_IDENTITY IV.VK_COMPONENT_SWIZZLE_IDENTITY IV.VK_COMPONENT_SWIZZLE_IDENTITY IV.VK_COMPONENT_SWIZZLE_IDENTITY)
      IV.VkImageSubresourceRange { IV.vkAspectMask = VK_IMAGE_ASPECT_COLOR_BIT
                                 , IV.vkBaseMipLevel = 0
                                 , IV.vkLevelCount = 1
                                 , IV.vkBaseArrayLayer = 0
                                 , IV.vkLayerCount = 1
                                 }
