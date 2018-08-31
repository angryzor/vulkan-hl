{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Graphics.Vulkan.HL.Core10.Pipeline
  ( PipelineShaderStageCreateInfo (..)
  , PipelineVertexInputStateCreateInfo (..)
  , PipelineInputAssemblyStateCreateInfo (..)
  , PipelineTessellationStateCreateInfo (..)
  , PipelineViewportStateCreateInfo (..)
  , PipelineRasterizationStateCreateInfo (..)
  , PipelineMultisampleStateCreateInfo (..)
  , PipelineDepthStencilStateCreateInfo (..)
  , PipelineColorBlendStateCreateInfo (..)
  , PipelineDynamicStateCreateInfo (..)
  , GraphicsPipelineCreateInfo (..)
  , createGraphicsPipelines
  , destroyPipeline
  , withGraphicsPipelines
  ) where

import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Data.Bits
import qualified Data.Vector.Storable.Sized as SV
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal
import Foreign.Marshal.Array
import Graphics.Vulkan.Core10.Core
import Graphics.Vulkan.Core10.DeviceInitialization
import Graphics.Vulkan.Core10.Pipeline
import Graphics.Vulkan.Core10.PipelineCache
import Graphics.Vulkan.Core10.PipelineLayout
import Graphics.Vulkan.Core10.Shader
import Graphics.Vulkan.HL.Exception
import Linear.V4
import Util

data PipelineShaderStageCreateInfo = PipelineShaderStageCreateInfo { stage :: VkShaderStageFlagBits
                                                                   , shaderModule :: VkShaderModule
                                                                   , name :: String
                                                                   --  , specializationInfo :: Maybe SpecializationInfo-- TODO: Implement
                                                                   }

data PipelineVertexInputStateCreateInfo = PipelineVertexInputStateCreateInfo { vertexBindingDescriptions :: [VkVertexInputBindingDescription]
                                                                             , vertexAttributeDescriptions :: [VkVertexInputAttributeDescription]
                                                                             }

data PipelineInputAssemblyStateCreateInfo = PipelineInputAssemblyStateCreateInfo { topology :: VkPrimitiveTopology
                                                                                 , primitiveRestartEnable :: VkBool32
                                                                                 }

data PipelineTessellationStateCreateInfo = PipelineTessellationStateCreateInfo { patchControlPoints :: Word32
                                                                               }

data PipelineViewportStateCreateInfo = PipelineViewportStateCreateInfo { viewports :: [VkViewport]
                                                                       , scissors :: [VkRect2D]
                                                                       }

data PipelineRasterizationStateCreateInfo = PipelineRasterizationStateCreateInfo { depthClampEnable :: VkBool32
                                                                                 , rasterizerDiscardEnable :: VkBool32
                                                                                 , polygonMode :: VkPolygonMode
                                                                                 , cullMode :: VkCullModeFlags
                                                                                 , frontFace :: VkFrontFace
                                                                                 , depthBiasEnable :: VkBool32
                                                                                 , depthBiasConstantFactor :: Float
                                                                                 , depthBiasClamp :: Float
                                                                                 , depthBiasSlopeFactor :: Float
                                                                                 , lineWidth :: Float
                                                                                 }

data PipelineMultisampleStateCreateInfo = PipelineMultisampleStateCreateInfo { rasterizationSamples :: VkSampleCountFlagBits
                                                                             , sampleShadingEnable :: VkBool32
                                                                             , minSampleShading :: Float
                                                                             , sampleMask :: Maybe VkSampleMask
                                                                             , alphaToCoverageEnable :: VkBool32
                                                                             , alphaToOneEnable :: VkBool32
                                                                             }

data PipelineDepthStencilStateCreateInfo = PipelineDepthStencilStateCreateInfo { depthTestEnable :: VkBool32
                                                                               , depthWriteEnable :: VkBool32
                                                                               , depthCompareOp :: VkCompareOp
                                                                               , depthBoundsTestEnable :: VkBool32
                                                                               , stencilTestEnable :: VkBool32
                                                                               , front :: VkStencilOpState
                                                                               , back :: VkStencilOpState
                                                                               , minDepthBounds :: Float
                                                                               , maxDepthBounds :: Float
                                                                               }

data PipelineColorBlendStateCreateInfo = PipelineColorBlendStateCreateInfo { logicOpEnable :: VkBool32
                                                                           , logicOp :: VkLogicOp
                                                                           , attachments :: [VkPipelineColorBlendAttachmentState]
                                                                           , blendConstants :: V4 Float
                                                                           }

data PipelineDynamicStateCreateInfo = PipelineDynamicStateCreateInfo { dynamicStates :: [VkDynamicState]
                                                                     }

data GraphicsPipelineCreateInfo = GraphicsPipelineCreateInfo { flags :: VkPipelineCreateFlagBits
                                                             , stages :: [PipelineShaderStageCreateInfo]
                                                             , vertexInputState :: PipelineVertexInputStateCreateInfo
                                                             , inputAssemblyState :: PipelineInputAssemblyStateCreateInfo
                                                             , tessellationState :: Maybe PipelineTessellationStateCreateInfo
                                                             , viewportState :: Maybe PipelineViewportStateCreateInfo
                                                             , rasterizationState :: PipelineRasterizationStateCreateInfo
                                                             , multisampleState :: Maybe PipelineMultisampleStateCreateInfo
                                                             , depthStencilState :: Maybe PipelineDepthStencilStateCreateInfo
                                                             , colorBlendState :: Maybe PipelineColorBlendStateCreateInfo
                                                             , dynamicState :: Maybe PipelineDynamicStateCreateInfo
                                                             , layout :: VkPipelineLayout
                                                             , renderPass :: VkRenderPass
                                                             , subpass :: Word32
                                                             , basePipelineHandle :: VkPipeline
                                                             , basePipelineIndex :: Int
                                                             }

convertPipelineShaderStageCreateInfo :: PipelineShaderStageCreateInfo -> IO VkPipelineShaderStageCreateInfo
convertPipelineShaderStageCreateInfo PipelineShaderStageCreateInfo{..} =
  withCString name $ \namePtr ->
  return $ VkPipelineShaderStageCreateInfo { vkSType = VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO
                                           , vkPNext = nullPtr
                                           , vkFlags = zeroBits
                                           , vkStage = stage
                                           , vkModule = shaderModule
                                           , vkPName = namePtr
                                           , vkPSpecializationInfo = nullPtr -- TODO: Implement
                                           }

convertPipelineVertexInputStateCreateInfo :: PipelineVertexInputStateCreateInfo -> IO VkPipelineVertexInputStateCreateInfo
convertPipelineVertexInputStateCreateInfo PipelineVertexInputStateCreateInfo{..} =
  withArrayLen vertexBindingDescriptions $ \vertexBindingDescriptionsLen vertexBindingDescriptionsPtr ->
  withArrayLen vertexAttributeDescriptions $ \vertexAttributeDescriptionsLen vertexAttributeDescriptionsPtr ->
  return $ VkPipelineVertexInputStateCreateInfo { vkSType = VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO
                                                , vkPNext = nullPtr
                                                , vkFlags = zeroBits
                                                , vkVertexBindingDescriptionCount = fromIntegral vertexBindingDescriptionsLen
                                                , vkPVertexBindingDescriptions = vertexBindingDescriptionsPtr
                                                , vkVertexAttributeDescriptionCount = fromIntegral vertexAttributeDescriptionsLen
                                                , vkPVertexAttributeDescriptions = vertexAttributeDescriptionsPtr
                                                }

convertPipelineInputAssemblyStateCreateInfo :: PipelineInputAssemblyStateCreateInfo -> VkPipelineInputAssemblyStateCreateInfo
convertPipelineInputAssemblyStateCreateInfo PipelineInputAssemblyStateCreateInfo{..} =
  VkPipelineInputAssemblyStateCreateInfo { vkSType = VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO
                                         , vkPNext = nullPtr
                                         , vkFlags = zeroBits
                                         , vkTopology = topology
                                         , vkPrimitiveRestartEnable = primitiveRestartEnable
                                         }

convertPipelineTessellationStateCreateInfo :: PipelineTessellationStateCreateInfo -> VkPipelineTessellationStateCreateInfo
convertPipelineTessellationStateCreateInfo PipelineTessellationStateCreateInfo{..} =
  VkPipelineTessellationStateCreateInfo { vkSType = VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_STATE_CREATE_INFO
                                         , vkPNext = nullPtr
                                         , vkFlags = zeroBits
                                         , vkPatchControlPoints = patchControlPoints
                                         }

convertPipelineViewportStateCreateInfo :: PipelineViewportStateCreateInfo -> IO VkPipelineViewportStateCreateInfo
convertPipelineViewportStateCreateInfo PipelineViewportStateCreateInfo{..} =
  withArrayLen viewports $ \viewportsLen viewportsPtr ->
  withArrayLen scissors $ \scissorsLen scissorsPtr ->
  return $ VkPipelineViewportStateCreateInfo { vkSType = VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO
                                             , vkPNext = nullPtr
                                             , vkFlags = zeroBits
                                             , vkViewportCount = fromIntegral viewportsLen
                                             , vkPViewports = viewportsPtr
                                             , vkScissorCount = fromIntegral scissorsLen
                                             , vkPScissors = scissorsPtr
                                             }

convertPipelineRasterizationStateCreateInfo :: PipelineRasterizationStateCreateInfo -> VkPipelineRasterizationStateCreateInfo
convertPipelineRasterizationStateCreateInfo PipelineRasterizationStateCreateInfo{..} =
  VkPipelineRasterizationStateCreateInfo { vkSType = VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO
                                         , vkPNext = nullPtr
                                         , vkFlags = zeroBits
                                         , vkDepthClampEnable = depthClampEnable
                                         , vkRasterizerDiscardEnable = rasterizerDiscardEnable
                                         , vkPolygonMode = polygonMode
                                         , vkCullMode = cullMode
                                         , vkFrontFace = frontFace
                                         , vkDepthBiasEnable = depthBiasEnable
                                         , vkDepthBiasConstantFactor = CFloat depthBiasConstantFactor
                                         , vkDepthBiasClamp = CFloat depthBiasClamp
                                         , vkDepthBiasSlopeFactor = CFloat depthBiasSlopeFactor
                                         , vkLineWidth = CFloat lineWidth
                                         }

convertPipelineMultisampleStateCreateInfo :: PipelineMultisampleStateCreateInfo -> IO VkPipelineMultisampleStateCreateInfo
convertPipelineMultisampleStateCreateInfo PipelineMultisampleStateCreateInfo{..} =
  maybeWith with sampleMask $ \sampleMaskPtr ->
  return $ VkPipelineMultisampleStateCreateInfo { vkSType = VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO
                                                , vkPNext = nullPtr
                                                , vkFlags = zeroBits
                                                , vkRasterizationSamples = rasterizationSamples
                                                , vkSampleShadingEnable = sampleShadingEnable
                                                , vkMinSampleShading = CFloat minSampleShading
                                                , vkPSampleMask = sampleMaskPtr
                                                , vkAlphaToCoverageEnable = alphaToCoverageEnable
                                                , vkAlphaToOneEnable = alphaToOneEnable
                                                }

convertPipelineDepthStencilStateCreateInfo :: PipelineDepthStencilStateCreateInfo -> VkPipelineDepthStencilStateCreateInfo
convertPipelineDepthStencilStateCreateInfo PipelineDepthStencilStateCreateInfo{..} =
  VkPipelineDepthStencilStateCreateInfo { vkSType = VK_STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO
                                         , vkPNext = nullPtr
                                         , vkFlags = zeroBits
                                         , vkDepthTestEnable = depthTestEnable
                                         , vkDepthWriteEnable = depthWriteEnable
                                         , vkDepthCompareOp = depthCompareOp
                                         , vkDepthBoundsTestEnable = depthBoundsTestEnable
                                         , vkStencilTestEnable = stencilTestEnable
                                         , vkFront = front
                                         , vkBack = back
                                         , vkMinDepthBounds = CFloat minDepthBounds
                                         , vkMaxDepthBounds = CFloat maxDepthBounds
                                         }

convertPipelineColorBlendStateCreateInfo :: PipelineColorBlendStateCreateInfo -> IO VkPipelineColorBlendStateCreateInfo
convertPipelineColorBlendStateCreateInfo PipelineColorBlendStateCreateInfo{..} =
  withArrayLen attachments $ \attachmentsLen attachmentsPtr ->
  return $ VkPipelineColorBlendStateCreateInfo { vkSType = VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO
                                               , vkPNext = nullPtr
                                               , vkFlags = zeroBits
                                               , vkLogicOpEnable = logicOpEnable
                                               , vkLogicOp = logicOp
                                               , vkAttachmentCount = fromIntegral attachmentsLen
                                               , vkPAttachments = attachmentsPtr
                                               , vkBlendConstants = let V4 x y z w = blendConstants in SV.fromTuple (CFloat x, CFloat y, CFloat z, CFloat w)
                                               }

convertPipelineDynamicStateCreateInfo :: PipelineDynamicStateCreateInfo -> IO VkPipelineDynamicStateCreateInfo
convertPipelineDynamicStateCreateInfo PipelineDynamicStateCreateInfo{..} =
  withArrayLen dynamicStates $ \dynamicStatesLen dynamicStatesPtr ->
  return $ VkPipelineDynamicStateCreateInfo { vkSType = VK_STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO
                                            , vkPNext = nullPtr
                                            , vkFlags = zeroBits
                                            , vkDynamicStateCount = fromIntegral dynamicStatesLen
                                            , vkPDynamicStates = dynamicStatesPtr
                                            }

convertGraphicsPipelineCreateInfo :: GraphicsPipelineCreateInfo -> IO VkGraphicsPipelineCreateInfo
convertGraphicsPipelineCreateInfo GraphicsPipelineCreateInfo{..} = do
  convertedStages <- traverse convertPipelineShaderStageCreateInfo stages
  convertedVertexInputState <- convertPipelineVertexInputStateCreateInfo vertexInputState
  let convertedInputAssemblyState = convertPipelineInputAssemblyStateCreateInfo inputAssemblyState
  let convertedTesselationState = convertPipelineTessellationStateCreateInfo <$> tessellationState
  convertedViewportState <- maybe (return Nothing) (fmap Just . convertPipelineViewportStateCreateInfo) viewportState
  let convertedRasterizationState = convertPipelineRasterizationStateCreateInfo rasterizationState
  convertedMultisampleState <- maybe (return Nothing) (fmap Just . convertPipelineMultisampleStateCreateInfo) multisampleState
  let convertedDepthStencilState = convertPipelineDepthStencilStateCreateInfo <$> depthStencilState
  convertedColorBlendState <- maybe (return Nothing) (fmap Just . convertPipelineColorBlendStateCreateInfo) colorBlendState
  convertedDynamicState <- maybe (return Nothing) (fmap Just . convertPipelineDynamicStateCreateInfo) dynamicState
  withArrayLen convertedStages $ \stagesLen stagesPtr ->
    with convertedVertexInputState $ \convertedVertexInputStatePtr ->
    with convertedInputAssemblyState $ \convertedInputAssemblyStatePtr ->
    maybeWith with convertedTesselationState $ \convertedTesselationStatePtr ->
    maybeWith with convertedViewportState $ \convertedViewportStatePtr ->
    with convertedRasterizationState $ \convertedRasterizationStatePtr ->
    maybeWith with convertedMultisampleState $ \convertedMultisampleStatePtr ->
    maybeWith with convertedDepthStencilState $ \convertedDepthStencilStatePtr ->
    maybeWith with convertedColorBlendState $ \convertedColorBlendStatePtr ->
    maybeWith with convertedDynamicState $ \convertedDynamicStatePtr ->
    return $ VkGraphicsPipelineCreateInfo { vkSType = VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO
                                          , vkPNext = nullPtr
                                          , vkFlags = flags
                                          , vkStageCount = fromIntegral stagesLen
                                          , vkPStages = stagesPtr
                                          , vkPVertexInputState = convertedVertexInputStatePtr
                                          , vkPInputAssemblyState = convertedInputAssemblyStatePtr
                                          , vkPTessellationState = convertedTesselationStatePtr
                                          , vkPViewportState = convertedViewportStatePtr
                                          , vkPRasterizationState = convertedRasterizationStatePtr
                                          , vkPMultisampleState = convertedMultisampleStatePtr
                                          , vkPDepthStencilState = convertedDepthStencilStatePtr
                                          , vkPColorBlendState = convertedColorBlendStatePtr
                                          , vkPDynamicState = convertedDynamicStatePtr
                                          , vkLayout = layout
                                          , vkRenderPass = renderPass
                                          , vkSubpass = subpass
                                          , vkBasePipelineHandle = basePipelineHandle
                                          , vkBasePipelineIndex = fromIntegral basePipelineIndex
                                          }

-- TODO: Correctly handle partial failures (Vulkan Spec Section 9.4)
createGraphicsPipelines :: MonadIO m => VkDevice -> VkPipelineCache -> [GraphicsPipelineCreateInfo] -> m [VkPipeline]
createGraphicsPipelines dev cache infos = liftIO $ do
  convertedInfos <- traverse convertGraphicsPipelineCreateInfo infos
  withArrayLen convertedInfos $ \infosLen infosPtr ->
    vulkanArrayR infosLen $ vkCreateGraphicsPipelines dev cache (fromIntegral infosLen) infosPtr nullPtr

destroyPipeline :: MonadIO m => VkDevice -> VkPipeline -> m ()
destroyPipeline dev pipeline =
  liftIO $ vkDestroyPipeline dev pipeline nullPtr

withGraphicsPipelines :: MonadBaseControl IO m => VkDevice -> VkPipelineCache -> [GraphicsPipelineCreateInfo] -> ([VkPipeline] -> m a) -> m a
withGraphicsPipelines dev cache infos =
  liftBaseOp $ bracket (createGraphicsPipelines dev cache infos) (traverse (destroyPipeline dev))
