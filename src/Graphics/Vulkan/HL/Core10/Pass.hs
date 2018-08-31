{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Graphics.Vulkan.HL.Core10.Pass
  ( SubpassDescription (..)
  , createFramebuffer
  , destroyFramebuffer
  , withFramebuffer
  , createRenderPass
  , destroyRenderPass
  , withRenderPass
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
import Graphics.Vulkan.Core10.ImageView
import Graphics.Vulkan.Core10.Pass
import Graphics.Vulkan.Core10.Pipeline
import Graphics.Vulkan.HL.Exception
import Util

data SubpassDescription = SubpassDescription { flags :: VkSubpassDescriptionFlags
                                             , pipelineBindPoint :: VkPipelineBindPoint
                                             , inputAttachments :: [VkAttachmentReference]
                                             , colorAttachments :: [VkAttachmentReference]
                                             , resolveAttachments :: Maybe [VkAttachmentReference]
                                             , depthStencilAttachment :: Maybe VkAttachmentReference
                                             , preserveAttachments :: [Word32]
                                             }

convertSubpassDescription :: SubpassDescription -> IO VkSubpassDescription
convertSubpassDescription SubpassDescription{..} =
  withArrayLen inputAttachments $ \inputAttachmentsLen inputAttachmentsPtr ->
  withArrayLen colorAttachments $ \colorAttachmentsLen colorAttachmentsPtr ->
  maybeWith withArray resolveAttachments $ \resolveAttachmentsPtr ->
  maybeWith with depthStencilAttachment $ \depthStencilAttachmentPtr ->
  withArrayLen preserveAttachments $ \preserveAttachmentsLen preserveAttachmentsPtr ->
  return $ VkSubpassDescription { vkFlags = flags
                                , vkPipelineBindPoint = pipelineBindPoint
                                , vkInputAttachmentCount = fromIntegral inputAttachmentsLen
                                , vkPInputAttachments = inputAttachmentsPtr
                                , vkColorAttachmentCount = fromIntegral colorAttachmentsLen
                                , vkPColorAttachments = colorAttachmentsPtr
                                , vkPResolveAttachments = resolveAttachmentsPtr
                                , vkPDepthStencilAttachment = depthStencilAttachmentPtr
                                , vkPreserveAttachmentCount = fromIntegral preserveAttachmentsLen
                                , vkPPreserveAttachments = preserveAttachmentsPtr
                                }

createFramebuffer :: MonadIO m => VkDevice -> VkRenderPass -> Word32 -> Word32 -> Word32 -> [VkImageView] -> m VkFramebuffer
createFramebuffer dev renderPass width height layers attachments = liftIO $
  withArrayLen attachments $ \attachmentsLen attachmentsPtr ->
  with VkFramebufferCreateInfo { vkSType = VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO
                               , vkPNext = nullPtr
                               , vkFlags = zeroBits
                               , vkRenderPass = renderPass
                               , vkAttachmentCount = fromIntegral attachmentsLen
                               , vkPAttachments = attachmentsPtr
                               , vkWidth = width
                               , vkHeight = height
                               , vkLayers = layers
                               } $ \createInfoPtr ->
  vulkanPtrR $ vkCreateFramebuffer dev createInfoPtr nullPtr

destroyFramebuffer :: MonadIO m => VkDevice -> VkFramebuffer -> m ()
destroyFramebuffer dev frameBuffer =
  liftIO $ vkDestroyFramebuffer dev frameBuffer nullPtr

withFramebuffer :: MonadBaseControl IO m => VkDevice -> VkRenderPass -> Word32 -> Word32 -> Word32 -> [VkImageView] -> (VkFramebuffer -> m a) -> m a
withFramebuffer dev renderPass width height layers attachments =
  liftBaseOp $ bracket (createFramebuffer dev renderPass width height layers attachments) (destroyFramebuffer dev)

createRenderPass :: MonadIO m => VkDevice -> [VkAttachmentDescription] -> [SubpassDescription] -> [VkSubpassDependency] -> m VkRenderPass
createRenderPass dev attachments subpasses dependencies = liftIO $ do
  convertedSubpasses <- traverse convertSubpassDescription subpasses
  withArrayLen attachments $ \attachmentsLen attachmentsPtr ->
    withArrayLen convertedSubpasses $ \subpassesLen subpassesPtr ->
    withArrayLen dependencies $ \dependenciesLen dependenciesPtr ->
    with VkRenderPassCreateInfo { vkSType = VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO
                                , vkPNext = nullPtr
                                , vkFlags = zeroBits
                                , vkAttachmentCount = fromIntegral attachmentsLen
                                , vkPAttachments = attachmentsPtr
                                , vkSubpassCount = fromIntegral subpassesLen
                                , vkPSubpasses = subpassesPtr
                                , vkDependencyCount = fromIntegral dependenciesLen
                                , vkPDependencies = dependenciesPtr
                                } $ \createInfoPtr ->
    vulkanPtrR $ vkCreateRenderPass dev createInfoPtr nullPtr

destroyRenderPass :: MonadIO m => VkDevice -> VkRenderPass -> m ()
destroyRenderPass dev renderPass =
  liftIO $ vkDestroyRenderPass dev renderPass nullPtr

withRenderPass :: MonadBaseControl IO m => VkDevice -> [VkAttachmentDescription] -> [SubpassDescription] -> [VkSubpassDependency] -> (VkRenderPass -> m a) -> m a
withRenderPass dev attachments subpasses dependencies =
  liftBaseOp $ bracket (createRenderPass dev attachments subpasses dependencies) (destroyRenderPass dev)
