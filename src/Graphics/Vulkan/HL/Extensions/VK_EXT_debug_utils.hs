{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}

module Graphics.Vulkan.HL.Extensions.VK_EXT_debug_utils
  ( DebugUtilsMessengerCallback
  , DebugUtilsLabel (..)
  , DebugUtilsObjectNameInfo (..)
  , DebugUtilsMessengerCallbackData (..)
  , createDebugUtilsMessenger
  , destroyDebugUtilsMessenger
  , withDebugUtilsMessenger
  ) where

import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Data.Bits
import qualified Data.Vector.Storable.Sized as SV
import Data.Word
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import Graphics.Vulkan.Core10.Core
import Graphics.Vulkan.Core10.DeviceInitialization
import Graphics.Vulkan.Dynamic
import Graphics.Vulkan.Extensions.VK_EXT_debug_utils
import Graphics.Vulkan.HL.Exception
import Linear.V4
import Util

-- TODO: INFO severity logging hangs on device creation. Potentially something to do with incorrectly terminated string
-- being marshalled forever?

type DebugUtilsMessengerCallback_Internal = VkDebugUtilsMessageSeverityFlagBitsEXT -> VkDebugUtilsMessageTypeFlagsEXT -> Ptr VkDebugUtilsMessengerCallbackDataEXT -> Ptr () -> IO VkBool32

type DebugUtilsMessengerCallback = VkDebugUtilsMessageTypeFlagsEXT -> VkDebugUtilsMessageSeverityFlagBitsEXT -> DebugUtilsMessengerCallbackData -> IO ()

data DebugUtilsLabel = DebugUtilsLabel { labelName :: String
                                       , color :: V4 Float
                                       } deriving (Show)

data DebugUtilsObjectNameInfo = DebugUtilsObjectNameInfo { objectType :: VkObjectType
                                                         , objectHandle :: Word64
                                                         , objectName :: Maybe String
                                                         } deriving (Show)

data DebugUtilsMessengerCallbackData = DebugUtilsMessengerCallbackData { messageIdName :: Maybe String
                                                                       , messageIdNumber :: Int
                                                                       , message :: String
                                                                       , queueLabels :: [DebugUtilsLabel]
                                                                       , cmdBufLabels :: [DebugUtilsLabel]
                                                                       , objects :: [DebugUtilsObjectNameInfo]
                                                                       } deriving (Show)

foreign import ccall "wrapper"
  mkDebugUtilsMessengerCallback :: DebugUtilsMessengerCallback_Internal -> IO (FunPtr DebugUtilsMessengerCallback_Internal)

convertVector4ToV4 :: SV.Vector 4 CFloat -> V4 Float
convertVector4ToV4 =
  V4 <$> item 0 <*> item 1 <*> item 2 <*> item 3
  where
    item i = convertCFloat . flip SV.index i
    convertCFloat (CFloat f) = f

convertDebugUtilsLabel :: VkDebugUtilsLabelEXT -> IO DebugUtilsLabel
convertDebugUtilsLabel label = do
  name <- peekCString (vkPLabelName label)
  return $ DebugUtilsLabel { labelName = name
                           , color = convertVector4ToV4 (vkColor label)
                           }

convertDebugUtilsObjectNameInfo :: VkDebugUtilsObjectNameInfoEXT -> IO DebugUtilsObjectNameInfo
convertDebugUtilsObjectNameInfo oni = do
  name <- maybePeek peekCString (vkPObjectName oni)
  return $ DebugUtilsObjectNameInfo { objectType = vkObjectType (oni :: VkDebugUtilsObjectNameInfoEXT)
                                    , objectHandle = vkObjectHandle (oni :: VkDebugUtilsObjectNameInfoEXT)
                                    , objectName = name
                                    }

mkInternalCallback :: DebugUtilsMessengerCallback -> DebugUtilsMessengerCallback_Internal
mkInternalCallback callback messageSeverity messageType callbackDataPtr _ = do
  callbackData <- peek callbackDataPtr
  messageIdName <- maybePeek peekCString (vkPMessageIdName callbackData)
  message <- peekCString (vkPMessage callbackData)
  queueLabelPtrs <- peekArray (fromIntegral $ vkQueueLabelCount callbackData) (vkPQueueLabels callbackData)
  cmdBufLabelPtrs <- peekArray (fromIntegral $ vkCmdBufLabelCount callbackData) (vkPCmdBufLabels callbackData)
  objectPtrs <- peekArray (fromIntegral $ vkObjectCount callbackData) (vkPObjects callbackData)
  queueLabels <- traverse convertDebugUtilsLabel queueLabelPtrs
  cmdBufLabels <- traverse convertDebugUtilsLabel cmdBufLabelPtrs
  objects <- traverse convertDebugUtilsObjectNameInfo objectPtrs
  callback messageType messageSeverity $ DebugUtilsMessengerCallbackData { messageIdName = messageIdName
                                                                         , messageIdNumber = fromIntegral $ vkMessageIdNumber callbackData
                                                                         , message = message
                                                                         , queueLabels = queueLabels
                                                                         , cmdBufLabels = cmdBufLabels
                                                                         , objects = objects
                                                                         }
  return VK_FALSE

createDebugUtilsMessenger :: MonadIO m => VkDebugUtilsMessageTypeFlagsEXT -> VkDebugUtilsMessageSeverityFlagBitsEXT -> DebugUtilsMessengerCallback -> VkInstance -> m VkDebugUtilsMessengerEXT
createDebugUtilsMessenger messageType messageSeverity callback inst = liftIO $
  mkDebugUtilsMessengerCallback (mkInternalCallback callback) >>= \debugUtilsMessengerCallbackPtr ->
  with VkDebugUtilsMessengerCreateInfoEXT { vkSType = VK_STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CREATE_INFO_EXT
                                          , vkPNext = nullPtr
                                          , vkFlags = zeroBits
                                          , vkMessageSeverity = messageSeverity
                                          , vkMessageType = messageType
                                          , vkPfnUserCallback = castFunPtrToPtr debugUtilsMessengerCallbackPtr
                                          , vkPUserData = nullPtr
                                          } $ \createInfoPtr ->
  initInstanceCmds inst >>= \instanceCmds ->
  vulkanPtrR $ createDebugUtilsMessengerEXT instanceCmds inst createInfoPtr nullPtr

destroyDebugUtilsMessenger :: MonadIO m => VkInstance -> VkDebugUtilsMessengerEXT -> m ()
destroyDebugUtilsMessenger inst messenger = liftIO $
  initInstanceCmds inst >>= \instanceCmds ->
  destroyDebugUtilsMessengerEXT instanceCmds inst messenger nullPtr

withDebugUtilsMessenger :: MonadBaseControl IO m => VkDebugUtilsMessageTypeFlagsEXT -> VkDebugUtilsMessageSeverityFlagBitsEXT -> DebugUtilsMessengerCallback -> VkInstance -> (VkDebugUtilsMessengerEXT -> m a) -> m a
withDebugUtilsMessenger messageType messageSeverity callback inst =
  liftBaseOp $ bracket (createDebugUtilsMessenger messageType messageSeverity callback inst) (destroyDebugUtilsMessenger inst)

-- submitDebugUtilsMessage :: VkDebugUtilsMessageTypeFlagsEXT -> VkDebugUtilsMessageSeverityFlagBitsEXT -> VkInstance -> DebugUtilsMessengerCallbackData -> IO ()
-- submitDebugUtilsMessage messageType messageSeverity inst callbackData =
--   with VkDebugUtilsMessengerCallbackDataEXT {  } $ \callbackDataPtr ->
--     vkSubmitDebugUtilsMessageEXT inst messageSeverity messageType callbackDataPtr
