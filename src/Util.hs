module Util where

import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Vector.Storable.Sized as SV
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import Graphics.Vulkan.Core10.Core
import Graphics.Vulkan.HL.Exception

sizedVectorToString :: SV.Vector s CChar -> IO String
sizedVectorToString vec =
  withArray (SV.toList vec) peekCString

sizedVectorToList :: Storable a => Word32 -> SV.Vector s a -> [a]
sizedVectorToList count = take (fromIntegral count) . SV.toList

withCStringArrayLen :: [String] -> (Int -> Ptr CString -> IO a) -> IO a
withCStringArrayLen strings action =
  withMany withCString strings $ \stringPtrs ->
  withArrayLen stringPtrs action

-- withCStringArrayLen :: [String] -> (Int -> Ptr CString -> IO a) -> IO a
-- withCStringArrayLen strings action = flip runCont (uncurry action) $ do
--   strs <- traverse (cont . withCString) strings
--   cont $ withArrayLen strs . curry

vulkanEnumeration :: Storable a => (Ptr Word32 -> Ptr a -> IO ()) -> IO [a]
vulkanEnumeration f =
  alloca $ \lenPtr -> do
    f lenPtr nullPtr
    len <- fromIntegral <$> peek lenPtr
    allocaArray len $ \arr -> do
      f lenPtr arr
      peekArray len arr

vulkanEnumerationR :: Storable a => (Ptr Word32 -> Ptr a -> IO VkResult) -> IO [a]
vulkanEnumerationR f = vulkanEnumeration (\len ptr -> guardVkResult =<< f len ptr)

vulkanPtr :: Storable a => (Ptr a -> IO ()) -> IO a
vulkanPtr f =
  alloca $ \ptr -> do
    f ptr
    peek ptr

vulkanPtrR :: Storable a => (Ptr a -> IO VkResult) -> IO a
vulkanPtrR = vulkanPtr . (guardVkResult <=<)

vulkanArray :: Storable a => Int -> (Ptr a -> IO ()) -> IO [a]
vulkanArray size f =
  allocaArray size $ \arr -> do
    f arr
    peekArray size arr

vulkanArrayR :: Storable a => Int -> (Ptr a -> IO VkResult) -> IO [a]
vulkanArrayR size = vulkanArray size . (guardVkResult <=<)
