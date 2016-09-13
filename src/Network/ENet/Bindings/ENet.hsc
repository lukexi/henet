module Network.ENet.Bindings.ENet where

import Foreign.Storable
import Foreign.C
import Foreign.Ptr
import Data.Word

import Network.ENet.Bindings.Callbacks

#include "enet/enet.h"

newtype Version = Version Word32

foreign import ccall unsafe "enet.h enet_initialize"                enetInitialize
  :: IO CInt
foreign import ccall unsafe "enet.h enet_initialize_with_callbacks" enetInitializeWithCallbacks
  :: Version -> Ptr Callbacks -> IO CInt
foreign import ccall unsafe "enet.h enet_deinitialize"              enetDeinitialize
  :: IO ()
foreign import ccall unsafe "enet.h enet_linked_version"            enetLinkedVersion
  :: IO Version

foreign import ccall unsafe "enet.h enet_time_get"                  enetTimeGet
  :: IO Word32
foreign import ccall unsafe "enet.h enet_time_set"                  enetTimeSet
  :: Word32 -> IO ()


