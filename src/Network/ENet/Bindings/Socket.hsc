{-# LANGUAGE EmptyDataDecls, GeneralizedNewtypeDeriving #-}
module Network.ENet.Bindings.Socket where
import Foreign.Storable
import Foreign.C
import Foreign.Ptr
import Data.Word

import Network.ENet.Bindings.Address

#include "enet/enet.h"

-- wraps both win32.h and unix.h with opaque types

 -- cheating, CUInt is an accidental aggreement between systems
 -- hiding constructor to make this slightly less bad
newtype Socket = Socket CUInt
               deriving (Show, Eq, Storable)


data Buffer
data SocketSet



------

data SocketType = Stream | Datagram
                deriving (Show, Eq)

instance Enum SocketType where
  fromEnum Stream   = 1
  fromEnum Datagram = 2
  toEnum 1 = Stream
  toEnum 2 = Datagram
  toEnum _ = Datagram

instance Storable SocketType where
  sizeOf    _ = (#size ENetSocketType)
  alignment _ = (#size void*) -- FIX THIS!
  peek ptr = (toEnum . fromIntegral) `fmap` (peekByteOff ptr 0 :: IO CUInt)
  poke ptr e = pokeByteOff ptr 0 $ (fromIntegral $ fromEnum e :: CUInt)

------

data SocketOption = NonBlock
                  | Broadcast
                  | ReceiveBuffer
                  | SendBuffer
                  | ReuseAddress
                  | ReceiveTimeIs0
                  | SendTimeIs0
                  | Error
                  deriving (Show, Eq)

instance Enum SocketOption where
  fromEnum NonBlock       = 1
  fromEnum Broadcast      = 2
  fromEnum ReceiveBuffer  = 3
  fromEnum SendBuffer     = 4
  fromEnum ReuseAddress   = 5
  fromEnum ReceiveTimeIs0 = 6
  fromEnum SendTimeIs0    = 7
  fromEnum Error          = 8
  toEnum 1 = NonBlock
  toEnum 2 = Broadcast
  toEnum 3 = ReceiveBuffer
  toEnum 4 = SendBuffer
  toEnum 5 = ReuseAddress
  toEnum 6 = ReceiveTimeIs0
  toEnum 7 = SendTimeIs0
  toEnum _ = Error

instance Storable SocketOption where
  sizeOf    _ = (#size ENetSocketOption)
  alignment _ = (#size void*) -- FIX THIS!
  peek ptr = (toEnum . fromIntegral) `fmap` (peekByteOff ptr 0 :: IO CUInt)
  poke ptr e = pokeByteOff ptr 0 $ (fromIntegral $ fromEnum e :: CUInt)

------

data SocketShutdown = Read | Write | ReadWrite
                    deriving (Show, Eq, Enum) -- safe because starts on 0

instance Storable SocketShutdown where
  sizeOf    _ = (#size ENetSocketShutdown)
  alignment _ = (#size void*) -- FIX THIS!
  peek ptr = (toEnum . fromIntegral) `fmap` (peekByteOff ptr 0 :: IO CUInt)
  poke ptr e = pokeByteOff ptr 0 $ (fromIntegral $ fromEnum e :: CUInt)


 -- Socket Functions

foreign import ccall unsafe "enet.h enet_socket_create"      socketCreate
  :: CUInt -> IO Socket
foreign import ccall unsafe "enet.h enet_socket_bind"        socketBind
  :: Socket -> Ptr Address -> IO CInt
foreign import ccall unsafe "enet.h enet_socket_get_address" socketGetAddress
  :: Socket -> Ptr Address -> IO CInt
foreign import ccall unsafe "enet.h enet_socket_listen"      socketListen
  :: Socket -> CInt -> IO CInt
foreign import ccall unsafe "enet.h enet_socket_accept"      socketAccept
  :: Socket -> Ptr Address -> IO Socket
foreign import ccall unsafe "enet.h enet_socket_connect"     socketConnect
  :: Socket -> Ptr Address -> IO CInt
foreign import ccall unsafe "enet.h enet_socket_send"        socketSend
  :: Socket -> Ptr Address -> Ptr Buffer -> CSize -> IO CInt
foreign import ccall unsafe "enet.h enet_socket_receive"     socketReceive
  :: Socket -> Ptr Address -> Ptr Buffer -> CSize -> IO CInt
foreign import ccall unsafe "enet.h enet_socket_wait"        socketWait
  :: Socket -> Ptr Word32 -> Ptr Word32 -> IO CInt
foreign import ccall unsafe "enet.h enet_socket_set_option"  socketSetOption
  :: Socket -> CUInt -> CInt -> IO CInt
foreign import ccall unsafe "enet.h enet_socket_get_option"  socketGetOption
  :: Socket -> CUInt -> Ptr CInt -> IO CInt
foreign import ccall unsafe "enet.h enet_socket_shutdown"    socketShutdown
  :: Socket -> CUInt -> IO CInt
foreign import ccall unsafe "enet.h enet_socket_destroy"     socketDestroy
  :: Socket -> IO ()
foreign import ccall unsafe "enet.h enet_socketset_select"   socketSelectSet
  :: Socket -> Ptr SocketSet -> Ptr SocketSet -> Word32 -> IO CInt
