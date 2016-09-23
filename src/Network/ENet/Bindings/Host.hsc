{-# LANGUAGE CPP, EmptyDataDecls, ForeignFunctionInterface #-}
module Network.ENet.Bindings.Host where

import Foreign
import Foreign.C.Types

import Network.ENet.Bindings.Peer
import Network.ENet.Bindings.Address
import Network.ENet.Bindings.Packet
import Network.ENet.Bindings.Event

#include "enet/enet.h"
#include "extra.h"

data Host

foreign import ccall unsafe "enet.h enet_host_create"                    hostCreate
  :: Ptr Address -> CSize -> CSize -> Word32 -> Word32 -> IO (Ptr Host)
foreign import ccall unsafe "enet.h enet_host_destroy"                   hostDestroy
  :: Ptr Host -> IO ()
foreign import ccall unsafe "enet.h enet_host_connect"                   hostConnect
  :: Ptr Host -> Ptr Address -> CSize -> Word32 -> IO (Ptr Peer)
foreign import ccall unsafe "enet.h enet_host_check_events"              hostCheckEvents
  :: Ptr Host -> Ptr Event -> IO CUInt
foreign import ccall unsafe "enet.h enet_host_service"                   hostService
  :: Ptr Host -> Ptr Event -> Word32 -> IO CUInt
foreign import ccall unsafe "enet.h enet_host_flush"                     hostFlush
  :: Ptr Host -> IO ()
foreign import ccall unsafe "enet.h enet_host_broadcast"                 hostBroadcast
  :: Ptr Host -> ChannelID -> Ptr Packet -> IO ()
foreign import ccall unsafe "enet.h enet_host_compress"                  hostCompress
  :: Ptr Host -> Ptr Compressor -> IO ()
foreign import ccall unsafe "enet.h enet_host_compress_with_range_coder" hostCompressWithRangeCoder
  :: Ptr Host -> IO CUInt
foreign import ccall unsafe "enet.h enet_host_channel_limit"             hostChannelLimit
  :: Ptr Host -> CSize -> IO ()
foreign import ccall unsafe "enet.h enet_host_bandwidth_limit"           hostBandwidthLimit
  :: Ptr Host -> Word32 -> Word32 -> IO ()


foreign import ccall unsafe "extra.h enet_host_broadcast_except"         hostBroadcastExcept
  :: Ptr Host -> Ptr Peer -> ChannelID -> Ptr Packet -> IO ()
