{-# LANGUAGE CPP, EmptyDataDecls, GeneralizedNewtypeDeriving #-}

module Network.ENet.Bindings.Peer where

import Data.Word
import Foreign.Ptr
import Foreign.C
import Foreign.Storable
import Network.ENet.Bindings.Packet

#include "enet/enet.h"

-- | Wrapper for channel index
newtype ChannelID = ChannelID Word8
                  deriving (Show, Eq, Storable)

data Peer

foreign import ccall unsafe "enet.h enet_peer_send"               peerSend
  :: Ptr Peer -> ChannelID -> Ptr Packet -> IO CInt
foreign import ccall unsafe "enet.h enet_peer_receive"            peerReceive
  :: Ptr Peer -> ChannelID -> IO (Ptr Packet)
foreign import ccall unsafe "enet.h enet_peer_ping"               peerPing
  :: Ptr Peer -> IO ()
foreign import ccall unsafe "enet.h enet_peer_ping_interval"      peerPingInterval
  :: Ptr Peer -> Word32 -> IO ()
foreign import ccall unsafe "enet.h enet_peer_timeout"            peerTimeout
  :: Ptr Peer -> Word32 -> Word32 -> Word32 -> IO ()
foreign import ccall unsafe "enet.h enet_peer_reset"              peerReset
  :: Ptr Peer -> IO ()
foreign import ccall unsafe "enet.h enet_peer_disconnect"         peerDisconnect
  :: Ptr Peer -> Word32 -> IO ()
foreign import ccall unsafe "enet.h enet_peer_disconnect_now"     peerDisconnectNow
  :: Ptr Peer -> Word32 -> IO ()
foreign import ccall unsafe "enet.h enet_peer_disconnect_later"   peerDisconnectLater
  :: Ptr Peer -> Word32 -> IO ()
foreign import ccall unsafe "enet.h enet_peer_throttle_configure" peerThrottleConfigure
  :: Ptr Peer -> Word32 -> Word32 -> Word32 -> IO ()
