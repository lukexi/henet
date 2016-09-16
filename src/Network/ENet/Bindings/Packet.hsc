{-# LANGUAGE CPP, EmptyDataDecls, GeneralizedNewtypeDeriving #-}


module Network.ENet.Bindings.Packet where

-- import Foreign.Storable
import Foreign.C
import Foreign.Ptr
import Data.Word

import Network.ENet.Bindings.Socket

#include "enet/enet.h"

data PacketFlag = Reliable
                | Unsequenced
                | NoAllocate
                | UnreliableFragment
                | IsSent
                | PacketFlagUnknown
                deriving (Show, Eq)

-- | Used in creation of flag set
instance Enum PacketFlag where
  fromEnum f = case f of
    Reliable -> 1
    Unsequenced -> 2
    NoAllocate -> 4
    UnreliableFragment -> 8
    IsSent -> 256
    PacketFlagUnknown -> 0
  toEnum i = case i of
    1 -> Reliable
    2 -> Unsequenced
    4 -> NoAllocate
    8 -> UnreliableFragment
    256 -> IsSent
    _ -> PacketFlagUnknown
------


data Packet


foreign import ccall unsafe "enet.h enet_packet_create"   packetCreate
  :: Ptr () -> CSize -> Word32 -> IO (Ptr Packet)
foreign import ccall unsafe "enet.h enet_packet_destroy"  packetDestroy
  :: Ptr Packet -> IO ()
foreign import ccall unsafe "enet.h enet_packet_resize"   packetResize
  :: Ptr Packet -> CSize -> IO CInt
foreign import ccall unsafe "enet.h enet_crc32"           crc32
  :: Ptr Buffer -> CSize -> IO Word32
