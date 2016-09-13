{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module Network.ENet.Packet
    ( module Network.ENet.Packet
    , module Exports
    ) where

import Foreign
import Foreign.C.Types

import Data.ByteString.Unsafe

import Data.ByteString (ByteString)
import Data.Word (Word32)
import Data.Bits ((.|.), (.&.))

import Data.Foldable


import qualified Network.ENet.Bindings.Packet as B
import Network.ENet.Bindings.Packet as Exports
    hiding (packetResize, Packet)

#include "enet/enet.h"

-- | Convert hENet packet to ENet packet.
{-|
Make sure 'NoAllocate' is not a member of the PacketFlag bitset, or its possible
the data of the packet will be GC'd before ENet can handle the packet. The
packet data will be copied by ENet, and thus the packet must be freed with
destroy.
-}
-- this ^ is why unsafe is OK
packetPoke :: Packet -> IO (Ptr B.Packet)
packetPoke (Packet f bs) = unsafeUseAsCStringLen bs $ \(ptr, len) ->
  B.packetCreate (castPtr ptr) (fromIntegral len) $ unPacketFlagSet f


-- | Convert ENet packet to hENet packet.
{-|
While the flag set is copied, the data will /not/ be copied. Instead the
ByteString will have destroy as it's finalizer. This means the original pointer
is no longer safe to use as it will become invalid whenever returned the hENet
packet is collected.
-}
packetPeek :: Ptr B.Packet -> IO Packet
packetPeek ptr = do
    f <- (#peek ENetPacket, flags     ) ptr
    p <- (#peek ENetPacket, data      ) ptr
    l <- (#peek ENetPacket, dataLength) ptr
    b <- unsafePackCStringFinalizer p l $ B.packetDestroy ptr
    return $ Packet (PacketFlagSet f) b

packetResize :: Ptr B.Packet -> CSize -> IO Bool
packetResize ptr size =
    (/=0) <$> B.packetResize ptr size



-- | Build from combining B.PacketFlag
newtype PacketFlagSet = PacketFlagSet { unPacketFlagSet :: Word32 }

makePacketFlagSet :: (Foldable t, Functor t) => (t B.PacketFlag) -> PacketFlagSet
makePacketFlagSet = PacketFlagSet . foldl' (.|.) 0 . fmap (fromIntegral . fromEnum)

unpackPacketFlagSet :: PacketFlagSet -> [B.PacketFlag]
unpackPacketFlagSet (PacketFlagSet w) = filter ((/= 0) . (w .&.) . fromIntegral . fromEnum) [B.Reliable .. B.IsSent]

emptyPacketFlagSet :: PacketFlagSet
emptyPacketFlagSet = PacketFlagSet 0

instance Show PacketFlagSet where
  show = show . unpackPacketFlagSet

-- | A more high level notion of a packet than used by the basic Bindings
data Packet = Packet PacketFlagSet ByteString
