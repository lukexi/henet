{-# LANGUAGE CPP, EmptyDataDecls, GeneralizedNewtypeDeriving #-}

module Network.ENet.Bindings.Event where

import Foreign.Storable
import Foreign.C
import Foreign.Ptr
import Data.Word

import Network.ENet.Bindings.Peer
import Network.ENet.Bindings.Packet

#include "enet/enet.h"


data Acknowledgement
data OutgoingCommand
data IncomingCommand
data PeerState
data Channel
data Compressor


data EventType = None | Connect | Disconnect | Receive
               deriving (Show, Eq, Enum) -- safe because starts on 0

instance Storable EventType where
  sizeOf    _ = (#size ENetEventType)
  alignment _ = (#size void*) -- FIX THIS!
  peek ptr    = toEnum . fromIntegral <$> (peekByteOff ptr 0 :: IO CUInt)
  poke ptr e  = pokeByteOff ptr 0 $ (fromIntegral $ fromEnum e :: CUInt)

------



data Event = Event
    { evtType       :: !EventType
    , evtPeer       :: !(Ptr Peer)
    , evtChannnelID :: !ChannelID
    , evtDataLength :: !Word32
    , evtPacket     :: !(Ptr Packet)
    }
    deriving Show

instance Storable Event where
    sizeOf    _ = (#size ENetEvent)
    alignment _ = (#size void*) -- FIX THIS!
    peek ptr = Event
        <$> (#peek ENetEvent, type)      ptr
        <*> (#peek ENetEvent, peer)      ptr
        <*> (#peek ENetEvent, channelID) ptr
        <*> (#peek ENetEvent, data)      ptr
        <*> (#peek ENetEvent, packet)    ptr
    poke ptr (Event t pe c d pa) = do
        (#poke ENetEvent, type)      ptr t
        (#poke ENetEvent, peer)      ptr pe
        (#poke ENetEvent, channelID) ptr c
        (#poke ENetEvent, data)      ptr d
        (#poke ENetEvent, packet)    ptr pa

