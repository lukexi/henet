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
  peek ptr = (toEnum . fromIntegral) `fmap` (peekByteOff ptr 0 :: IO CUInt)
  poke ptr e = pokeByteOff ptr 0 $ (fromIntegral $ fromEnum e :: CUInt)

------



data Event = Event
             EventType
             (Ptr Peer)
             ChannelID
             Word32 -- event data
             (Ptr Packet)
    deriving Show

instance Storable Event where
  sizeOf    _ = (#size ENetEvent)
  alignment _ = (#size void*) -- FIX THIS!
  peek ptr = do t  <- (#peek ENetEvent, type)      ptr
                pe <- (#peek ENetEvent, peer)      ptr
                c  <- (#peek ENetEvent, channelID) ptr
                d  <- (#peek ENetEvent, data)      ptr
                pa <- (#peek ENetEvent, packet)    ptr
                return $ Event t pe c d pa
  poke ptr (Event t pe c d pa) =
    do (#poke ENetEvent, type)      ptr t
       (#poke ENetEvent, peer)      ptr pe
       (#poke ENetEvent, channelID) ptr c
       (#poke ENetEvent, data)      ptr d
       (#poke ENetEvent, packet)    ptr pa
