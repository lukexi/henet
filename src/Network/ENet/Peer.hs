module Network.ENet.Peer
    ( module Network.ENet.Peer
    , module Exports
    ) where

import Foreign
import Foreign.C.Error

import qualified Network.ENet.Bindings.Peer as B
import qualified Network.ENet.Bindings.Packet as B
import Network.ENet.Bindings.Peer as Exports hiding (peerSend, peerReceive)

peerSend :: Ptr B.Peer -> B.ChannelID -> Ptr B.Packet -> IO ()
peerSend peer cID packet = do
    _ <- throwErrnoIf
       (/=0)
       "could not send packet"
       $ B.peerSend peer cID packet
    return ()

peerReceive :: Ptr B.Peer -> B.ChannelID -> IO (Maybe (Ptr B.Packet))
peerReceive peer cID = do
    ptr <- B.peerReceive peer cID
    return $ if ptr == nullPtr
       then Nothing
       else Just ptr

