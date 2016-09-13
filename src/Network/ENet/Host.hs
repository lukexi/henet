module Network.ENet.Host
    ( module Network.ENet.Host
    , module Exports
    )
    where

import Foreign
import Foreign.C.Types

import Network.Socket(SockAddr)

import Network.ENet.Bindings.Address

import qualified Network.ENet.Bindings.Host as B
import qualified Network.ENet.Bindings.Event as B
import qualified Network.ENet.Bindings.Peer as B
import Network.ENet.Bindings.Host as Exports hiding
    (hostCreate, hostConnect, hostService, hostCheckEvents)

withMaybeToNull :: Storable a => Maybe a -> (Ptr a -> IO b) -> IO b
withMaybeToNull (Just val) f = alloca $ \ptr -> poke ptr val >> f ptr
withMaybeToNull Nothing    f = f nullPtr

hostCreate :: Maybe SockAddr -> CSize -> CSize -> Word32 -> Word32 -> IO (Maybe (Ptr B.Host))
hostCreate maybeSockAddr numChannels maxClients inputBandwidth outputBandwidth = do
    hostPtr <- withMaybeToNull (toENetAddress <$> maybeSockAddr) $ \addrPtr ->
        B.hostCreate addrPtr numChannels maxClients
            inputBandwidth outputBandwidth
    return $ if hostPtr == nullPtr then Nothing else Just hostPtr

hostConnect :: Ptr B.Host -> SockAddr -> CSize -> Word32 -> IO (Maybe (Ptr B.Peer))
hostConnect host address channelCount datum = alloca $ \addr -> do
  poke addr $ toENetAddress address
  result <- B.hostConnect host addr channelCount datum
  return $ if result == nullPtr then Nothing else Just result

hostCheckEvents :: Ptr B.Host -> IO (Either String (Maybe B.Event))
hostCheckEvents host = alloca $ \ptr -> do
  status <- B.hostCheckEvents host ptr
  case compare status 0 of
    GT -> Right . Just <$> peek ptr
    EQ -> return $ Right Nothing
    LT -> return $ Left "error checking events"

hostService :: Ptr B.Host -> Word32 -> IO (Either String (Maybe B.Event))
hostService host timeout = alloca $ \ptr -> do
  status <- B.hostService host ptr timeout
  case compare status 0 of
    GT -> (Right . Just) <$> peek ptr
    EQ -> return $ Right Nothing
    LT -> return $ Left "error servicing events"


