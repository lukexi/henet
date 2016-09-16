{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

--import Foreign.Ptr
--import Control.Concurrent
import Control.Monad
import Network.Socket (SockAddr(..), inet_addr, PortNumber)

import Network.ENet

import Shared

main :: IO ()
main = startClient "127.0.0.1" 1234

clientHostConfig :: HostConfig
clientHostConfig = HostConfig
    { hcMaxClients = 1
    , hcNumChannels = 2
    , hcBandwidthIn = 0
    , hcBandwidthOut = 0
    }



startClient :: String -> PortNumber -> IO ()
startClient address port = withENetDo $ do
    let noPublicAddress = Nothing -- Nothing == Client (address is for public connection)
        HostConfig{..} = clientHostConfig

    host <- fromJustNote "Couldn't create host :(" <$> hostCreate noPublicAddress
        hcMaxClients hcNumChannels
        hcBandwidthIn hcBandwidthOut

    let datum = 0
    addressBytes <- inet_addr address
    serverPeer <- fromJustNote "Couldn't connect to host :(" <$> hostConnect host
        (SockAddrInet port addressBytes)
        hcNumChannels
        datum

    maybeConnectionEvent <- hostService host 5000
    case maybeConnectionEvent of
        Right (Just (Event Connect peer channelID packetLength packetPtr)) -> do
            putStrLn "Connected!"
            print (Connect, peer, channelID, packetLength, packetPtr)
        _other -> do
            peerReset serverPeer
            error "Failed to connect :("


    let packet = Packet
            (makePacketFlagSet [Reliable])
            "Hello from client"

    forever $ do
        peerSend serverPeer (ChannelID 0) =<< packetPoke packet
        let maxWaitMillisec = 1
        maybeEvent <- hostService host maxWaitMillisec
        case maybeEvent of
            Right (Just (Event eventType peer channelID packetLength packetPtr)) ->
                print (eventType, peer, channelID, packetLength, packetPtr)
            Left anError -> putStrLn anError
            _ -> return ()

