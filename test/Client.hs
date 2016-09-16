{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

--import Foreign.Ptr
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Network.Socket (SockAddr(..), inet_addr, PortNumber)

import Network.ENet

import Data.Binary

import Shared

main :: IO ()
main = do
    (sendChan, receiveChan) <- startClient "127.0.0.1" 1234

    forever $ do
        message <- atomically $ tryReadTChan receiveChan
        putStrLn $ "RECEIVED: " ++ show message
        threadDelay 1000000
        atomically $ writeTChan sendChan $
            ( [Reliable]
            , EntityUpdate 1234 (1.2, 3.4)
            )


clientHostConfig :: HostConfig
clientHostConfig = HostConfig
    { hcMaxClients = 1
    , hcNumChannels = 2
    , hcBandwidthIn = 0
    , hcBandwidthOut = 0
    }

reliablePacket contents = encodePacket [Reliable] contents

encodePacket flags contents = Packet
    (makePacketFlagSet flags) (encodeStrict contents)

createHostWithConfig HostConfig{..} address = hostCreate address
    hcMaxClients hcNumChannels
    hcBandwidthIn hcBandwidthOut

connectToHost hostConfig host address port = do
    sockAddress <- SockAddrInet port <$> inet_addr address
    hostConnect host
        sockAddress
        (hcNumChannels hostConfig)
        datum
    where datum = 0

awaitConnection host peer maxTime = do

    maybeConnectionEvent <- hostService host maxTime
    case maybeConnectionEvent of
        Right (Just (Event Connect thePeer channelID packetLength packetPtr)) -> do
            putStrLn "Connected!"
            print (Connect, thePeer, channelID, packetLength, packetPtr)
        _other -> do
            peerReset peer
            error "Failed to connect :("

startClient :: Binary a
            => String
            -> PortNumber
            -> IO (TChan ([PacketFlag], a), TChan a)
startClient address port = do

    outgoingChan <- newTChanIO
    incomingChan <- newTChanIO

    forkOS $ withENetDo $ do

        -- Create the host
        let noPublicAddress = Nothing -- Nothing == Client (address is for public connection)

        host <- fromJustNote "Couldn't create host :(" <$>
            createHostWithConfig clientHostConfig noPublicAddress

        -- Connect to the server
        serverPeer <- fromJustNote "Couldn't connect to host :(" <$>
            connectToHost clientHostConfig host address port

        -- Await the connection event
        awaitConnection host serverPeer 5000

        forever $ do
            atomically (tryReadTChan outgoingChan) >>= \case
                Nothing -> return ()
                Just (flags, message) -> do
                    peerSend serverPeer (ChannelID 0) =<<
                        packetPoke (encodePacket flags message)
            let maxWaitMillisec = 1
            maybeEvent <- hostService host maxWaitMillisec
            case maybeEvent of
                Right (Just event) -> do
                    case evtType event of
                        Receive -> do
                            Packet _flags contents <- packetPeek (evtPacket event)
                            atomically $ writeTChan incomingChan $!
                                decodeStrict contents
                        _ -> return ()
                    print event
                Left anError -> putStrLn anError
                _ -> return ()
    return (outgoingChan, incomingChan)
