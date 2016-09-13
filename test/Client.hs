{-# LANGUAGE OverloadedStrings #-}
import Control.Monad
import Control.Concurrent
import Foreign.Ptr
import Network.Socket (SockAddr(..), inet_addr)

import Network.ENet

main :: IO ()
main = withENetDo $ do
    let address = Nothing -- Nothing == Client

    let maxClients = 1
        numChannels = 2
        bandwidthIn = 0
        bandwidthOut = 0
    Just clientPtr <- hostCreate address
        maxClients numChannels
        bandwidthIn bandwidthOut

    when (clientPtr == nullPtr) $
        error "Couldn't create client :("

    let datum = 0
    localhost <- inet_addr "127.0.0.1"
    Just peerPtr <- hostConnect clientPtr (SockAddrInet 1234 localhost)
        numChannels datum

    maybeEvent <- hostService clientPtr 5000
    case maybeEvent of
        Right (Just (Event Connect peerPtr channelID packetLength packetPtr)) -> do
            putStrLn "Connected!"
            print (Connect, peerPtr, channelID, packetLength, packetPtr)
        other -> do
            peerReset peerPtr
            error "Failed to connect :("


    let packet = Packet
            (makePacketFlagSet [Reliable])
            "Hello from client"

    forever $ do
        withPacket packet $
            peerSend peerPtr (ChannelID 0)
        --threadDelay 10000
        --putStrLn $ "Awaiting events..."
        let maxWaitMillisec = 1000
        maybeEvent <- hostService clientPtr maxWaitMillisec
        case maybeEvent of
            Right (Just (Event eventType peerPtr channelID packetLength packetPtr)) ->
                print (eventType, peerPtr, channelID, packetLength, packetPtr)
            Left anError -> putStrLn anError
            _ -> return ()

withPacket packet action = do
    allocated <- packetPoke packet
    r <- action allocated
    --Packet.destroy allocated
    return r


