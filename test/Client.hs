{-# LANGUAGE OverloadedStrings #-}
import Control.Monad
import Control.Concurrent
import Foreign.Ptr
import Network.Socket (SockAddr(..), inet_addr)
import Network.ENet
import Network.ENet.Host
import Network.ENet.Peer
import Network.ENet.Bindings
import Network.ENet.Packet

main :: IO ()
main = withENetDo $ do
    let address = Nothing -- Nothing == Client

    let maxClients = 1
        numChannels = 2
        bandwidthIn = 0
        bandwidthOut = 0
    clientPtr <- create address
        maxClients numChannels
        bandwidthIn bandwidthOut

    when (clientPtr == nullPtr) $
        error "Couldn't create client :("

    let datum = 0
    localhost <- inet_addr "127.0.0.1"
    peerPtr <- connect clientPtr (SockAddrInet 1234 localhost)
        numChannels datum

    when (peerPtr == nullPtr) $
        error "No available peers :("

    maybeEvent <- service clientPtr 5000
    case maybeEvent of
        Just (Event Connect peerPtr channelID packetLength packetPtr) -> do
            putStrLn "Connected!"
            print (Connect, peerPtr, channelID, packetLength, packetPtr)
        other -> do
            reset peerPtr
            error "Failed to connect :("


    let packet = Packet
            (makePacketFlagSet [Reliable])
            "Hello from client"

    send peerPtr (ChannelID 0)
        =<< (poke packet)
    forever $ do
        threadDelay 10000
        putStrLn $ "Awaiting events..."
        let maxWaitMillisec = 1000
        maybeEvent <- service clientPtr maxWaitMillisec
        case maybeEvent of
            Just (Event eventType peerPtr channelID packetLength packetPtr) ->
                print (eventType, peerPtr, channelID, packetLength, packetPtr)
            Nothing -> putStrLn "No event"
