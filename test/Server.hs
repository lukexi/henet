import Control.Monad
import Foreign.Ptr
import Network.Socket
import Network.ENet
import Network.ENet.Host
import Network.ENet.Bindings
import Network.ENet.Packet

main :: IO ()
main = withENetDo $ do
    let address = Just (SockAddrInet 1234 iNADDR_ANY)

    let maxClients = 32
        numChannels = 2
        bandwidthIn = 0
        bandwidthOut = 0
    hostPtr <- create address
        maxClients numChannels
        bandwidthIn bandwidthOut

    when (hostPtr == nullPtr) $
        error "Couldn't create server :("

    forever $ do
        putStrLn $ "Awaiting events..."
        let maxWaitMillisec = 1000
        maybeEvent <- service hostPtr maxWaitMillisec
        case maybeEvent of
            Just (Event eventType peerPtr channelID packetLength packetPtr) -> do
                print (eventType, peerPtr, channelID, packetLength, packetPtr)

                case eventType of
                    Receive -> do

                        Packet flags contents <- peek packetPtr
                        print contents
                    _ -> return ()
            Nothing -> putStrLn "No event"
