import Control.Monad
import Network.Socket

import Network.ENet

main :: IO ()
main = withENetDo $ do
    let address = Just (SockAddrInet 1234 iNADDR_ANY)

    let maxClients = 32
        numChannels = 2
        bandwidthIn = 0
        bandwidthOut = 0
    Just hostPtr <- hostCreate address
        maxClients numChannels
        bandwidthIn bandwidthOut

    forever $ do
        --putStrLn $ "Awaiting events..."
        let maxWaitMillisec = 1000
        maybeEvent <- hostService hostPtr maxWaitMillisec
        case maybeEvent of
            Right (Just (Event eventType peerPtr channelID packetLength packetPtr)) -> do
                print (eventType, peerPtr, channelID, packetLength, packetPtr)

                case eventType of
                    Receive -> do

                        Packet _flags contents <- packetPeek packetPtr
                        print contents
                    _ -> return ()
            Left anError -> do
                putStrLn anError
            _ -> return ()
