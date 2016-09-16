{-# LANGUAGE RecordWildCards #-}
import Control.Monad
import Network.Socket

import Network.ENet

import Shared
import Data.ByteString (ByteString)

main :: IO ()
main = startServer 1234 serverHostConfig $ \packet -> do
    print packet

serverHostConfig :: HostConfig
serverHostConfig = HostConfig
    { hcMaxClients = 32
    , hcNumChannels = 2
    , hcBandwidthIn = 0
    , hcBandwidthOut = 0
    }

startServer :: PortNumber -> HostConfig -> (ByteString -> IO ()) -> IO a
startServer port HostConfig{..} action = withENetDo $ do
    let address = Just (SockAddrInet port iNADDR_ANY)

    host <- fromJustNote "Couldn't create server." <$> hostCreate address
        hcMaxClients hcNumChannels
        hcBandwidthIn hcBandwidthOut

    forever $ do
        let maxWaitMillisec = 1
        maybeEvent <- hostService host maxWaitMillisec
        case maybeEvent of
            Right (Just _event@(Event eventType _peer channelID _packetLength rawPacket)) -> do
                --print event
                --hostBroadcast host channelID rawPacket
                case eventType of
                    Receive -> do
                        Packet _flags contents <- packetPeek rawPacket
                        action contents
                    _ -> return ()
            Left anError -> do
                putStrLn anError
            _ -> return ()
