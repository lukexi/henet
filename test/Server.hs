{-# LANGUAGE RecordWildCards #-}
import Control.Monad
import Network.Socket

import Network.ENet

import Shared
import Data.Binary

main :: IO ()
main = startServer 1234 serverHostConfig $ \message -> do
    print (message :: Message)
    return ()

serverHostConfig :: HostConfig
serverHostConfig = HostConfig
    { hcMaxClients = 32
    , hcNumChannels = 2
    , hcBandwidthIn = 0
    , hcBandwidthOut = 0
    }

startServer :: Binary a => PortNumber -> HostConfig -> (a -> IO ()) -> IO ()
startServer port HostConfig{..} action = do

    withENetDo $ do
        let address = Just (SockAddrInet port iNADDR_ANY)

        host <- fromJustNote "Couldn't create server." <$> hostCreate address
            hcMaxClients hcNumChannels
            hcBandwidthIn hcBandwidthOut

        void . forever $ do
            let maxWaitMillisec = 1
            maybeEvent <- hostService host maxWaitMillisec
            case maybeEvent of
                Right (Just event) -> do
                    case evtType event of
                        Receive -> do
                            Packet _flags contents <- packetPeek (evtPacket event)
                            action $! decodeStrict contents
                        _ -> return ()
                Left anError -> do
                    putStrLn anError
                _ -> return ()
