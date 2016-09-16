{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Shared where
import Foreign.C
import Data.Word
import Data.Binary
import qualified Data.ByteString.Lazy as L
import Data.ByteString (ByteString)
import GHC.Generics
type EntityID = Int

data Message = EntityUpdate !EntityID !(Float, Float)
    deriving (Generic, Binary, Show)

data HostConfig = HostConfig
    { hcMaxClients :: CSize
    , hcNumChannels :: CSize
    , hcBandwidthIn :: Word32
    , hcBandwidthOut :: Word32
    }

fromJustNote :: String -> Maybe t -> t
fromJustNote note m = case m of
    Just a -> a
    Nothing -> error note

decodeStrict :: Binary a => ByteString -> a
decodeStrict = decode . L.fromStrict
encodeStrict :: Binary a => a -> ByteString
encodeStrict = L.toStrict . encode
