module Shared where
import Foreign.C
import Data.Word
type EntityID = Int

data Message = EntityUpdate EntityID (Float, Float)

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
