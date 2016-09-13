module Network.ENet.Bindings.Address where
import Network.Socket(HostAddress)

import Foreign.Storable
import Foreign.C
import Foreign.Ptr
import Data.Word


import Data.Endian
import Data.Endian.Unsafe( unsafeUnwrapBigEndian
                         , unsafeAssertBigEndian)

import Network.Socket.Internal

#include "enet/enet.h"

toENetAddress :: SockAddr -> Address
toENetAddress (SockAddrInet port ip) =
  Address ip $ fromBigEndian $ unsafeAssertBigEndian $ fromIntegral port
toENetAddress _ = error "Unsupported enet address!"

toSockAddr :: Address -> SockAddr
toSockAddr (Address ip port) =
  SockAddrInet (fromIntegral $ unsafeUnwrapBigEndian $ toBigEndian $ port) ip


data Address = Address HostAddress Word16

instance Storable Address where
  sizeOf    _ = (#size ENetAddress)
  alignment _ = (#size void*) -- FIX THIS!
  peek ptr = do h <- (#peek ENetAddress, host) ptr
                p <- (#peek ENetAddress, port) ptr
                return $ Address h p
  poke ptr (Address h p) = do (#poke ENetAddress, host) ptr h
                              (#poke ENetAddress, port) ptr p



foreign import ccall unsafe "enet.h enet_address_set_host"    addressSetHost
  :: Ptr Address -> IO CString
foreign import ccall unsafe "enet.h enet_address_get_host_ip" addressGetHostIP
  :: Ptr Address -> CString -> IO CSize
foreign import ccall unsafe "enet.h enet_address_get_host"    addressGetHost
  :: Ptr Address -> CString -> IO CSize
