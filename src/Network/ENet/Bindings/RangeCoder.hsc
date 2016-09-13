{-# LANGUAGE CPP, EmptyDataDecls, GeneralizedNewtypeDeriving #-}

module Network.ENet.Bindings.RangeCoder where

import Foreign.C
import Foreign.Ptr
import Data.Word

import Network.ENet.Bindings.Socket

data RangeCoder

foreign import ccall unsafe "enet.h enet_range_coder_create"     rangeCoderCreate
  :: IO (Ptr  RangeCoder)
foreign import ccall unsafe "enet.h enet_range_coder_destroy"    rangeCoderDestroy
  :: Ptr RangeCoder -> IO ()
foreign import ccall unsafe "enet.h enet_range_coder_compress"   rangeCoderCompress
  :: Ptr RangeCoder -> Ptr Buffer -> CSize -> CSize -> Word8 -> CSize -> IO CSize
foreign import ccall unsafe "enet.h enet_range_coder_decompress" rangeCoderDecompress
  :: Ptr RangeCoder -> Ptr Buffer -> CSize -> Word8 -> CSize -> IO CSize
