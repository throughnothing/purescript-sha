module Data.HMAC.SHA.Class
  ( class ShaHmac
  , shaHmac
  , shaHmacHex
  , shaHmacB64
  ) where

import Prelude ((<<<))
import Data.ArrayBuffer.DataView (whole)
import Data.ArrayBuffer.Typed (asUint8Array)
import Data.ArrayBuffer.Types (ArrayBuffer, ArrayView, DataView, Uint8, Uint8Array)
import Data.HMAC.SHA.Util (shaHmacBase, shaHmacHexBase, shaHmacB64Base)
import Data.TextEncoder (encodeUtf8)

import Data.Digest.SHA.Types

class ShaHmac a where
  shaHmac    :: ShaAlgorithm -> a -> a -> Uint8Array
  shaHmacHex :: ShaAlgorithm -> a -> a -> String
  shaHmacB64 :: ShaAlgorithm -> a -> a -> String

instance uint8ArrayShaHmac :: ShaHmac (ArrayView Uint8) where
  shaHmac    = shaHmacBase
  shaHmacHex = shaHmacHexBase
  shaHmacB64 = shaHmacB64Base

instance arrayBufferShaHmac :: ShaHmac ArrayBuffer where
  shaHmac    a k d = shaHmacBase    a (_fromAB k) (_fromAB d)
  shaHmacHex a k d = shaHmacHexBase a (_fromAB k) (_fromAB d)
  shaHmacB64 a k d = shaHmacB64Base a (_fromAB k) (_fromAB d)

instance dataViewShaHmac :: ShaHmac DataView where
  shaHmac    a k d = shaHmacBase    a (asUint8Array k) (asUint8Array d)
  shaHmacHex a k d = shaHmacHexBase a (asUint8Array k) (asUint8Array d)
  shaHmacB64 a k d = shaHmacB64Base a (asUint8Array k) (asUint8Array d)

instance stringShaHmac :: ShaHmac String where
  shaHmac    a k d = shaHmacBase    a (encodeUtf8 k) (encodeUtf8 d)
  shaHmacHex a k d = shaHmacHexBase a (encodeUtf8 k) (encodeUtf8 d)
  shaHmacB64 a k d = shaHmacB64Base a (encodeUtf8 k) (encodeUtf8 d)


_fromAB :: ArrayBuffer -> Uint8Array
_fromAB = asUint8Array <<< whole
