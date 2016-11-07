module Data.Digest.SHA.Class
  ( class Sha
  , sha
  , shaHex
  , shaB64
  ) where

import Prelude ((<<<))
import Data.ArrayBuffer.DataView (whole)
import Data.ArrayBuffer.Typed (asUint8Array)
import Data.ArrayBuffer.Types (ArrayBuffer, DataView, ArrayView, Uint8, Uint8Array)
import Data.TextEncoder (encodeUtf8)

import Data.Digest.SHA.Util
import Data.Digest.SHA.Types (ShaAlgorithm)

class Sha a where
  sha :: ShaAlgorithm -> a -> Uint8Array
  shaHex :: ShaAlgorithm -> a -> String
  shaB64 :: ShaAlgorithm -> a -> String

instance uint8ArraySha :: Sha (ArrayView Uint8) where
  sha    = shaBase
  shaHex = shaHexBase
  shaB64 = shaB64Base

instance arrayBufferSha :: Sha ArrayBuffer where
  sha    a b = shaBase    a (_fromAB b)
  shaHex a b = shaHexBase a (_fromAB b)
  shaB64 a b = shaB64Base a (_fromAB b)


instance dataViewSha :: Sha DataView where
  sha    a x = shaBase    a (asUint8Array x)
  shaHex a x = shaHexBase a (asUint8Array x)
  shaB64 a x = shaB64Base a (asUint8Array x)

instance stringArraySha :: Sha String where
  sha    a x = shaBase    a (encodeUtf8 x)
  shaHex a x = shaHexBase a (encodeUtf8 x)
  shaB64 a x = shaB64Base a (encodeUtf8 x)

_fromAB :: ArrayBuffer -> Uint8Array
_fromAB = asUint8Array <<< whole
