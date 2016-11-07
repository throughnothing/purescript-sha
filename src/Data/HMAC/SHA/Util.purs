module Data.HMAC.SHA.Util where

import Prelude
import Data.ArrayBuffer.Types (Uint8Array)

import Data.Digest.SHA.Types (ShaAlgorithm)
import Data.HMAC.SHA.Types (ShaHmacData, ShaHmacKey)

foreign import _shaHmacBuffer :: String -> Uint8Array -> Uint8Array -> Uint8Array

foreign import _shaHmacHex :: String -> Uint8Array -> Uint8Array -> String

foreign import _shaHmacB64 :: String -> Uint8Array -> Uint8Array -> String


shaHmacB64Base :: ShaAlgorithm -> ShaHmacKey -> ShaHmacData -> String
shaHmacB64Base a k d = _shaHmacB64 (show a) k d

shaHmacBase :: ShaAlgorithm -> ShaHmacKey -> ShaHmacData -> Uint8Array
shaHmacBase a k d = _shaHmacBuffer (show a) k d

shaHmacHexBase :: ShaAlgorithm -> ShaHmacKey -> ShaHmacData -> String
shaHmacHexBase a k d = _shaHmacHex (show a) k d
