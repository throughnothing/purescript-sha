module Data.Digest.SHA.Util
  ( shaB64Base
  , shaBase
  , shaHexBase
  ) where

import Prelude (show)
import Data.ArrayBuffer.Types (Uint8Array)

import Data.Digest.SHA.Types (ShaAlgorithm)

foreign import _shaBuffer :: String -> Uint8Array -> Uint8Array

foreign import _shaHex :: String -> Uint8Array -> String

foreign import _shaBase64 :: String -> Uint8Array -> String


shaB64Base :: ShaAlgorithm -> Uint8Array -> String
shaB64Base a = _shaBase64 (show a)

shaHexBase :: ShaAlgorithm -> Uint8Array -> String
shaHexBase a = _shaHex (show a)

shaBase :: ShaAlgorithm -> Uint8Array -> Uint8Array
shaBase a = _shaBuffer (show a)
