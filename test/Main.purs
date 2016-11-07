module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Data.ArrayBuffer.DataView (buffer)
import Data.ArrayBuffer.Typed (dataView)
import Data.ArrayBuffer.Types (ArrayBuffer, DataView)
import Data.TextEncoder (encodeUtf8)

import Test.Assert

import Data.Digest.SHA
import Data.HMAC.SHA

strToBuf :: String -> ArrayBuffer
strToBuf = buffer <<< strToDv

strToDv :: String -> DataView
strToDv = dataView <<< encodeUtf8

main :: forall e. Eff (assert :: ASSERT, console :: CONSOLE | e) Unit
main = do
  -- Check known SHA test vectors
  -- http://www.di-mgt.com.au/sha_testvectors.html

  -- String
  assert $ do
    let hash256Abc = shaHex SHA_256 "abc"
    hash256Abc == "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"

  -- Uint8Array
  assert $ do
    let hash512Abc = shaHex SHA_512 $ encodeUtf8 "abc"
    hash512Abc == "ddaf35a193617abacc417349ae20413112e6fa4e89a97ea20a9eeee64b55d39a2192992a274fc1a836ba3c23a3feebbd454d4423643ce80e2a9ac94fa54ca49f"

  -- DataView
  assert $ do
    let hash256Empty = shaHex SHA_256 $ strToDv "abc"
    hash256Empty == "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"

  -- ArrayBuffer
  assert $ do
    let hash512Empty = shaHex SHA_512 $ strToBuf "abc"
    hash512Empty == "ddaf35a193617abacc417349ae20413112e6fa4e89a97ea20a9eeee64b55d39a2192992a274fc1a836ba3c23a3feebbd454d4423643ce80e2a9ac94fa54ca49f"


  -- Check HMAC SHA test vector 2
  -- https://tools.ietf.org/html/rfc4231#section-4
  let key = "Jefe"
  let dat = "what do ya want for nothing?"

  -- String
  assert $ do
    let hmac256 = shaHmacHex SHA_256 key dat
    hmac256 == "5bdcc146bf60754e6a042426089575c75a003f089d2739839dec58b964ec3843"

  -- Uint8Array
  assert $ do
    let hmac512 = shaHmacHex SHA_512 (encodeUtf8 key) (encodeUtf8 dat)
    hmac512 == "164b7a7bfcf819e2e395fbe73b56e0a387bd64222e831fd610270cd7ea2505549758bf75c05a994a6d034f65f8f0e6fdcaeab1a34d4a6b4b636e070a38bce737"

  -- DataView
  assert $ do
    let hmac224 = shaHmacHex SHA_224 (strToDv key) (strToDv dat)
    hmac224 == "a30e01098bc6dbbf45690f3a7e9e6d0f8bbea2a39e6148008fd05e44"

  -- ArrayBuffer
  let hmacHex = shaHmacHex SHA_224 (strToBuf key) (strToBuf dat)
  assert $ do
    hmacHex == "a30e01098bc6dbbf45690f3a7e9e6d0f8bbea2a39e6148008fd05e44"
