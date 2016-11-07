module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Data.ArrayBuffer.ArrayBuffer (fromString)
import Data.ArrayBuffer.DataView (whole)
import Data.ArrayBuffer.Typed (dataView)
import Data.TextEncoder (encodeUtf8)

import Test.Assert

import Data.Digest.SHA
import Data.HMAC.SHA

main :: forall e. Eff (assert :: ASSERT, console :: CONSOLE | e) Unit
main = do
  -- Check known SHA test vectors
  -- http://www.di-mgt.com.au/sha_testvectors.html

  -- String
  let hash256Abc = shaHex SHA_256 "abc"
  assert $ hash256Abc == "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"

  -- Uint8Array
  let hash512Abc = shaHex SHA_512 $ encodeUtf8 "abc"
  assert $ hash512Abc == "ddaf35a193617abacc417349ae20413112e6fa4e89a97ea20a9eeee64b55d39a2192992a274fc1a836ba3c23a3feebbd454d4423643ce80e2a9ac94fa54ca49f"

  -- DataView
  let hash256Empty = shaHex SHA_256 $ (whole <<< fromString) ""
  assert $ hash256Empty == "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"

  -- ArrayBuffer
  let hash512Empty = shaHex SHA_512 $ fromString ""
  assert $ hash512Empty == "cf83e1357eefb8bdf1542850d66d8007d620e4050b5715dc83f4a921d36ce9ce47d0d13c5d85f2b0ff8318d2877eec2f63b931bd47417a81a538327af927da3e"



  -- Check HMAC SHA test vector 2
  -- https://tools.ietf.org/html/rfc4231#section-4
  let key = "Jefe"
  let dat = "what do ya want for nothing?"

  -- String
  let shaHmac256Test2 = shaHmacHex SHA_256 key dat
  assert $ shaHmac256Test2 == "5bdcc146bf60754e6a042426089575c75a003f089d2739839dec58b964ec3843"

  -- Uint8Array
  let shaHmac512Test2 = shaHmacHex SHA_512 (encodeUtf8 key) (encodeUtf8 dat)
  assert $ shaHmac512Test2 == "164b7a7bfcf819e2e395fbe73b56e0a387bd64222e831fd610270cd7ea2505549758bf75c05a994a6d034f65f8f0e6fdcaeab1a34d4a6b4b636e070a38bce737"

  -- DataView
  let shaHmac224Test2 = shaHmacHex SHA_224 ((dataView <<< encodeUtf8) key) ((dataView <<< encodeUtf8) dat)
  assert $ shaHmac224Test2 == "a30e01098bc6dbbf45690f3a7e9e6d0f8bbea2a39e6148008fd05e44"
