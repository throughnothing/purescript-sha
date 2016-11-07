module Data.Digest.SHA.Types where

import Prelude

data ShaAlgorithm
  = SHA_1
  | SHA_224
  | SHA3_224
  | SHA_256
  | SHA3_256
  | SHA_384
  | SHA_512
  | SHA3_512
  | SHAKE128
  | SHAKE256

instance showShaAlgorithm :: Show ShaAlgorithm where
  show SHA_1    = "SHA-1"
  show SHA_224  = "SHA-224"
  show SHA3_224 = "SHA3-224"
  show SHA_256  = "SHA-256"
  show SHA3_256 = "SHA3-256"
  show SHA_384  = "SHA-384"
  show SHA_512  = "SHA-512"
  show SHA3_512 = "SHA3_512"
  show SHAKE128 = "SHAKE128"
  show SHAKE256 = "SHAKE256"
