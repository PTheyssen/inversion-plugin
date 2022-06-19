module Encryption where

import Prelude hiding ((++), map, takeWhile, dropWhile, length, head)

import Data.Bits
import Data.Word

--------------------------------------------------------------------------------
-- Implementing the tiny encryption algorithms by David Wheeler, Roger Needham
--------------------------------------------------------------------------------

-- 128 bit key
data TEAKey = TEAKey {-# UNPACK #-} !Word32 {-# UNPACK #-} !Word32 {-# UNPACK #-} !Word32 {-# UNPACK #-} !Word32
  deriving (Show)


secretKey :: TEAKey
secretKey = (TEAKey  0xdeadbeef 0xdeadbeef 0xdeadbeef 0xdeadbeef)

delta :: Word32
delta = 0x9e3779b9

myData :: Word64
myData = 0xbadf00000000000d

rounds :: Int
rounds = 32

-- Plugin cannot handle Data.Bits, Data.Word :(
teaEncrypt :: TEAKey -> Word64 -> Word64
teaEncrypt (TEAKey k0 k1 k2 k3) v = doCycle rounds 0 v0 v1 where
  v0, v1 :: Word32
  v0 = fromIntegral v
  v1 = fromIntegral $ v `shiftR` 32
  doCycle :: Int -> Word32 -> Word32 -> Word32 -> Word64
  doCycle 0 _ v0 v1 = (fromIntegral v1 `shiftL` 32)
                      .|. (fromIntegral v0 .&. 0xffffffff)
  doCycle n sum v0 v1 = doCycle (n - 1) sum' v0' v1'
    where
      sum' = sum + delta
      v0' = v0 + (((v1 `shiftL` 4) + k0) `xor` (v1 + sum') `xor` ((v1 `shiftR` 5) + k1))
      v1' = v1 + (((v0 `shiftL` 4) + k2) `xor` (v0 + sum') `xor` ((v0 `shiftR` 5) + k3))


