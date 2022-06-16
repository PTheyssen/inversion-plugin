module Encryption where

import Prelude hiding ((++), map, takeWhile, dropWhile, length, head)

import Data.Bits
import Data.Word

--------------------------------------------------------------------------------
-- Implementing the tiny encryption algorithms by David Wheeler, Roger Needham
--------------------------------------------------------------------------------

-- understanding unpack pragma
-- https://stackoverflow.com/questions/33931991/what-does-the-unpack-pragma-do-in-this-case

-- 128 bit key
data TEAKey = TEAKey {-# UNPACK #-} !Word32 {-# UNPACK #-} !Word32 {-# UNPACK #-} !Word32 {-# UNPACK #-} !Word32
  deriving (Show)

secretKey :: TEAKey
secretKey = (TEAKey  0xdeadbeef 0xdeadbeef 0xdeadbeef 0xdeadbeef)

-- teaEncipher :: TEAKey -> Word64 -> Word64
-- teaEncipher (TEAKey k0 k1 k2 k3)  = undefined
