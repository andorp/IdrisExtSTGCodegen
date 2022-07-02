module Idris.Runtime.PrimOp.BOr where

import Data.Bits
import Idris.Runtime.PrimType

bits8 :: Bits8 -> Bits8 -> Bits8
bits8 = (.|.)

bits16 :: Bits16 -> Bits16 -> Bits16
bits16 = (.|.)

bits32 :: Bits32 -> Bits32 -> Bits32
bits32 = (.|.)

bits64 :: Bits64 -> Bits64 -> Bits64
bits64 = (.|.)
