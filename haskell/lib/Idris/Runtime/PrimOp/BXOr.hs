module Idris.Runtime.PrimOp.BXOr where

import Data.Bits
import Idris.Runtime.PrimType

bits8 :: Bits8 -> Bits8 -> Bits8
bits8 = xor

bits16 :: Bits16 -> Bits16 -> Bits16
bits16 = xor

bits32 :: Bits32 -> Bits32 -> Bits32
bits32 = xor

bits64 :: Bits64 -> Bits64 -> Bits64
bits64 = xor
