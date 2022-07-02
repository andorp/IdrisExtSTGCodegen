module Idris.Runtime.PrimOp.ShiftL where

import Data.Bits
import Idris.Runtime.PrimType

int :: Int -> Int -> Int
int x y = shiftL x y

bits8 :: Bits8 -> Bits8 -> Bits8
bits8 x y = shiftL x (fromIntegral y)

bits16 :: Bits16 -> Bits16 -> Bits16
bits16 x y = shiftL x (fromIntegral y)

bits32 :: Bits32 -> Bits32 -> Bits32
bits32 x y = shiftL x (fromIntegral y)

bits64 :: Bits64 -> Bits64 -> Bits64
bits64 x y = shiftL x (fromIntegral y)

