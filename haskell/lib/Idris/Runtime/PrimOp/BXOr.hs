module Idris.Runtime.PrimOp.BXOr where

import Data.Bits
import Idris.Runtime.PrimType
import Idris.Runtime.Integer

int :: Int -> Int -> Int
int = xor

int8 :: Int8 -> Int8 -> Int8
int8 = xor

int16 :: Int16 -> Int16 -> Int16
int16 = xor

int32 :: Int32 -> Int32 -> Int32
int32 = xor

int64 :: Int64 -> Int64 -> Int64
int64 = xor

integer :: BI -> BI -> BI
integer (BI x) (BI y) = BI (xor x y)

bits8 :: Bits8 -> Bits8 -> Bits8
bits8 = xor

bits16 :: Bits16 -> Bits16 -> Bits16
bits16 = xor

bits32 :: Bits32 -> Bits32 -> Bits32
bits32 = xor

bits64 :: Bits64 -> Bits64 -> Bits64
bits64 = xor
