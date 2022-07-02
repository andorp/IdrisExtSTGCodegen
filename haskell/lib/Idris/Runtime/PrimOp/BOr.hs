module Idris.Runtime.PrimOp.BOr where

import Data.Bits
import Idris.Runtime.PrimType
import Idris.Runtime.Integer

int :: Int -> Int -> Int
int = (.|.)

int8 :: Int8 -> Int8 -> Int8
int8 = (.|.)

int16 :: Int16 -> Int16 -> Int16
int16 = (.|.)

int32 :: Int32 -> Int32 -> Int32
int32 = (.|.)

int64 :: Int64 -> Int64 -> Int64
int64 = (.|.)

integer :: BI -> BI -> BI
integer (BI x) (BI y) = BI (x .|. y)

bits8 :: Bits8 -> Bits8 -> Bits8
bits8 = (.|.)

bits16 :: Bits16 -> Bits16 -> Bits16
bits16 = (.|.)

bits32 :: Bits32 -> Bits32 -> Bits32
bits32 = (.|.)

bits64 :: Bits64 -> Bits64 -> Bits64
bits64 = (.|.)
