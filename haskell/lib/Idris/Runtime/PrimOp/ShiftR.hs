module Idris.Runtime.PrimOp.ShiftR where

import Data.Bits
import Idris.Runtime.PrimType
import Idris.Runtime.Integer

int :: Int -> Int -> Int
int x y = shiftR x y

int8 :: Int8 -> Int8 -> Int8
int8 x y = shiftR x (fromIntegral y)

int16 :: Int16 -> Int16 -> Int16
int16 x y = shiftR x (fromIntegral y)

int32 :: Int32 -> Int32 -> Int32
int32 x y = shiftR x (fromIntegral y)

int64 :: Int64 -> Int64 -> Int64
int64 x y = shiftR x (fromIntegral y)

integer :: BI -> BI -> BI
integer (BI x) (BI y) = BI (shiftR x (fromIntegral y))

bits8 :: Bits8 -> Bits8 -> Bits8
bits8 x y = shiftR x (fromIntegral y)

bits16 :: Bits16 -> Bits16 -> Bits16
bits16 x y = shiftR x (fromIntegral y)

bits32 :: Bits32 -> Bits32 -> Bits32
bits32 x y = shiftR x (fromIntegral y)

bits64 :: Bits64 -> Bits64 -> Bits64
bits64 x y = shiftR x (fromIntegral y)
