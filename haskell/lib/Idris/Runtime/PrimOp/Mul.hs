module Idris.Runtime.PrimOp.Mul where

import GHC.Int
import Idris.Runtime.Integer as BI
import Idris.Runtime.PrimType

int :: Int -> Int -> Int
int = (*)

integer :: BI -> BI -> BI
integer = BI.mul

int8 :: Int8 -> Int8 -> Int8
int8 = (*)

int16 :: Int16 -> Int16 -> Int16
int16 = (*)

int32 :: Int32 -> Int32 -> Int32
int32 = (*)

int64 :: Int64 -> Int64 -> Int64
int64 = (*)

bits8 :: Bits8 -> Bits8 -> Bits8
bits8 = (*)

bits16 :: Bits16 -> Bits16 -> Bits16
bits16 = (*)

bits32 :: Bits32 -> Bits32 -> Bits32
bits32 = (*)

bits64 :: Bits64 -> Bits64 -> Bits64
bits64 = (*)

double :: Double -> Double -> Double
double = (*)
