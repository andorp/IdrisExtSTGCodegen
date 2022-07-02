module Idris.Runtime.PrimOp.Div where

import GHC.Int
import Idris.Runtime.Integer as BI
import Idris.Runtime.PrimType

int :: Int -> Int -> Int
int = Prelude.div

integer :: BI -> BI -> BI
integer = BI.add

int8 :: Int8 -> Int8 -> Int8
int8 = Prelude.div

int16 :: Int16 -> Int16 -> Int16
int16 = Prelude.div

int32 :: Int32 -> Int32 -> Int32
int32 = Prelude.div

int64 :: Int64 -> Int64 -> Int64
int64 = Prelude.div

bits8 :: Bits8 -> Bits8 -> Bits8
bits8 = Prelude.div

bits16 :: Bits16 -> Bits16 -> Bits16
bits16 = Prelude.div

bits32 :: Bits32 -> Bits32 -> Bits32
bits32 = Prelude.div

bits64 :: Bits64 -> Bits64 -> Bits64
bits64 = Prelude.div

double :: Double -> Double -> Double
double = (/)
