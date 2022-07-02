module Idris.Runtime.PrimOp.Neg where

import GHC.Int
import Idris.Runtime.Integer as BI

int :: Int -> Int
int = Prelude.negate

integer :: BI -> BI
integer = BI.neg

int8 :: Int8 -> Int8
int8 = Prelude.negate

int16 :: Int16 -> Int16
int16 = Prelude.negate

int32 :: Int32 -> Int32
int32 = Prelude.negate

int64 :: Int64 -> Int64
int64 = Prelude.negate

double :: Double -> Double
double = Prelude.negate
