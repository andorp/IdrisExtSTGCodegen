module Idris.Runtime.PrimOp.Mod where

import GHC.Int
import Idris.Runtime.Integer as BI
import Idris.Runtime.PrimType

int :: Int -> Int -> Int
int = Prelude.mod

integer :: BI -> BI -> BI
integer = BI.mod

int8 :: Int8 -> Int8 -> Int8
int8 = Prelude.mod

int16 :: Int16 -> Int16 -> Int16
int16 = Prelude.mod

int32 :: Int32 -> Int32 -> Int32
int32 = Prelude.mod

int64 :: Int64 -> Int64 -> Int64
int64 = Prelude.mod

bits8 :: Bits8 -> Bits8 -> Bits8
bits8 = Prelude.mod

bits16 :: Bits16 -> Bits16 -> Bits16
bits16 = Prelude.mod

bits32 :: Bits32 -> Bits32 -> Bits32
bits32 = Prelude.mod

bits64 :: Bits64 -> Bits64 -> Bits64
bits64 = Prelude.mod
