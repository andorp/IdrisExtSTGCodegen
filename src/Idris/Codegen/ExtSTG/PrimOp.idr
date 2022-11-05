module Idris.Codegen.ExtSTG.PrimOp

import Data.Vect
import Core.Core
import Core.Context
import Compiler.ANF
import Data.List
import Idris.Codegen.ExtSTG.STG
import Idris.Codegen.ExtSTG.Core
import Idris.Codegen.ExtSTG.ExtName
import Idris.Codegen.ExtSTG.Context
import Idris.Codegen.ExtSTG.ExternalTopIds
import Idris.Codegen.ExtSTG.Configuration


data PrimFnExt
  = NonImplemented
  | PureExt ExtName
  | IOExt ExtName

total
primFnExtAdd : PrimType -> PrimFnExt
primFnExtAdd IntType      = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "Add"] "int"
primFnExtAdd Int8Type     = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "Add"] "int8"
primFnExtAdd Int16Type    = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "Add"] "int16"
primFnExtAdd Int32Type    = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "Add"] "int32"
primFnExtAdd Int64Type    = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "Add"] "int64"
primFnExtAdd IntegerType  = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "Add"] "integer"
primFnExtAdd Bits8Type    = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "Add"] "bits8"
primFnExtAdd Bits16Type   = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "Add"] "bits16"
primFnExtAdd Bits32Type   = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "Add"] "bits32"
primFnExtAdd Bits64Type   = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "Add"] "bits64"
primFnExtAdd DoubleType   = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "Add"] "double"
primFnExtAdd _ = NonImplemented

total
primFnExtSub : PrimType -> PrimFnExt
primFnExtSub IntType      = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "Sub"] "int"
primFnExtSub Int8Type     = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "Sub"] "int8"
primFnExtSub Int16Type    = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "Sub"] "int16"
primFnExtSub Int32Type    = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "Sub"] "int32"
primFnExtSub Int64Type    = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "Sub"] "int64"
primFnExtSub IntegerType  = PureExt $ MkExtName "main" ["Idris", "Runtime", "Integer"] "sub"
primFnExtSub Bits8Type    = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "Sub"] "bits8"
primFnExtSub Bits16Type   = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "Sub"] "bits16"
primFnExtSub Bits32Type   = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "Sub"] "bits32"
primFnExtSub Bits64Type   = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "Sub"] "bits64"
primFnExtSub DoubleType   = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "Sub"] "double"
primFnExtSub _ = NonImplemented

total
primFnExtMul : PrimType -> PrimFnExt
primFnExtMul IntType      = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "Mul"] "int"
primFnExtMul Int8Type     = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "Mul"] "int8"
primFnExtMul Int16Type    = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "Mul"] "int16"
primFnExtMul Int32Type    = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "Mul"] "int32"
primFnExtMul Int64Type    = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "Mul"] "int64"
primFnExtMul IntegerType  = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "Mul"] "integer"
primFnExtMul Bits8Type    = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "Mul"] "bits8"
primFnExtMul Bits16Type   = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "Mul"] "bits16"
primFnExtMul Bits32Type   = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "Mul"] "bits32"
primFnExtMul Bits64Type   = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "Mul"] "bits64"
primFnExtMul DoubleType   = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "Mul"] "double"
primFnExtMul _ = NonImplemented

total
primFnExtDiv : PrimType -> PrimFnExt
primFnExtDiv IntType      = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "Div"] "int"
primFnExtDiv Int8Type     = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "Div"] "int8"
primFnExtDiv Int16Type    = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "Div"] "int16"
primFnExtDiv Int32Type    = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "Div"] "int32"
primFnExtDiv Int64Type    = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "Div"] "int64"
primFnExtDiv IntegerType  = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "Div"] "integer"
primFnExtDiv Bits8Type    = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "Div"] "bits8"
primFnExtDiv Bits16Type   = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "Div"] "bits16"
primFnExtDiv Bits32Type   = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "Div"] "bits32"
primFnExtDiv Bits64Type   = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "Div"] "bits64"
primFnExtDiv DoubleType   = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "Div"] "double"
primFnExtDiv _ = NonImplemented

total
primFnExtMod : PrimType -> PrimFnExt
primFnExtMod IntType      = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "Mod"] "int"
primFnExtMod Int8Type     = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "Mod"] "int8"
primFnExtMod Int16Type    = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "Mod"] "int16"
primFnExtMod Int32Type    = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "Mod"] "int32"
primFnExtMod Int64Type    = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "Mod"] "int64"
primFnExtMod IntegerType  = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "Mod"] "integer"
primFnExtMod Bits8Type    = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "Mod"] "bits8"
primFnExtMod Bits16Type   = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "Mod"] "bits16"
primFnExtMod Bits32Type   = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "Mod"] "bits32"
primFnExtMod Bits64Type   = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "Mod"] "bits64"
primFnExtMod DoubleType   = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "Mod"] "double"
primFnExtMod _ = NonImplemented

total
primFnExtNeg : PrimType -> PrimFnExt
primFnExtNeg IntType      = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "Neg"] "int"
primFnExtNeg Int8Type     = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "Neg"] "int8"
primFnExtNeg Int16Type    = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "Neg"] "int16"
primFnExtNeg Int32Type    = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "Neg"] "int32"
primFnExtNeg Int64Type    = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "Neg"] "int64"
primFnExtNeg IntegerType  = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "Neg"] "integer"
primFnExtNeg DoubleType   = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "Neg"] "double"
primFnExtNeg _ = NonImplemented

total
primFnExtShiftL : PrimType -> PrimFnExt
primFnExtShiftL IntType     = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "ShiftL"] "int"
primFnExtShiftL Int8Type    = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "ShiftL"] "int8"
primFnExtShiftL Int16Type   = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "ShiftL"] "int16"
primFnExtShiftL Int32Type   = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "ShiftL"] "int32"
primFnExtShiftL Int64Type   = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "ShiftL"] "int64"
primFnExtShiftL IntegerType = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "ShiftL"] "integer"
primFnExtShiftL Bits8Type   = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "ShiftL"] "bits8"
primFnExtShiftL Bits16Type  = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "ShiftL"] "bits16"
primFnExtShiftL Bits32Type  = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "ShiftL"] "bits32"
primFnExtShiftL Bits64Type  = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "ShiftL"] "bits64"
primFnExtShiftL _ = NonImplemented

total
primFnExtShiftR : PrimType -> PrimFnExt
primFnExtShiftR IntType     = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "ShiftR"] "int"
primFnExtShiftR Int8Type    = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "ShiftR"] "int8"
primFnExtShiftR Int16Type   = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "ShiftR"] "int16"
primFnExtShiftR Int32Type   = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "ShiftR"] "int32"
primFnExtShiftR Int64Type   = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "ShiftR"] "int64"
primFnExtShiftR IntegerType = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "ShiftR"] "integer"
primFnExtShiftR Bits8Type   = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "ShiftR"] "bits8"
primFnExtShiftR Bits16Type  = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "ShiftR"] "bits16"
primFnExtShiftR Bits32Type  = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "ShiftR"] "bits32"
primFnExtShiftR Bits64Type  = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "ShiftR"] "bits64"
primFnExtShiftR _ = NonImplemented

total
primFnExtBAnd : PrimType -> PrimFnExt
primFnExtBAnd IntType     = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "BAnd"] "int"
primFnExtBAnd Int8Type    = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "BAnd"] "int8"
primFnExtBAnd Int16Type   = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "BAnd"] "int16"
primFnExtBAnd Int32Type   = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "BAnd"] "int32"
primFnExtBAnd Int64Type   = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "BAnd"] "int64"
primFnExtBAnd IntegerType = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "BAnd"] "integer"
primFnExtBAnd Bits8Type   = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "BAnd"] "bits8"
primFnExtBAnd Bits16Type  = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "BAnd"] "bits16"
primFnExtBAnd Bits32Type  = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "BAnd"] "bits32"
primFnExtBAnd Bits64Type  = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "BAnd"] "bits64"
primFnExtBAnd _ = NonImplemented

total
primFnExtBOr : PrimType -> PrimFnExt
primFnExtBOr IntType      = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "BOr"] "int"
primFnExtBOr Int8Type     = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "BOr"] "int8"
primFnExtBOr Int16Type    = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "BOr"] "int16"
primFnExtBOr Int32Type    = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "BOr"] "int32"
primFnExtBOr Int64Type    = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "BOr"] "int64"
primFnExtBOr IntegerType  = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "BOr"] "integer"
primFnExtBOr Bits8Type    = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "BOr"] "bits8"
primFnExtBOr Bits16Type   = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "BOr"] "bits16"
primFnExtBOr Bits32Type   = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "BOr"] "bits32"
primFnExtBOr Bits64Type   = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "BOr"] "bits64"
primFnExtBOr _ = NonImplemented

total
primFnExtBXOr : PrimType -> PrimFnExt
primFnExtBXOr IntType     = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "BXOr"] "int"
primFnExtBXOr Int8Type    = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "BXOr"] "int8"
primFnExtBXOr Int16Type   = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "BXOr"] "int16"
primFnExtBXOr Int32Type   = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "BXOr"] "int32"
primFnExtBXOr Int64Type   = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "BXOr"] "int64"
primFnExtBXOr IntegerType = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "BXOr"] "integer"
primFnExtBXOr Bits8Type   = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "BXOr"] "bits8"
primFnExtBXOr Bits16Type  = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "BXOr"] "bits16"
primFnExtBXOr Bits32Type  = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "BXOr"] "bits32"
primFnExtBXOr Bits64Type  = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "BXOr"] "bits64"
primFnExtBXOr _ = NonImplemented

total
primFnExtLT : PrimType -> PrimFnExt
primFnExtLT IntType     = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "LT"] "int"
primFnExtLT Int8Type    = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "LT"] "int8"
primFnExtLT Int16Type   = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "LT"] "int16"
primFnExtLT Int32Type   = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "LT"] "int32"
primFnExtLT Int64Type   = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "LT"] "int64"
primFnExtLT IntegerType = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "LT"] "integer"
primFnExtLT Bits8Type   = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "LT"] "bits8"
primFnExtLT Bits16Type  = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "LT"] "bits16"
primFnExtLT Bits32Type  = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "LT"] "bits32"
primFnExtLT Bits64Type  = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "LT"] "bits64"
primFnExtLT StringType  = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "LT"] "string"
primFnExtLT CharType    = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "LT"] "char"
primFnExtLT DoubleType  = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "LT"] "double"
primFnExtLT _ = NonImplemented

total
primFnExtLTE : PrimType -> PrimFnExt
primFnExtLTE IntType      = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "LTE"] "int"
primFnExtLTE Int8Type     = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "LTE"] "int8"
primFnExtLTE Int16Type    = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "LTE"] "int16"
primFnExtLTE Int32Type    = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "LTE"] "int32"
primFnExtLTE Int64Type    = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "LTE"] "int64"
primFnExtLTE IntegerType  = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "LTE"] "integer"
primFnExtLTE Bits8Type    = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "LTE"] "bits8"
primFnExtLTE Bits16Type   = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "LTE"] "bits16"
primFnExtLTE Bits32Type   = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "LTE"] "bits32"
primFnExtLTE Bits64Type   = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "LTE"] "bits64"
primFnExtLTE StringType   = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "LTE"] "string"
primFnExtLTE CharType     = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "LTE"] "char"
primFnExtLTE DoubleType   = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "LTE"] "double"
primFnExtLTE _ = NonImplemented

total
primFnExtEQ : PrimType -> PrimFnExt
primFnExtEQ IntType     = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "EQ"] "int"
primFnExtEQ Int8Type    = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "EQ"] "int8"
primFnExtEQ Int16Type   = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "EQ"] "int16"
primFnExtEQ Int32Type   = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "EQ"] "int32"
primFnExtEQ Int64Type   = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "EQ"] "int64"
primFnExtEQ IntegerType = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "EQ"] "integer"
primFnExtEQ Bits8Type   = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "EQ"] "bits8"
primFnExtEQ Bits16Type  = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "EQ"] "bits16"
primFnExtEQ Bits32Type  = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "EQ"] "bits32"
primFnExtEQ Bits64Type  = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "EQ"] "bits64"
primFnExtEQ StringType  = IOExt   $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "EQ"] "string"
primFnExtEQ CharType    = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "EQ"] "char"
primFnExtEQ DoubleType  = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "EQ"] "double"
primFnExtEQ _ = NonImplemented

total
primFnExtGTE : PrimType -> PrimFnExt
primFnExtGTE IntType      = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "GTE"] "int"
primFnExtGTE Int8Type     = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "GTE"] "int8"
primFnExtGTE Int16Type    = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "GTE"] "int16"
primFnExtGTE Int32Type    = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "GTE"] "int32"
primFnExtGTE Int64Type    = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "GTE"] "int64"
primFnExtGTE IntegerType  = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "GTE"] "integer"
primFnExtGTE Bits8Type    = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "GTE"] "bits8"
primFnExtGTE Bits16Type   = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "GTE"] "bits16"
primFnExtGTE Bits32Type   = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "GTE"] "bits32"
primFnExtGTE Bits64Type   = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "GTE"] "bits64"
primFnExtGTE StringType   = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "GTE"] "string"
primFnExtGTE CharType     = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "GTE"] "char"
primFnExtGTE DoubleType   = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "GTE"] "double"
primFnExtGTE _ = NonImplemented

total
primFnExtGT : PrimType -> PrimFnExt
primFnExtGT IntType     = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "GT"] "int"
primFnExtGT Int8Type    = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "GT"] "int8"
primFnExtGT Int16Type   = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "GT"] "int16"
primFnExtGT Int32Type   = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "GT"] "int32"
primFnExtGT Int64Type   = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "GT"] "int64"
primFnExtGT IntegerType = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "GT"] "integer"
primFnExtGT Bits8Type   = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "GT"] "bits8"
primFnExtGT Bits16Type  = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "GT"] "bits16"
primFnExtGT Bits32Type  = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "GT"] "bits32"
primFnExtGT Bits64Type  = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "GT"] "bits64"
primFnExtGT StringType  = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "GT"] "string"
primFnExtGT CharType    = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "GT"] "char"
primFnExtGT DoubleType  = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "GT"] "double"
primFnExtGT _ = NonImplemented

primFnExtCastModule : PrimType -> Maybe String
primFnExtCastModule IntType     = Just "Int"
primFnExtCastModule Int8Type    = Just "Int8"
primFnExtCastModule Int16Type   = Just "Int16"
primFnExtCastModule Int32Type   = Just "Int32"
primFnExtCastModule Int64Type   = Just "Int64"
primFnExtCastModule IntegerType = Just "Integer"
primFnExtCastModule Bits8Type   = Just "Bits8"
primFnExtCastModule Bits16Type  = Just "Bits16"
primFnExtCastModule Bits32Type  = Just "Bits32"
primFnExtCastModule Bits64Type  = Just "Bits64"
primFnExtCastModule StringType  = Just "String"
primFnExtCastModule CharType    = Just "Char"
primFnExtCastModule DoubleType  = Just "Double"
primFnExtCastModule WorldType   = Nothing

primFnExtCastFun : PrimType -> Maybe String
primFnExtCastFun IntType     = Just "int"
primFnExtCastFun Int8Type    = Just "int8"
primFnExtCastFun Int16Type   = Just "int16"
primFnExtCastFun Int32Type   = Just "int32"
primFnExtCastFun Int64Type   = Just "int64"
primFnExtCastFun IntegerType = Just "integer"
primFnExtCastFun Bits8Type   = Just "bits8"
primFnExtCastFun Bits16Type  = Just "bits16"
primFnExtCastFun Bits32Type  = Just "bits32"
primFnExtCastFun Bits64Type  = Just "bits64"
primFnExtCastFun StringType  = Just "string"
primFnExtCastFun CharType    = Just "char"
primFnExtCastFun DoubleType  = Just "double"
primFnExtCastFun WorldType   = Nothing

primFnExtCastIO : PrimType -> Bool
primFnExtCastIO StringType = True
primFnExtCastIO _          = False

primFnExtCast : PrimType -> PrimType -> PrimFnExt
primFnExtCast f t = fromMaybe NonImplemented $ do
  m <- primFnExtCastModule f
  g <- primFnExtCastFun t
  Just
    $ (if (primFnExtCastIO f || primFnExtCastIO t) then IOExt else PureExt)
    $ MkExtName "main" ["Idris", "Runtime", "Cast", m] g

total
primFnExt : PrimFn n -> PrimFnExt
primFnExt (Add ty)      = primFnExtAdd ty
primFnExt (Sub ty)      = primFnExtSub ty
primFnExt (Mul ty)      = primFnExtMul ty
primFnExt (Div ty)      = primFnExtDiv ty
primFnExt (Mod ty)      = primFnExtMod ty
primFnExt (Neg ty)      = primFnExtNeg ty
primFnExt (ShiftL ty)   = primFnExtShiftL ty
primFnExt (ShiftR ty)   = primFnExtShiftR ty
primFnExt (BAnd ty)     = primFnExtBAnd ty
primFnExt (BOr ty)      = primFnExtBOr ty
primFnExt (BXOr ty)     = primFnExtBXOr ty
primFnExt (LT ty)       = primFnExtLT ty
primFnExt (LTE ty)      = primFnExtLTE ty
primFnExt (EQ ty)       = primFnExtEQ ty
primFnExt (GTE ty)      = primFnExtGTE ty
primFnExt (GT ty)       = primFnExtGT ty
primFnExt StrLength     = PureExt $ MkExtName "main" ["Idris", "Runtime", "String"] "strLength"
primFnExt StrHead       = PureExt $ MkExtName "main" ["Idris", "Runtime", "String"] "strHead"
primFnExt StrTail       = IOExt   $ MkExtName "main" ["Idris", "Runtime", "String"] "strTail"
primFnExt StrIndex      = PureExt $ MkExtName "main" ["Idris", "Runtime", "String"] "strIndex"
primFnExt StrCons       = IOExt   $ MkExtName "main" ["Idris", "Runtime", "String"] "strCons"
primFnExt StrAppend     = IOExt   $ MkExtName "main" ["Idris", "Runtime", "String"] "strAppend"
primFnExt StrReverse    = IOExt   $ MkExtName "main" ["Idris", "Runtime", "String"] "strReverse"
primFnExt StrSubstr     = IOExt   $ MkExtName "main" ["Idris", "Runtime", "String"] "strSubstr"
primFnExt DoubleExp     = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "Double"] "exp"
primFnExt DoubleLog     = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "Double"] "log"
primFnExt DoublePow     = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "Double"] "pow"
primFnExt DoubleSin     = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "Double"] "sin"
primFnExt DoubleCos     = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "Double"] "cos"
primFnExt DoubleTan     = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "Double"] "tan"
primFnExt DoubleASin    = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "Double"] "asin"
primFnExt DoubleACos    = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "Double"] "acos"
primFnExt DoubleATan    = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "Double"] "atan"
primFnExt DoubleSqrt    = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "Double"] "sqrt"
primFnExt DoubleFloor   = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "Double"] "floor"
primFnExt DoubleCeiling = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "Double"] "ceiling"
primFnExt (Cast t1 t2)  = primFnExtCast t1 t2
primFnExt BelieveMe     = PureExt $ MkExtName "main" ["Idris", "Runtime", "BelieveMe"] "believeMe"
primFnExt Crash         = IOExt $ MkExtName "main" ["Idris", "Runtime", "Crash"] "crash"

export
compilePrimOp
  :  Ref STGCtxt STGContext
  => FC -> Core.Name.Name -> PrimFn ar -> Vect ar AVar
  -> Core (Expr Core.stgRepType)
compilePrimOp fc n p as = case primFnExt p of
  NonImplemented => coreFail $ InternalError "Non-implemented primop \{show p}."
  PureExt ex => do
    args <- traverse (map (mkArgSg . StgVarArg . binderId) . lookupLocalVarBinder n) $ toList as
    logLine Debug $ "compilePrimOp: PExt \{show p} \{show ex}"
    createExtSTGPureApp ex args
  IOExt ex => do
    args <- traverse (map (mkArgSg . StgVarArg . binderId) . lookupLocalVarBinder n) $ toList as
    logLine Debug $ "compilePrimOp: IOExt \{show p} \{show ex}"
    createExtSTGIOApp ex args
