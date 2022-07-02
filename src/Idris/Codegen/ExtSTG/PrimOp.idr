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
primFnExtShiftL IntType = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "ShiftL"] "int"
-- primFnExtShiftL Int8Type = ?primFnExtShiftL_rhs_1
-- primFnExtShiftL Int16Type = ?primFnExtShiftL_rhs_2
-- primFnExtShiftL Int32Type = ?primFnExtShiftL_rhs_3
-- primFnExtShiftL Int64Type = ?primFnExtShiftL_rhs_4
-- primFnExtShiftL IntegerType = ?primFnExtShiftL_rhs_5
primFnExtShiftL Bits8Type  = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "ShiftL"] "bits8"
primFnExtShiftL Bits16Type = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "ShiftL"] "bits16"
primFnExtShiftL Bits32Type = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "ShiftL"] "bits32"
primFnExtShiftL Bits64Type = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "ShiftL"] "bits64"
primFnExtShiftL _ = NonImplemented

total
primFnExtShiftR : PrimType -> PrimFnExt
primFnExtShiftR IntType = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "ShiftR"] "int"
-- primFnExtShiftR Int8Type = ?primFnExtShiftR_rhs_1
-- primFnExtShiftR Int16Type = ?primFnExtShiftR_rhs_2
-- primFnExtShiftR Int32Type = ?primFnExtShiftR_rhs_3
-- primFnExtShiftR Int64Type = ?primFnExtShiftR_rhs_4
-- primFnExtShiftR IntegerType = ?primFnExtShiftR_rhs_5
primFnExtShiftR Bits8Type  = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "ShiftR"] "bits8"
primFnExtShiftR Bits16Type = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "ShiftR"] "bits16"
primFnExtShiftR Bits32Type = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "ShiftR"] "bits32"
primFnExtShiftR Bits64Type = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "ShiftR"] "bits64"
primFnExtShiftR _ = NonImplemented

total
primFnExtBAnd : PrimType -> PrimFnExt
-- primFnExtBAnd IntType = ?primFnExtBAnd_rhs_0
-- primFnExtBAnd Int8Type = ?primFnExtBAnd_rhs_1
-- primFnExtBAnd Int16Type = ?primFnExtBAnd_rhs_2
-- primFnExtBAnd Int32Type = ?primFnExtBAnd_rhs_3
-- primFnExtBAnd Int64Type = ?primFnExtBAnd_rhs_4
-- primFnExtBAnd IntegerType = ?primFnExtBAnd_rhs_5
primFnExtBAnd Bits8Type  = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "BAnd"] "bits8"
primFnExtBAnd Bits16Type = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "BAnd"] "bits16"
primFnExtBAnd Bits32Type = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "BAnd"] "bits32"
primFnExtBAnd Bits64Type = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "BAnd"] "bits64"
primFnExtBAnd _ = NonImplemented

total
primFnExtBOr : PrimType -> PrimFnExt
-- primFnExtBOr IntType = ?primFnExtBOr_rhs_0
-- primFnExtBOr Int8Type = ?primFnExtBOr_rhs_1
-- primFnExtBOr Int16Type = ?primFnExtBOr_rhs_2
-- primFnExtBOr Int32Type = ?primFnExtBOr_rhs_3
-- primFnExtBOr Int64Type = ?primFnExtBOr_rhs_4
-- primFnExtBOr IntegerType = ?primFnExtBOr_rhs_5
primFnExtBOr Bits8Type  = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "BOr"] "bits8"
primFnExtBOr Bits16Type = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "BOr"] "bits16"
primFnExtBOr Bits32Type = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "BOr"] "bits32"
primFnExtBOr Bits64Type = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "BOr"] "bits64"
primFnExtBOr _ = NonImplemented

total
primFnExtBXOr : PrimType -> PrimFnExt
-- primFnExtBXOr IntType = ?primFnExtBXOr_rhs_0
-- primFnExtBXOr Int8Type = ?primFnExtBXOr_rhs_1
-- primFnExtBXOr Int16Type = ?primFnExtBXOr_rhs_2
-- primFnExtBXOr Int32Type = ?primFnExtBXOr_rhs_3
-- primFnExtBXOr Int64Type = ?primFnExtBXOr_rhs_4
-- primFnExtBXOr IntegerType = ?primFnExtBXOr_rhs_5
primFnExtBXOr Bits8Type  = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "BXOr"] "bits8"
primFnExtBXOr Bits16Type = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "BXOr"] "bits16"
primFnExtBXOr Bits32Type = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "BXOr"] "bits32"
primFnExtBXOr Bits64Type = PureExt $ MkExtName "main" ["Idris", "Runtime", "PrimOp", "BXOr"] "bits64"
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

primFnExtCast : PrimType -> PrimType -> PrimFnExt
-- primFnExtCast IntType Int8Type = ?primFnExtCast_rhs_14
-- primFnExtCast IntType Int16Type = ?primFnExtCast_rhs_15
-- primFnExtCast IntType Int32Type = ?primFnExtCast_rhs_16
-- primFnExtCast IntType Int64Type = ?primFnExtCast_rhs_17
primFnExtCast IntType IntegerType = PureExt $ MkExtName "main" ["Idris", "Runtime", "Cast"] "intInteger"
-- primFnExtCast IntType Bits8Type = ?primFnExtCast_rhs_19
-- primFnExtCast IntType Bits16Type = ?primFnExtCast_rhs_20
-- primFnExtCast IntType Bits32Type = ?primFnExtCast_rhs_21
-- primFnExtCast IntType Bits64Type = ?primFnExtCast_rhs_22
primFnExtCast IntType StringType = IOExt $ MkExtName "main" ["Idris", "Runtime", "Cast"] "intString"
primFnExtCast IntType CharType   = PureExt $ MkExtName "main" ["Idris", "Runtime", "Cast"] "intChar"
-- primFnExtCast IntType DoubleType = ?primFnExtCast_rhs_25
-- primFnExtCast Int8Type IntType = ?primFnExtCast_rhs_5
-- primFnExtCast Int8Type Int16Type = ?primFnExtCast_rhs_15
-- primFnExtCast Int8Type Int32Type = ?primFnExtCast_rhs_16
-- primFnExtCast Int8Type Int64Type = ?primFnExtCast_rhs_17
-- primFnExtCast Int8Type IntegerType = ?primFnExtCast_rhs_18
-- primFnExtCast Int8Type Bits8Type = ?primFnExtCast_rhs_19
-- primFnExtCast Int8Type Bits16Type = ?primFnExtCast_rhs_20
-- primFnExtCast Int8Type Bits32Type = ?primFnExtCast_rhs_21
-- primFnExtCast Int8Type Bits64Type = ?primFnExtCast_rhs_22
primFnExtCast Int8Type StringType = IOExt $ MkExtName "main" ["Idris", "Runtime", "Cast"] "int8String"
-- primFnExtCast Int8Type CharType = ?primFnExtCast_rhs_24
-- primFnExtCast Int8Type DoubleType = ?primFnExtCast_rhs_25
-- primFnExtCast Int8Type WorldType = ?primFnExtCast_rhs_26
-- primFnExtCast Int16Type IntType = ?primFnExtCast_rhs_5
-- primFnExtCast Int16Type Int8Type = ?primFnExtCast_rhs_14
-- primFnExtCast Int16Type Int32Type = ?primFnExtCast_rhs_16
-- primFnExtCast Int16Type Int64Type = ?primFnExtCast_rhs_17
-- primFnExtCast Int16Type IntegerType = ?primFnExtCast_rhs_18
-- primFnExtCast Int16Type Bits8Type = ?primFnExtCast_rhs_19
-- primFnExtCast Int16Type Bits16Type = ?primFnExtCast_rhs_20
-- primFnExtCast Int16Type Bits32Type = ?primFnExtCast_rhs_21
-- primFnExtCast Int16Type Bits64Type = ?primFnExtCast_rhs_22
primFnExtCast Int16Type StringType = IOExt $ MkExtName "main" ["Idris", "Runtime", "Cast"] "int16String"
-- primFnExtCast Int16Type CharType = ?primFnExtCast_rhs_24
-- primFnExtCast Int16Type DoubleType = ?primFnExtCast_rhs_25
-- primFnExtCast Int32Type IntType = ?primFnExtCast_rhs_5
-- primFnExtCast Int32Type Int8Type = ?primFnExtCast_rhs_14
-- primFnExtCast Int32Type Int16Type = ?primFnExtCast_rhs_15
-- primFnExtCast Int32Type Int64Type = ?primFnExtCast_rhs_17
-- primFnExtCast Int32Type IntegerType = ?primFnExtCast_rhs_18
-- primFnExtCast Int32Type Bits8Type = ?primFnExtCast_rhs_19
-- primFnExtCast Int32Type Bits16Type = ?primFnExtCast_rhs_20
-- primFnExtCast Int32Type Bits32Type = ?primFnExtCast_rhs_21
-- primFnExtCast Int32Type Bits64Type = ?primFnExtCast_rhs_22
primFnExtCast Int32Type StringType = IOExt $ MkExtName "main" ["Idris", "Runtime", "Cast"] "int32String"
-- primFnExtCast Int32Type CharType = ?primFnExtCast_rhs_24
-- primFnExtCast Int32Type DoubleType = ?primFnExtCast_rhs_25
-- primFnExtCast Int64Type IntType = ?primFnExtCast_rhs_5
-- primFnExtCast Int64Type Int8Type = ?primFnExtCast_rhs_14
-- primFnExtCast Int64Type Int16Type = ?primFnExtCast_rhs_15
-- primFnExtCast Int64Type Int32Type = ?primFnExtCast_rhs_16
-- primFnExtCast Int64Type IntegerType = ?primFnExtCast_rhs_18
-- primFnExtCast Int64Type Bits8Type = ?primFnExtCast_rhs_19
-- primFnExtCast Int64Type Bits16Type = ?primFnExtCast_rhs_20
-- primFnExtCast Int64Type Bits32Type = ?primFnExtCast_rhs_21
-- primFnExtCast Int64Type Bits64Type = ?primFnExtCast_rhs_22
primFnExtCast Int64Type StringType = IOExt $ MkExtName "main" ["Idris", "Runtime", "Cast"] "int64String"
-- primFnExtCast Int64Type CharType = ?primFnExtCast_rhs_24
-- primFnExtCast Int64Type DoubleType = ?primFnExtCast_rhs_25
primFnExtCast IntegerType IntType    = PureExt $ MkExtName "main" ["Idris", "Runtime", "Integer"] "castInt"
-- primFnExtCast IntegerType Int8Type = ?primFnExtCast_rhs_15
-- primFnExtCast IntegerType Int16Type = ?primFnExtCast_rhs_16
-- primFnExtCast IntegerType Int32Type = ?primFnExtCast_rhs_17
-- primFnExtCast IntegerType Int64Type = ?primFnExtCast_rhs_18
primFnExtCast IntegerType Bits8Type  = PureExt $ MkExtName "main" ["Idris", "Runtime", "Cast"] "integerBits8"
primFnExtCast IntegerType Bits16Type = PureExt $ MkExtName "main" ["Idris", "Runtime", "Cast"] "integerBits16"
primFnExtCast IntegerType Bits32Type = PureExt $ MkExtName "main" ["Idris", "Runtime", "Cast"] "integerBits32"
primFnExtCast IntegerType Bits64Type = PureExt $ MkExtName "main" ["Idris", "Runtime", "Cast"] "integerBits64"
primFnExtCast IntegerType StringType = IOExt   $ MkExtName "main" ["Idris", "Runtime", "Integer"] "toStr"
-- primFnExtCast IntegerType CharType = ?primFnExtCast_rhs_25
primFnExtCast IntegerType DoubleType = PureExt $ MkExtName "main" ["Idris", "Runtime", "Cast"] "integerDouble"
-- primFnExtCast Bits8Type IntType = ?primFnExtCast_rhs_13
-- primFnExtCast Bits8Type Int8Type = ?primFnExtCast_rhs_14
-- primFnExtCast Bits8Type Int16Type = ?primFnExtCast_rhs_15
-- primFnExtCast Bits8Type Int32Type = ?primFnExtCast_rhs_16
-- primFnExtCast Bits8Type Int64Type = ?primFnExtCast_rhs_17
-- primFnExtCast Bits8Type IntegerType = ?primFnExtCast_rhs_18
-- primFnExtCast Bits8Type Bits16Type = ?primFnExtCast_rhs_20
-- primFnExtCast Bits8Type Bits32Type = ?primFnExtCast_rhs_21
-- primFnExtCast Bits8Type Bits64Type = ?primFnExtCast_rhs_22
primFnExtCast Bits8Type StringType = IOExt $ MkExtName "main" ["Idris", "Runtime", "Cast"] "bits8String"
-- primFnExtCast Bits8Type CharType = ?primFnExtCast_rhs_24
-- primFnExtCast Bits8Type DoubleType = ?primFnExtCast_rhs_25
-- primFnExtCast Bits16Type IntType = ?primFnExtCast_rhs_13
-- primFnExtCast Bits16Type Int8Type = ?primFnExtCast_rhs_14
-- primFnExtCast Bits16Type Int16Type = ?primFnExtCast_rhs_15
-- primFnExtCast Bits16Type Int32Type = ?primFnExtCast_rhs_16
-- primFnExtCast Bits16Type Int64Type = ?primFnExtCast_rhs_17
-- primFnExtCast Bits16Type IntegerType = ?primFnExtCast_rhs_18
-- primFnExtCast Bits16Type Bits8Type = ?primFnExtCast_rhs_19
-- primFnExtCast Bits16Type Bits32Type = ?primFnExtCast_rhs_21
-- primFnExtCast Bits16Type Bits64Type = ?primFnExtCast_rhs_22
primFnExtCast Bits16Type StringType = IOExt $ MkExtName "main" ["Idris", "Runtime", "Cast"] "bits16String"
-- primFnExtCast Bits16Type CharType = ?primFnExtCast_rhs_24
-- primFnExtCast Bits16Type DoubleType = ?primFnExtCast_rhs_25
-- primFnExtCast Bits32Type IntType = ?primFnExtCast_rhs_20
-- primFnExtCast Bits32Type Int8Type = ?primFnExtCast_rhs_26
-- primFnExtCast Bits32Type Int16Type = ?primFnExtCast_rhs_27
-- primFnExtCast Bits32Type Int32Type = ?primFnExtCast_rhs_28
-- primFnExtCast Bits32Type Int64Type = ?primFnExtCast_rhs_29
-- primFnExtCast Bits32Type IntegerType = ?primFnExtCast_rhs_30
-- primFnExtCast Bits32Type Bits8Type = ?primFnExtCast_rhs_31
-- primFnExtCast Bits32Type Bits16Type = ?primFnExtCast_rhs_32
-- primFnExtCast Bits32Type Bits64Type = ?primFnExtCast_rhs_34
primFnExtCast Bits32Type StringType = IOExt $ MkExtName "main" ["Idris", "Runtime", "Cast"] "bits32String"
-- primFnExtCast Bits32Type CharType = ?primFnExtCast_rhs_36
-- primFnExtCast Bits32Type DoubleType = ?primFnExtCast_rhs_37
-- primFnExtCast Bits64Type IntType = ?primFnExtCast_rhs_33
-- primFnExtCast Bits64Type Int8Type = ?primFnExtCast_rhs_38
-- primFnExtCast Bits64Type Int16Type = ?primFnExtCast_rhs_39
-- primFnExtCast Bits64Type Int32Type = ?primFnExtCast_rhs_40
-- primFnExtCast Bits64Type Int64Type = ?primFnExtCast_rhs_41
-- primFnExtCast Bits64Type IntegerType = ?primFnExtCast_rhs_42
-- primFnExtCast Bits64Type Bits8Type = ?primFnExtCast_rhs_43
-- primFnExtCast Bits64Type Bits16Type = ?primFnExtCast_rhs_44
-- primFnExtCast Bits64Type Bits32Type = ?primFnExtCast_rhs_45
primFnExtCast Bits64Type StringType = IOExt $ MkExtName "main" ["Idris", "Runtime", "Cast"] "bits64String"
-- primFnExtCast Bits64Type CharType = ?primFnExtCast_rhs_48
-- primFnExtCast Bits64Type DoubleType = ?primFnExtCast_rhs_49
primFnExtCast StringType IntType = PureExt $ MkExtName "main" ["Idris", "Runtime", "Cast"] "stringInt"
-- primFnExtCast StringType Int8Type = ?primFnExtCast_rhs_50
-- primFnExtCast StringType Int16Type = ?primFnExtCast_rhs_51
-- primFnExtCast StringType Int32Type = ?primFnExtCast_rhs_52
-- primFnExtCast StringType Int64Type = ?primFnExtCast_rhs_53
primFnExtCast StringType IntegerType = PureExt $ MkExtName "main" ["Idris", "Runtime", "Cast"] "stringInteger"
-- primFnExtCast StringType Bits8Type = ?primFnExtCast_rhs_55
-- primFnExtCast StringType Bits16Type = ?primFnExtCast_rhs_56
-- primFnExtCast StringType Bits32Type = ?primFnExtCast_rhs_57
-- primFnExtCast StringType Bits64Type = ?primFnExtCast_rhs_58
-- primFnExtCast StringType CharType = ?primFnExtCast_rhs_60
-- primFnExtCast StringType DoubleType = ?primFnExtCast_rhs_61
primFnExtCast CharType IntType = PureExt $ MkExtName "main" ["Idris", "Runtime", "Cast"] "charInt"
-- primFnExtCast CharType Int8Type = ?primFnExtCast_rhs_62
-- primFnExtCast CharType Int16Type = ?primFnExtCast_rhs_63
-- primFnExtCast CharType Int32Type = ?primFnExtCast_rhs_64
-- primFnExtCast CharType Int64Type = ?primFnExtCast_rhs_65
primFnExtCast CharType IntegerType = PureExt $ MkExtName "main" ["Idris", "Runtime", "Integer"] "fromChar"
-- primFnExtCast CharType Bits8Type = ?primFnExtCast_rhs_67
-- primFnExtCast CharType Bits16Type = ?primFnExtCast_rhs_68
-- primFnExtCast CharType Bits32Type = ?primFnExtCast_rhs_69
-- primFnExtCast CharType Bits64Type = ?primFnExtCast_rhs_70
primFnExtCast CharType StringType = IOExt $ MkExtName "main" ["Idris", "Runtime", "Cast"] "charString"
-- primFnExtCast CharType DoubleType = ?primFnExtCast_rhs_73
-- primFnExtCast DoubleType IntType = ?primFnExtCast_rhs_72
-- primFnExtCast DoubleType Int8Type = ?primFnExtCast_rhs_74
-- primFnExtCast DoubleType Int16Type = ?primFnExtCast_rhs_75
-- primFnExtCast DoubleType Int32Type = ?primFnExtCast_rhs_76
-- primFnExtCast DoubleType Int64Type = ?primFnExtCast_rhs_77
-- primFnExtCast DoubleType IntegerType = ?primFnExtCast_rhs_78
-- primFnExtCast DoubleType Bits8Type = ?primFnExtCast_rhs_79
-- primFnExtCast DoubleType Bits16Type = ?primFnExtCast_rhs_80
-- primFnExtCast DoubleType Bits32Type = ?primFnExtCast_rhs_81
-- primFnExtCast DoubleType Bits64Type = ?primFnExtCast_rhs_82
primFnExtCast DoubleType StringType = IOExt $ MkExtName "main" ["Idris", "Runtime", "Cast"] "doubleString"
-- primFnExtCast DoubleType CharType = ?primFnExtCast_rhs_84
primFnExtCast _ _ = NonImplemented

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
    args <- traverse (map (mkArgSg . StgVarArg) . mkBinderIdVar fc n) $ toList as
    logLine Debug $ "compilePrimOp: PExt \{show p} \{show ex}"
    createExtSTGPureApp ex args
  IOExt ex => do
    args <- traverse (map (mkArgSg . StgVarArg) . mkBinderIdVar fc n) $ toList as
    logLine Debug $ "compilePrimOp: IOExt \{show p} \{show ex}"
    createExtSTGIOApp ex args
