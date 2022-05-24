module Idris.Codegen.ExtSTG.ConstantRep

import Core.TT
import Core.Core
import Idris.Codegen.ExtSTG.STG

namespace TypeConstant

  public export
  data TypeConstant : PrimType -> Type where
    IntConstant    : TypeConstant IntType
    BigIntConstant : TypeConstant IntegerType
    Int8Constant   : TypeConstant Int8Type
    Int16Constant  : TypeConstant Int16Type
    Int32Constant  : TypeConstant Int32Type
    Int64Constant  : TypeConstant Int64Type
    Byte8Constant  : TypeConstant Bits8Type
    Byte16Constant : TypeConstant Bits16Type
    Byte32Constant : TypeConstant Bits32Type
    Byte64Constant : TypeConstant Bits64Type
    CharConstant   : TypeConstant CharType
    DoubleConstant : TypeConstant DoubleType
    WorldConstant  : TypeConstant WorldType

  export
  Show (TypeConstant t) where
    show IntConstant    = "IntConstant"
    show BigIntConstant = "BigIntConstant"
    show Int8Constant   = "Int8Constant"
    show Int16Constant  = "Int16Constant"
    show Int32Constant  = "Int32Constant"
    show Int64Constant  = "Int64Constant"
    show Byte8Constant  = "Byte8Constant"
    show Byte16Constant = "Byte16Constant"
    show Byte32Constant = "Byte32Constant"
    show Byte64Constant = "Byte64Constant"
    show CharConstant   = "CharConstant"
    show DoubleConstant = "DoubleConstant"
    show WorldConstant  = "WorldConstant"

namespace ValueConstant

  public export
  data ValueConstant : Constant -> Type where
    IntConstant    : ValueConstant (I x)
    BigIntConstant : ValueConstant (BI x)
    Int8Constant   : ValueConstant (I8 x)
    Int16Constant  : ValueConstant (I16 x)
    Int32Constant  : ValueConstant (I32 x)
    Int64Constant  : ValueConstant (I64 x)
    Byte8Constant  : ValueConstant (B8 x)
    Byte16Constant : ValueConstant (B16 x)
    Byte32Constant : ValueConstant (B32 x)
    Byte64Constant : ValueConstant (B64 x)
    CharConstant   : ValueConstant (Ch x)
    DoubleConstant : ValueConstant (Db x)
    WorldConstant  : ValueConstant WorldVal

  export
  Show (ValueConstant c) where
    show IntConstant    = "IntConstant"
    show BigIntConstant = "BigIntConstant"
    show Int8Constant   = "Int8Constant"
    show Int16Constant  = "Int16Constant"
    show Int32Constant  = "Int32Constant"
    show Int64Constant  = "Int64Constant"
    show Byte8Constant  = "Byte8Constant"
    show Byte16Constant = "Byte16Constant"
    show Byte32Constant = "Byte32Constant"
    show Byte64Constant = "Byte64Constant"
    show CharConstant   = "CharConstant"
    show DoubleConstant = "DoubleConstant"
    show WorldConstant  = "WorldConstant"

  public export
  checkValueConstant : (c : Constant) -> Maybe (ValueConstant c)
  checkValueConstant (I _)    = Just IntConstant
  checkValueConstant (BI _)   = Just BigIntConstant
  checkValueConstant (I8 _)   = Just Int8Constant
  checkValueConstant (I16 _)  = Just Int16Constant
  checkValueConstant (I32 _)  = Just Int32Constant
  checkValueConstant (I64 _)  = Just Int64Constant
  checkValueConstant (B8 _)   = Just Byte8Constant
  checkValueConstant (B16 _)  = Just Byte16Constant
  checkValueConstant (B32 _)  = Just Byte32Constant
  checkValueConstant (B64 _)  = Just Byte64Constant
  checkValueConstant (Ch _)   = Just CharConstant
  checkValueConstant (Db _)   = Just DoubleConstant
  checkValueConstant WorldVal = Just WorldConstant
  checkValueConstant _        = Nothing

  export
  checkValueConstantM : (c : Constant) -> Core (ValueConstant c)
  checkValueConstantM c = case checkValueConstant c of
    Nothing => coreFail $ InternalError $ "checkValueConstantM: " ++ show c ++ " is not a value constant."
    Just vc => pure vc


namespace ConstantValueTypeAssoc

  public export
  data ConstantValueTypeAssoc : Constant -> PrimType -> Type where
    IntConstant    : ConstantValueTypeAssoc (I x)     IntType
    BigIntConstant : ConstantValueTypeAssoc (BI _)    IntegerType
    Int8Constant   : ConstantValueTypeAssoc (I8 _)    Int8Type
    Int16Constant  : ConstantValueTypeAssoc (I16 _)   Int16Type
    Int32Constant  : ConstantValueTypeAssoc (I32 _)   Int32Type
    Int64Constant  : ConstantValueTypeAssoc (I64 _)   Int64Type
    Byte8Constant  : ConstantValueTypeAssoc (B8 _)    Bits8Type
    Byte16Constant : ConstantValueTypeAssoc (B16 _)   Bits16Type
    Byte32Constant : ConstantValueTypeAssoc (B32 _)   Bits32Type
    Byte64Constant : ConstantValueTypeAssoc (B64 _)   Bits64Type
    CharConstant   : ConstantValueTypeAssoc (Ch _)    CharType
    DoubleConstant : ConstantValueTypeAssoc (Db _)    DoubleType
    WorldConstant  : ConstantValueTypeAssoc WorldVal  WorldType

  export
  Show (ConstantValueTypeAssoc c t) where
    show IntConstant    = "IntConstant"
    show BigIntConstant = "BigIntConstant"
    show Int8Constant   = "Int8Constant"
    show Int16Constant  = "Int16Constant"
    show Int32Constant  = "Int32Constant"
    show Int64Constant  = "Int64Constant"
    show Byte8Constant  = "Byte8Constant"
    show Byte16Constant = "Byte16Constant"
    show Byte32Constant = "Byte32Constant"
    show Byte64Constant = "Byte64Constant"
    show CharConstant   = "CharConstant"
    show DoubleConstant = "DoubleConstant"
    show WorldConstant  = "WorldConstant"

public export
typeConstantPrimReps : (t : PrimType) -> TypeConstant t => List PrimRep
typeConstantPrimReps IntType      = [IntRep]
typeConstantPrimReps IntegerType  = [IntRep]
typeConstantPrimReps Int8Type     = [Int8Rep]
typeConstantPrimReps Int16Type    = [Int16Rep]
typeConstantPrimReps Int32Type    = [Int32Rep]
typeConstantPrimReps Int64Type    = [Int64Rep]
typeConstantPrimReps Bits8Type    = [Word8Rep]
typeConstantPrimReps Bits16Type   = [Word16Rep]
typeConstantPrimReps Bits32Type   = [Word32Rep]
typeConstantPrimReps Bits64Type   = [Word64Rep]
typeConstantPrimReps CharType     = [WordRep]
typeConstantPrimReps DoubleType   = [DoubleRep]
typeConstantPrimReps WorldType    = []

public export
valueConstantPrimReps : (c : Constant) -> ValueConstant c => List PrimRep
valueConstantPrimReps (I _)    = [IntRep]
valueConstantPrimReps (BI _)   = [IntRep]
valueConstantPrimReps (I8 _)   = [Int8Rep]
valueConstantPrimReps (I16 _)  = [Int16Rep]
valueConstantPrimReps (I32 _)  = [Int32Rep]
valueConstantPrimReps (I64 _)  = [Int64Rep]
valueConstantPrimReps (B8 _)   = [Word8Rep]
valueConstantPrimReps (B16 _)  = [Word16Rep]
valueConstantPrimReps (B32 _)  = [Word32Rep]
valueConstantPrimReps (B64 _)  = [Word64Rep]
valueConstantPrimReps (Ch _)   = [Word8Rep]
valueConstantPrimReps (Db _)   = [DoubleRep]
valueConstantPrimReps WorldVal = []
