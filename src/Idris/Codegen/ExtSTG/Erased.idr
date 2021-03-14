module Idris.Codegen.ExtSTG.Erased

import Core.Core
import Core.Context
import Compiler.ANF
import Idris.Codegen.ExtSTG.Core
import Idris.Codegen.ExtSTG.STG


ERASED_TYPE_NAME : String
ERASED_TYPE_NAME = "Erased"

ERASED_CON_NAME : String
ERASED_CON_NAME = "Erased"

export
ERASED_TOPLEVEL_NAME : String
ERASED_TOPLEVEL_NAME = "idrisErasedValue"

export
defineErasedADT
  :  UniqueMapRef
  => Ref Counter Int
  => DataTypeMapRef
  => Core ()
defineErasedADT = do
  d <- createSTyCon
        (ERASED_TYPE_NAME, SsUnhelpfulSpan ERASED_TYPE_NAME)
        [ (ERASED_CON_NAME, AlgDataCon [], SsUnhelpfulSpan ERASED_CON_NAME)]
  defineDataType (MkUnitId MAIN_UNIT) (MkModuleName MAIN_MODULE) d

export
erasedTopBinding
  :  UniqueMapRef
  => Ref Counter Int
  => DataTypeMapRef
  => Core TopBinding
erasedTopBinding = do
  dtc <- mkDataConIdStr ERASED_CON_NAME
  rhs <- pure $ StgRhsCon dtc []
  erasedNameBinder <- mkSBinderStr emptyFC ERASED_TOPLEVEL_NAME
  binding <- pure $ StgNonRec erasedNameBinder rhs
  pure $ StgTopLifted binding
