module Idris.Codegen.ExtSTG.Erased

import Core.Core
import Core.Context
import Compiler.ANF
import Idris.Codegen.ExtSTG.Core
import Idris.Codegen.ExtSTG.STG
import Idris.Codegen.ExtSTG.Context


ERASED_TYPE_NAME : String
ERASED_TYPE_NAME = "Erased"

ERASED_CON_NAME : String
ERASED_CON_NAME = "Erased"

export
ERASED_TOPLEVEL_NAME : String
ERASED_TOPLEVEL_NAME = "idrisErasedValue"

export
defineErasedADT : Ref STGCtxt STGContext => Core ()
defineErasedADT = do
  d <- createSTyCon
        (ERASED_TYPE_NAME, SsUnhelpfulSpan ERASED_TYPE_NAME)
        [ (ERASED_CON_NAME, AlgDataCon [], SsUnhelpfulSpan ERASED_CON_NAME)]
  defineDataType (MkUnitId MAIN_UNIT) (MkModuleName MAIN_MODULE) d

export
erasedTopBinding : Ref STGCtxt STGContext => Core TopBinding
erasedTopBinding = do
  dtc <- mkDataConIdStr ERASED_CON_NAME
  rhs <- pure $ StgRhsCon dtc []
  erasedNameBinder <- mkSBinderStr emptyFC ERASED_TOPLEVEL_NAME
  binding <- pure $ StgNonRec erasedNameBinder rhs
  pure $ StgTopLifted binding
