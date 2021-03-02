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
  :  {auto _ : UniqueMapRef}
  -> {auto _ : Ref Counter Int}
  -> {auto _ : DataTypeMapRef}
  -> Core ()
defineErasedADT = do
  t <- MkTyConId   <$> uniqueForType ERASED_TYPE_NAME
  n <- MkDataConId <$> uniqueForTerm ERASED_CON_NAME
  d <- pure $ MkSTyCon ERASED_TYPE_NAME t
                         [ MkSDataCon ERASED_CON_NAME n
                                        (AlgDataCon [])
                                        !(mkSBinderStr emptyFC ("mk" ++ ERASED_CON_NAME))
                                        (SsUnhelpfulSpan "<no location>") ]
                         (SsUnhelpfulSpan "<no location>")
  defineDataType (MkUnitId MAIN_UNIT) (MkModuleName MAIN_MODULE) d

export
erasedTopBinding
  :  {auto _ : UniqueMapRef}
  -> {auto _ : Ref Counter Int}
  -> Core TopBinding
erasedTopBinding = do
  dtc <- MkDataConId <$> uniqueForTerm ERASED_CON_NAME
  rhs <- pure $ StgRhsCon dtc []
  erasedNameBinder <- mkSBinderStr emptyFC ERASED_TOPLEVEL_NAME
  binding <- pure $ StgNonRec erasedNameBinder rhs
  pure $ StgTopLifted binding
