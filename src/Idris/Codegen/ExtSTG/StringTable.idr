module Idris.Codegen.ExtSTG.StringTable

import Data.StringMap
import Core.Context
import Idris.Codegen.ExtSTG.Core
import Idris.Codegen.ExtSTG.STG

{-
Local String literals are registered during the compilation of the ANF.expression.

When compiling and LitString is found, we try to register as a TopLevel definition.
If there was already in the StringTable we just return the BinderID for the TopLevel
definition. The StgApp of that binder will evaluate to an Addr which needs to
be wrapped in an StgStringLit data constructor.

When the compilation of the expression is finished, the compilation of the module happens,
at that point the learned TopLevel string definitions needs to be registered in the module.
-}

export
StringTableMap : Type
StringTableMap = StringMap TopBinding

public export
data StringTableR : Type where

public export
StringTableRef : Type
StringTableRef = Ref StringTableR StringTableMap

newEntry
  :  {auto _ : UniqueMapRef}
  -> {auto _ : Ref Counter Int}
  -> FC -> String -> StringTableMap -> Core (StringTableMap, BinderId)
newEntry fc str m = case lookup str m of
  Nothing => do
    strBinder <- mkFreshSBinderStr GlobalScope fc "stringTableEntry"
    pure (insert str (StgTopStringLit strBinder str) m, Id strBinder)
  Just (StgTopStringLit strBinder _) =>
    pure (m, Id strBinder)
  Just (StgTopLifted _) =>
    coreFail $ InternalError $ "TopLifted find in StringTable for" ++ show str

||| Create a new StringTableRef that contains a StringTableMap
export
newStringTableRef : Core StringTableRef
newStringTableRef = newRef StringTableR empty

||| Try to register a new String and return its BinderId
export
registerString
  :  {auto _ : StringTableRef}
  -> {auto _ : UniqueMapRef}
  -> {auto _ : Ref Counter Int}
  -> FC -> String
  -> Core BinderId
registerString fc str = do
  m0 <- get StringTableR
  (m1, b) <- newEntry fc str m0
  put StringTableR m1
  pure b

||| Returns all the toplevel binders registered in the StringTable
export
topLevelBinders
  :  {auto _ : StringTableRef}
  -> Core (List TopBinding)
topLevelBinders = map values $ get StringTableR
