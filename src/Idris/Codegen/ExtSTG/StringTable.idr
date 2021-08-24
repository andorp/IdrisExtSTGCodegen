module Idris.Codegen.ExtSTG.StringTable

import Libraries.Data.StringMap
import Core.Context
import Idris.Codegen.ExtSTG.Core
import Idris.Codegen.ExtSTG.STG
import Idris.Codegen.ExtSTG.Context

{-
Local String literals are registered during the compilation of the ANF.expression.

When compiling and LitString is found, we try to register as a TopLevel definition.
If there was already in the StringTable we just return the BinderID for the TopLevel
definition. The StgApp of that binder will evaluate to an Addr which needs to
be wrapped in an StgStringLit data constructor.

When the compilation of the expression is finished, the compilation of the module happens,
at that point the learned TopLevel string definitions needs to be registered in the module.
-}

newEntry
  :  Ref STGCtxt STGContext
  => FC -> String -> StringTableMap -> Core (StringTableMap, BinderId (SingleValue AddrRep))
newEntry fc str m = case lookup str m of
  Nothing => do
    strBinder <- mkFreshSBinderRepStr GlobalScope (SingleValue AddrRep) fc "stringTableEntry"
    pure (insert str (StgTopStringLit strBinder str) m, binderId strBinder)
  Just (StgTopStringLit strBinder _) =>
    pure (m, binderId strBinder) -- TODO: Check this binder
  Just (StgTopLifted _) =>
    coreFail $ InternalError $ "TopLifted find in StringTable for" ++ show str

||| Try to register a new String and return its BinderId
export
registerString
  :  Ref STGCtxt STGContext
  => FC -> String
  -> Core (BinderId (SingleValue AddrRep))
registerString fc str = do
  m0 <- getSTGCtxt stringTable
  (m1, b) <- newEntry fc str m0
  modifySTGCtxt $ record { stringTable = m1 }
  pure b

||| Returns all the toplevel binders registered in the StringTable
export
topLevelBinders
  :  Ref STGCtxt STGContext
  => Core (List TopBinding)
topLevelBinders = map values $ getSTGCtxt stringTable
