module Idris.Codegen.ExtSTG.StringTable

import Core.Context
import Libraries.Data.StringMap

import Idris.Codegen.ExtSTG.Configuration
import Idris.Codegen.ExtSTG.Context
import Idris.Codegen.ExtSTG.Core
import Idris.Codegen.ExtSTG.STG

%default total

{-
Local String literals are registered during the compilation of the ANF.expression.

When compiling and LitString is found, we try to register as a TopLevel definition.
If there was already in the StringTable we just return the BinderID for the TopLevel
definition. The StgApp of that binder will evaluate to an Addr which needs to
be wrapped in an StgStringLit data constructor.

When the compilation of the expression is finished, the compilation of the module happens,
at that point the learned TopLevel string definitions needs to be registered in the module.
-}

||| Try to register a new String and return its BinderId
export
registerString
  :  Ref STGCtxt STGContext
  => FC -> String -> Core (BinderId (SingleValue AddrRep))
registerString fc str = do
  top <- lookupStringTable str
  case top of
    Nothing => do
      logLine Debug "StringTable: new entry \{show fc} \{str}"
      strBinder <- mkFreshSBinderStr GlobalScope fc "stringTableEntry"
      insertStringTable str (StgTopStringLit strBinder str)
      pure (binderId strBinder)
    Just (StgTopStringLit strBinder _) =>
      pure (binderId strBinder) -- TODO: Check this binder
    Just (StgTopLifted _) =>
      coreFail $ InternalError $ "TopLifted find in StringTable for \{show str}"

||| Returns all the toplevel binders registered in the StringTable
export
topLevelBinders
  :  Ref STGCtxt STGContext
  => Core (List TopBinding)
topLevelBinders = do
  logLine Debug "Get all StringTable entries."
  map values $ getStringTable
