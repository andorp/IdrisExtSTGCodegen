module Idris.Codegen.ExtSTG.Binders

import Idris.Codegen.ExtSTG.Core
import Idris.Codegen.ExtSTG.ExternalTopIds

import Compiler.ANF
import Core.Context
import Core.Core
import Core.TT
import Idris.Codegen.ExtSTG.STG
import Prelude
import Idris.Codegen.ExtSTG.ExtName
import Idris.Codegen.ExtSTG.Context
import Idris.Codegen.ExtSTG.ADTAlias


-- export
mkSBinderTyCon
  :  Ref STGCtxt STGContext
  => FC -> String -- TODO : Constant
  -> Core (SBinder Core.stgRepType)
mkSBinderTyCon = mkSBinder TypeBinder GlobalScope


||| Create a local binder for a given name. Used in STG.String module
export
mkSBinderLocalStr
  :  Ref STGCtxt STGContext
  => String
  -> Core (SBinder Core.stgRepType)
mkSBinderLocalStr n = mkSBinder TermBinder LocalScope emptyFC n

||| Create a local binder for a given name. Used in STG.String module
export
mkSBinderRepLocalStr
  :  Ref STGCtxt STGContext
  => (rep : RepType)
  -> String
  -> Core (SBinder rep)
mkSBinderRepLocalStr r n = mkSBinderRep TermBinder LocalScope r emptyFC n

||| Create a binder for ANF local variable.
export
mkSBinderLocal
  :  Ref STGCtxt STGContext
  => FC -> Core.Name.Name -> Int
  -> Core (SBinder Core.stgRepType)
mkSBinderLocal f n x = mkSBinder TermBinder LocalScope f (binderStr n ++ ":" ++ show x)

||| Create a binder for ANF local variable.
export
mkSBinderRepLocal
  :  Ref STGCtxt STGContext
  => (rep : RepType) -> FC -> Core.Name.Name -> Int
  -> Core (SBinder rep)
mkSBinderRepLocal r f n x = mkSBinderRep TermBinder LocalScope r f (binderStr n ++ ":" ++ show x)

export
mkSBinderName
  :  Ref STGCtxt STGContext
  => FC -> Core.Name.Name
  -> Core (SBinder Core.stgRepType)
mkSBinderName f n = mkSBinder TermBinder GlobalScope f (binderStr n)

||| Create a binder for a function that is defined in another STG module.
||| Primary use case for this is the STG-FFI, or exported from the module.
export
mkSBinderExtId
  :  Ref STGCtxt STGContext
  => FC -> String
  -> Core (SBinder Core.stgRepType)
mkSBinderExtId = mkSBinder TermBinder HaskellExported

||| Create a unique SBinder for the local variable.
|||
||| If the variable is local it returns the same binder for the variable.
||| If the variable is null (erased?) than this binder shouldn't be used at all.
export
mkSBinderVar
  :  Ref STGCtxt STGContext
  => FC -> Core.Name.Name -> AVar
  -> Core (SBinder Core.stgRepType)
mkSBinderVar fc n (ALocal x) = mkSBinder TermBinder LocalScope fc (binderStr n ++ ":" ++ show x)
mkSBinderVar fc n ANull      = mkFreshSBinderStr LocalScope fc (binderStr n ++ ":null")

||| Lookup a binder based on the name encoded as String
export
mkBinderIdStr
  :  Ref STGCtxt STGContext
  => String
  -> Core (BinderId Core.stgRepType)
mkBinderIdStr = map MkBinderId . uniqueForTerm -- TODO: Is this right?

export
mkBinderIdName
  :  Ref STGCtxt STGContext
  => Core.Name.Name
  -> Core (BinderId Core.stgRepType)
mkBinderIdName = map MkBinderId . uniqueForTerm . binderStr -- TODO: Is this right?

export
mkBinderIdVarRep
  :  Ref STGCtxt STGContext
  => FC -> Core.Name.Name -> (r : RepType) -> AVar
  -> Core (BinderId r)
mkBinderIdVarRep fc n r (ALocal x) = MkBinderId <$> uniqueForTerm (binderStr n ++ ":" ++ show x)
mkBinderIdVarRep fc n r ANull      = coreFail $ InternalError "mkBinderIdVarRep: ANull"

export
mkBinderIdVar
  :  Ref STGCtxt STGContext
  => FC -> Core.Name.Name -> AVar
  -> Core (BinderId Core.stgRepType)
mkBinderIdVar fc n (ALocal x) = MkBinderId <$> uniqueForTerm (binderStr n ++ ":" ++ show x)
mkBinderIdVar fc n ANull      = extNameLR erasedExtName


||| Create a StdVarArg for the Argument of a function application.
|||
||| If the argument is ANull/erased, then it returns a NulAddr literal
export
mkStgArg
  :  Ref STGCtxt STGContext
  => FC -> Core.Name.Name -> AVar
  -> Core ArgSg
mkStgArg fc n a = mkArgSg . StgVarArg <$> (mkBinderIdVar fc n a)
