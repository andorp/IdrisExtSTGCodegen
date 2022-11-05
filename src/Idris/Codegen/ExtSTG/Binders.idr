module Idris.Codegen.ExtSTG.Binders

import Core.Name
import Idris.Codegen.ExtSTG.STG
import Idris.Codegen.ExtSTG.ExtName
import Data.SortedMap

%hide STG.Name
%default total

public export
FunctionBinder : Type
FunctionBinder = SBinder (SingleValue LiftedRep)

public export
LocalVarBinder : Type
LocalVarBinder = SBinder (SingleValue LiftedRep)

public export
FFIBinder : Type
FFIBinder = SBinder (SingleValue LiftedRep)

export
record Binders where
  constructor MkBinders
  localVars   : SortedMap (Name, Int) LocalVarBinder
  functions   : SortedMap Name        FunctionBinder
  ffiBinders  : SortedMap ExtName     FFIBinder
  voidHash    : (ExtName, SBinder (SingleValue VoidRep))

voidHashBinder : (ExtName, SBinder (SingleValue VoidRep))
voidHashBinder =
  ( MkExtName "ghc-prim" ["GHC", "Prim"] "void#"
  , MkSBinder
    { binderName    = "void#"
    , binderId      = MkBinderId (MkUnique '0' 21)
    , binderTypeSig = "void#"
    , binderScope   = HaskellExported
    , binderDetails = VanillaId
    , binderInfo    = "void#"
    , binderDefLoc  = SsUnhelpfulSpan "void#"
    }
  )

export
createBinders : Binders
createBinders = MkBinders
  { localVars   = empty
  , functions   = empty
  , ffiBinders  = empty
  , voidHash    = voidHashBinder
  }

export
getExtBinders : Binders -> List (ExtName, SBinderSg)
getExtBinders binders =
  [mapSnd (MkDPair _) binders.voidHash] ++
  (map (mapSnd (MkDPair _)) $ toList $ binders.ffiBinders)

export
insertFunction : Name -> FunctionBinder -> Binders -> Either String Binders
insertFunction n b binders = do
  let Nothing = lookup n binders.functions
      | Just _ => Left "Function \{show n} is already defined."
  Right $ { functions $= insert n b } binders

export
lookupFunction : Name -> Binders -> Maybe FunctionBinder
lookupFunction n binders = lookup n binders.functions

export
insertLocalVar : Name -> Int -> LocalVarBinder -> Binders -> Either String Binders
insertLocalVar n i b binders = do
  let Nothing = lookup (n,i) binders.localVars
      | Just _ => Left "LocalVar \{show (n,i)} is already defined."
  Right $ { localVars $= insert (n,i) b } binders

export
lookupLocalVar : Name -> Int -> Binders -> Maybe LocalVarBinder
lookupLocalVar n i binders = lookup (n,i) binders.localVars

export
insertFFIBinder : ExtName -> FFIBinder -> Binders -> Either String Binders
insertFFIBinder e b binders = do
  let Nothing = lookup e binders.ffiBinders
      | Just _ => Left "FFIBinder \{show e} is already defined."
  Right $ { ffiBinders $= insert e b } binders

export
lookupFFIBinder : ExtName -> Binders -> Maybe FFIBinder
lookupFFIBinder e binders = lookup e binders.ffiBinders
