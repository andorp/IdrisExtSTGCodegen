module Idris.Codegen.ExtSTG.Core

import Compiler.ANF
import Core.Context
import Core.Core
import Core.TT
import Data.IOArray
import Data.List
import Data.List
import Data.List.Views
import Data.List1
import Data.SortedMap
import Data.String
import Data.String
import Data.Vect
import Prelude

import Idris.Codegen.ExtSTG.ADTAlias
import Idris.Codegen.ExtSTG.ADTs
import Idris.Codegen.ExtSTG.Configuration
import Idris.Codegen.ExtSTG.Context
import Idris.Codegen.ExtSTG.ExtName
import Idris.Codegen.ExtSTG.STG

%default total

export
unitRet : Core a -> Core ()
unitRet m = do { _ <- m; pure () }

-- TODO: Remove export
public export
stgRepType : RepType
stgRepType = SingleValue LiftedRep

export
mkSrcSpan : FC -> SrcSpan
mkSrcSpan (MkFC file (sl,sc) (el,ec))
  = SsRealSrcSpan (MkRealSrcSpan (show file) (sl + 1) (sc + 1) (el + 1) (ec + 1)) Nothing
mkSrcSpan (MkVirtualFC file (sl,sc) (el,ec))
  = SsRealSrcSpan (MkRealSrcSpan (show file) (sl + 1) (sc + 1) (el + 1) (ec + 1)) Nothing
mkSrcSpan EmptyFC
  = SsUnhelpfulSpan "<no location>"

export
mainBinder : Ref STGCtxt STGContext => Core (SBinder (SingleValue LiftedRep))
mainBinder = do
  u <- mkUnique 'm'
  let scope = HaskellExported
  let bindern = "main"
  let binderId = MkBinderId u
  let typeSig = "mkSBinder: typeSig"
  let details = VanillaId
  let info    = "mkSBinder: IdInfo"
  let fc      = mkSrcSpan emptyFC
  pure $ MkSBinder
    { binderName    = bindern
    , binderId      = binderId
    , binderTypeSig = typeSig
    , binderScope   = scope
    , binderDetails = details
    , binderInfo    = info
    , binderDefLoc  = fc
    }

export
mainArgBinder : Ref STGCtxt STGContext => Core (SBinder (SingleValue VoidRep))
mainArgBinder = do
  u <- mkUnique 'm'
  let scope = GlobalScope
  let bindern = "main"
  let binderId = MkBinderId u
  let typeSig = "mkSBinder: typeSig"
  let details = VanillaId
  let info    = "mkSBinder: IdInfo"
  let fc      = mkSrcSpan emptyFC
  pure $ MkSBinder
    { binderName    = bindern
    , binderId      = binderId
    , binderTypeSig = typeSig
    , binderScope   = scope
    , binderDetails = details
    , binderInfo    = info
    , binderDefLoc  = fc
    }

export
idrisMainEntryBinder : Ref STGCtxt STGContext => Core FunctionBinder
idrisMainEntryBinder = lookupFunctionBinder (MN "__mainExpression" 0)

||| Create a TyConId for the given idris primtive type.
export
tyConIdForPrimType : Ref STGCtxt STGContext => PrimType -> Core TyConId
tyConIdForPrimType = map (Id . typeConSTG) . lookupPrimType

||| Creates a DataConId for the given data constructor name, checks if the name is already have
||| a definition, if not throw an InternalError
export
covering
mkDataConIdStr : Ref Ctxt Defs => Ref STGCtxt STGContext => Core.Name.Name -> Core DataConIdSg
mkDataConIdStr = map (identSg . fst) . lookupDTCon

export
covering
mkTyDataConId
  :  Ref Ctxt Defs
  => Ref STGCtxt STGContext
  => Core.Name.Name
  -> Core DataConIdSg
mkTyDataConId = map (identSg . snd) . lookupTYCon

||| Always creates a fresh binder, its main purpose to create a binder which won't be used, mainly StgCase
export
nonused : Ref STGCtxt STGContext => Core (SBinder (SingleValue LiftedRep))
nonused = mkFreshSBinderStr LocalScope emptyFC "nonused"

export
nonusedRep : Ref STGCtxt STGContext => (rep : RepType) -> Core (SBinder rep)
nonusedRep rep = mkFreshSBinderStr LocalScope emptyFC "nonused"

||| Create binders for STG local variables that are not directly compiled from ANF local variables.
export
localBinderRep : Ref STGCtxt STGContext => FC -> (rep : RepType) -> Core (SBinder rep)
localBinderRep fc rep = mkFreshSBinderStr LocalScope fc "local"

export
topLevel : {r : RepType} -> SBinder (SingleValue LiftedRep) -> List SBinderSg -> Expr r -> TopBinding
topLevel n as body
  = StgTopLifted
  $ StgNonRec n
  $ StgRhsClosure ReEntrant as body

||| Generate External Top Ids for an STG module.
export
genExtTopIds
  :  Ref STGCtxt STGContext
  => Core (List (UnitId, List (ModuleName, List SBinderSg)))
genExtTopIds = do
  map ( groupExternalTopIds
      . map
            (\(MkExtName pck mdl fn, binder) =>
              (MkUnitId pck, MkModuleName (concat (intersperse "." mdl)), binder))
        )
      $ getExtBinds
  where
    groupExternalTopIds
      :  List (UnitId, ModuleName, SBinderSg)
      -> List (UnitId, List (ModuleName, List SBinderSg))
    groupExternalTopIds = resultList . unionsMap . map singletonMap
      where
        EntryMap : Type
        EntryMap = SortedMap String (SortedMap String (List SBinderSg))

        resultList : EntryMap -> List (UnitId, List (ModuleName, List SBinderSg))
        resultList
          = map (bimap MkUnitId (map (mapFst MkModuleName) . toList))
          . toList

        unionsMap : List EntryMap -> EntryMap
        unionsMap = foldl (mergeWith (mergeWith (++))) empty

        singletonMap : (UnitId, ModuleName, SBinderSg) -> EntryMap
        singletonMap (MkUnitId n, MkModuleName m, sbinder) = singleton n (singleton m [sbinder])  

||| Create an StgCase which will represent and force the result for IO external function.
|||
||| Use this function when the external haskell function needs an IO computation.
export
createExtSTGIOApp
  :  Ref STGCtxt STGContext
  => ExtName -> List ArgSg
  -> Core (Expr Core.stgRepType)
createExtSTGIOApp ext originalArgs = do
  extCallResult <- mkFreshSBinderStr {rep=UnboxedTuple [LiftedRep]} LocalScope emptyFC "extCallIOResult"
  extCallResult2 <- mkFreshSBinderStr LocalScope emptyFC "extCallIOResultForce"
  extNameBinderId <- map binderId $ extNameLR ext
  (UnboxedTupleCon 1 ** dataConId) <- map identSg $ lookupExtNameDTCon soloExtName
    | (rep ** _) => coreFail $ InternalError "Unexpected rep type: \{show rep}"
  let args : List ArgSg := originalArgs ++ [ mkArgSg $ StgVarArg $ binderId !realWorldHashBinder ] 
  pure
    $ StgCase
        (MultiValAlt 1) -- IO
        (StgApp extNameBinderId args (UnboxedTuple [LiftedRep]))
        extCallResult
        [ MkAlt (AltUnboxedOneTuple dataConId) extCallResult2
          $ (StgApp (getBinderId extCallResult2) [] (SingleValue LiftedRep))
        ]

||| Create an StgCase which will represent a pure function call on the Haskell side.
|||
||| Use this function when the external does not represent an IO call.
export
createExtSTGPureApp
  :  Ref STGCtxt STGContext
  => ExtName -> List ArgSg
  -> Core (Expr Core.stgRepType)
createExtSTGPureApp ext args = do
  extCallResult <- mkFreshSBinderStr LocalScope emptyFC "extCallPureResult"
  extNameBinderId <- map binderId $ extNameLR ext
  pure
    $ StgCase
        PolyAlt
        (StgApp extNameBinderId args (SingleValue LiftedRep))
        extCallResult
        [ MkAlt AltDefault ()
          $ StgApp (getBinderId extCallResult) [] (SingleValue LiftedRep)
        ]
