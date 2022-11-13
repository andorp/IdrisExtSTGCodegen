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
import Data.String
import Data.String
import Data.Vect
import Libraries.Data.IntMap
import Libraries.Data.StringMap
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

-- export
-- getSTGCtxt : (Ref STGCtxt STGContext) => (STGContext -> a) -> Core a
-- getSTGCtxt g = map g $ get STGCtxt

-- namespace Uniques
--   -- TODO: Rename this
--   export
--   uniqueForType2
--     :  Ref STGCtxt STGContext
--     => Core.Name.Name
--     -> Core Unique
--   uniqueForType2 cname = do
--     ?todo1
--     -- u <- lookupIdrisTypeNamespace cname
--     -- case u of
--     --   Nothing => do
--     --     v <- mkUnique 'y'
--     --     t <- mkUnique 't'
--     --     insertIdrisTypeNamespace cname v t
--     --     pure v
--     --   Just v =>
--     --     pure v.tyUnique

--   export
--   uniqueForTerm2
--     :  Ref STGCtxt STGContext
--     => Core.Name.Name
--     -> Core Unique
--   uniqueForTerm2 cname = do
--     ?todo2 --
--     -- u <- lookupIdrisTermNamespace cname
--     -- case u of
--     --   Nothing => do
--     --     v <- mkUnique 'e'
--     --     insertIdrisTermNamespace cname v
--     --     pure v
--     --   Just v =>
--     --     pure v

--   export
--   uniqueForHaskellType
--     :  Ref STGCtxt STGContext
--     => ExtName
--     -> Core Unique
--   uniqueForHaskellType ext = do
--     ?todo3
--     -- u <- ?todo3 -- lookupHaskellTypeNamespace ext
--     -- case u of
--     --   Nothing => do
--     --     v <- mkUnique 'k'
--     --     insertHaskellTypeNamespace ext v
--     --     pure v
--     --   Just v =>
--     --     pure v

--   export
--   uniqueForHaskellTerm
--     :  Ref STGCtxt STGContext
--     => ExtName
--     -> Core Unique
--   uniqueForHaskellTerm ext = do
--     ?todo4
--     -- u <- ?todo4 -- lookupHaskellTermNamespace ext
--     -- case u of
--     --   Nothing => do
--     --     v <- mkUnique 'h'
--     --     insertHaskellTermNamespace ext v
--     --     pure v
--     --   Just v =>
--     --     pure v

--   export
--   uniqueForIdrisTypeDataCon
--     :  Ref STGCtxt STGContext
--     => Core.Name.Name
--     -> Core Unique
--   uniqueForIdrisTypeDataCon n = do
--     ?todo5
--     -- Nothing <- ?todo5 -- lookupIdrisTypeDataCon n
--     --   | Just u => pure u
--     -- uv <- mkUnique 'y'
--     -- ut <- mkUnique 't'
--     -- insertIdrisTypeNamespace n uv ut
--     -- pure ut

-- public export
-- HardcodedUnique : Type
-- HardcodedUnique = (String, List String, String, Unique, RepType)

-- public export
-- hardcodedRepType : HardcodedUnique -> RepType
-- hardcodedRepType (_,_,_,_,r) = r

-- ||| void# in STG
-- public export
-- hardcodedVoidHash : HardcodedUnique
-- hardcodedVoidHash = ("ghc-prim", ["GHC", "Prim"], "void#", MkUnique '0' 21, SingleValue VoidRep)

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

-- public export
-- data BinderKind = Trm | Typ

-- public export
-- data BinderName
--   = HsName  ExtName
--   | IdrName Core.Name.Name
--   | IdrLocal Core.Name.Name (Maybe Int) -- TODO: Nat

-- localVarName : Core.Name.Name -> Int -> Core.Name.Name
-- localVarName = PV

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
  -- u <- ?todo -- uniqueForTerm2 (MN "__mainExpression" 0)
  -- let scope = HaskellExported
  -- let bindern = "main"
  -- let binderId = MkBinderId u
  -- let typeSig = "mkSBinder: typeSig"
  -- let details = VanillaId
  -- let info    = "mkSBinder: IdInfo"
  -- let fc      = mkSrcSpan emptyFC
  -- pure $ MkSBinder
  --   { binderName    = bindern
  --   , binderId      = binderId
  --   , binderTypeSig = typeSig
  --   , binderScope   = scope
  --   , binderDetails = details
  --   , binderInfo    = info
  --   , binderDefLoc  = fc
  --   }

-- export
-- mkSBinder
--   :  Ref STGCtxt STGContext
--   => {rep : RepType} -> SrcSpan -> BinderKind -> BinderName
--   -> Core (SBinder rep)
-- mkSBinder fc binderKind binderName = do
--   u <- case (binderKind, binderName) of
--         (Trm, HsName   n)   => uniqueForHaskellTerm n
--         (Trm, IdrName  n)   => uniqueForTerm2 n
--         (Trm, IdrLocal n x) => uniqueForTerm2 $ maybe n (localVarName n) x
--         (Typ, HsName   n)   => uniqueForHaskellType n
--         (Typ, IdrName  n)   => uniqueForType2 n
--         (Typ, IdrLocal n x) => uniqueForType2 $ maybe n (localVarName n) x
--   let scope = case binderName of
--                 HsName   _   => HaskellExported
--                 IdrName  _   => GlobalScope
--                 IdrLocal _ _ => LocalScope
--   let bindern = case binderName of
--                   HsName   n   => extNameFunction n
--                   IdrName  n   => binderStr n
--                   IdrLocal n x => binderStr $ maybe n (localVarName n) x
--   let binderId = MkBinderId u
--   let typeSig = "mkSBinder: typeSig"
--   let details = VanillaId
--   let info    = "mkSBinder: IdInfo"
--   pure $ MkSBinder
--     { binderName    = bindern
--     , binderId      = binderId
--     , binderTypeSig = typeSig
--     , binderScope   = scope
--     , binderDetails = details
--     , binderInfo    = info
--     , binderDefLoc  = fc
--     }

||| Always return a new binder for the given name adding the counter at the end of the name.
||| Used in defining local variables.
-- export

-- export
-- mkSBinderHardcoded
--   :  Ref STGCtxt STGContext
--   => (h : HardcodedUnique)
--   -> FC -> Core (SBinder (hardcodedRepType h))
-- mkSBinderHardcoded (_,_,binderName,unique,repType) fc = do
--   let binderId = MkBinderId unique
--   let typeSig = "mkSBinder: hardcodedSig"
--   let details = VanillaId
--   let info = "mkSBinder: IdInfo"
--   let defLoc = mkSrcSpan fc
--   pure $ MkSBinder
--     { binderName    = binderName
--     , binderId      = binderId
--     , binderTypeSig = typeSig
--     , binderScope   = HaskellExported
--     , binderDetails = details
--     , binderInfo    = info
--     , binderDefLoc  = defLoc
--     }

-- ||| Return the type and datacon names and paths
-- export
-- runtimeRepresentationOf : PrimType -> Core (ExtName, ExtName, List PrimRep)
-- runtimeRepresentationOf IntType = pure
--   ( MkExtName "ghc-prim" ["GHC", "Types"] "Int"
--   , MkExtName "ghc-prim" ["GHC", "Types"] "I#", [IntRep])
-- runtimeRepresentationOf IntegerType = pure
--   ( MkExtName "main" ["Idris", "Runtime", "Integer"] "BI"
--   , MkExtName "main" ["Idris", "Runtime", "Integer"] "BI", [LiftedRep])
-- runtimeRepresentationOf Int8Type = pure
--   ( MkExtName "base" ["GHC", "Int"] "Int8"
--   , MkExtName "base" ["GHC", "Int"] "I8#", [Int8Rep])
-- runtimeRepresentationOf Int16Type = pure
--   ( MkExtName "base" ["GHC", "Int"] "Int16"
--   , MkExtName "base" ["GHC", "Int"] "I16#", [Int16Rep])
-- runtimeRepresentationOf Int32Type = pure
--   ( MkExtName "base" ["GHC", "Int"] "Int32"
--   , MkExtName "base" ["GHC", "Int"] "I32#", [Int32Rep])
-- runtimeRepresentationOf Int64Type = pure
--   ( MkExtName "base" ["GHC", "Int"] "Int64"
--   , MkExtName "base" ["GHC", "Int"] "I64#", [Int64Rep])
-- runtimeRepresentationOf Bits8Type = pure
--   ( MkExtName "base" ["GHC", "Word"] "Word8"
--   , MkExtName "base" ["GHC", "Word"] "W8#", [Word8Rep])
-- runtimeRepresentationOf Bits16Type = pure
--   ( MkExtName "base" ["GHC", "Word"] "Word16"
--   , MkExtName "base" ["GHC", "Word"] "W16#", [Word16Rep])
-- runtimeRepresentationOf Bits32Type = pure
--   ( MkExtName "base" ["GHC", "Word"] "Word32"
--   , MkExtName "base" ["GHC", "Word"] "W32#", [Word32Rep])
-- runtimeRepresentationOf Bits64Type = pure
--   ( MkExtName "base" ["GHC", "Word"] "Word64"
--   , MkExtName "base" ["GHC", "Word"] "W64#", [Word64Rep])
-- runtimeRepresentationOf CharType = pure
--   ( MkExtName "ghc-prim" ["GHC", "Types"] "Char"
--   , MkExtName "ghc-prim" ["GHC", "Types"] "C#", [CharRep])
-- runtimeRepresentationOf DoubleType = pure
--   ( MkExtName "ghc-prim" ["GHC", "Types"] "Double"
--   , MkExtName "ghc-prim" ["GHC", "Types"] "D#", [DoubleRep])
-- runtimeRepresentationOf WorldType = pure
--   ( MkExtName "main" ["Idris", "Runtime", "World"] "World"
--   , MkExtName "main" ["Idris", "Runtime", "World"] "World", [])
-- runtimeRepresentationOf other
--   = coreFail $ UserError $ "No type and data constructor for " ++ show other


||| Create a TyConId for the given idris primtive type.
export
tyConIdForPrimType : Ref STGCtxt STGContext => PrimType -> Core TyConId
tyConIdForPrimType = map (Id . typeConSTG) . lookupPrimType
  -- (e, _) <- runtimeRepresentationOf c
  -- MkTyConId <$> uniqueForHaskellType e

-- namespace DataTypes

  -- export
  -- checkDefinedSTyCon
  --   :  Ref STGCtxt STGContext
  --   => TyConId
  --   -> Core (Maybe STyCon)
  -- checkDefinedSTyCon (MkTyConId u) = getTyConId u

  -- dataTypeList : DataTypeMap -> List (UnitId, List (ModuleName, List STyCon))
  -- dataTypeList = map (mapFst MkUnitId) . Data.StringMap.toList . map (map (mapFst MkModuleName) . Data.StringMap.toList)

  -- export
  -- createSTyCon
  --   :  Ref STGCtxt STGContext
  --   => (Core.Name.Name, SrcSpan) -> List (Core.Name.Name, DataConRep, SrcSpan)
  --   -> Core STyCon
  -- createSTyCon (tName,tSpan) dCons = do
  --   ds <- traverse (\(dName, drep, span) => pure $ mkSDataConSg $
  --                     mkSDataCon
  --                       drep
  --                       (binderStr dName)
  --                       (MkDataConId !(uniqueForTerm2 dName))
  --                       !(mkSBinder span Trm (IdrName dName))
  --                       span
  --                  )
  --                  dCons
  --   pure $ MkSTyCon (binderStr tName) (MkTyConId !(uniqueForType2 tName)) ds tSpan

  -- export
  -- createSTyConExt
  --   :  Ref STGCtxt STGContext
  --   => (ExtName, SrcSpan) -> List (ExtName, DataConRep, SrcSpan)
  --   -> Core STyCon
  -- createSTyConExt (tExtName,tSpan) dCons = do
  --   ds <- traverse (\(dExtName, drep, span) => do
  --                     pure
  --                       $ mkSDataConSg
  --                       $ mkSDataCon
  --                           drep
  --                           (extName dExtName)
  --                           (MkDataConId !(uniqueForHaskellTerm dExtName))
  --                           !(mkSBinder span Trm (HsName dExtName))
  --                           span
  --                  )
  --                  dCons
  --   pure $ MkSTyCon (extName tExtName) (MkTyConId !(uniqueForHaskellType tExtName)) ds tSpan
  --   where
  --     renderFullName : ExtName -> String
  --     renderFullName (MkExtName u p n) = u ++ ":" ++ concat (intersperse "." p) ++ "." ++ n

  --     extName : ExtName -> STG.Name
  --     extName (MkExtName _ _ n) = n

  -- ||| Register an STG datatype under the compilation unit and module name.
  -- export
  -- defineDataType
  --   :  Ref STGCtxt STGContext
  --   => UnitId -> ModuleName -> STyCon
  --   -> Core ()
  -- defineDataType u m s = ?todo -- addDataType u m s

  -- ||| Return all the STG data type definition that were registered during the compilation
  -- export
  -- getDefinedDataTypes
  --   :  Ref STGCtxt STGContext
  --   => Core (List (UnitId, List (ModuleName, List STyCon)))
  -- getDefinedDataTypes = map dataTypeList getDataTypes

||| Creates a DataConId for the given data constructor name, checks if the name is already have
||| a definition, if not throw an InternalError
export
covering
mkDataConIdStr : Ref Ctxt Defs => Ref STGCtxt STGContext => Core.Name.Name -> Core DataConIdSg
mkDataConIdStr = map (identSg . fst) . lookupDTCon

-- export
-- mkDataConIdExtName : Ref STGCtxt STGContext => ExtName -> Core DataConIdSg
-- mkDataConIdExtName ext = do
--   (r ** d) <- getUniqueDataCon !(uniqueForHaskellTerm ext)
--   pure (r ** ident d)

-- ||| Creates a DataConId for the given data constructor name, checks if the name is already have
-- ||| a definition, if not throw an InternalError
-- -- export
-- covering
-- mkDataConId
--   :  Ref Ctxt Defs
--   => Ref STGCtxt STGContext
--   => Core.Name.Name -- Name of the fully qualified data constructor (not an Idris primitive type)
--   -> Core DataConIdSg
-- mkDataConId = mkDataConIdStr

export
covering
mkTyDataConId
  :  Ref Ctxt Defs
  => Ref STGCtxt STGContext
  => Core.Name.Name
  -> Core DataConIdSg
mkTyDataConId = map (identSg . snd) . lookupTYCon
  -- pure $ identSg d
    -- Just u <- lookupIdrisTypeDataCon n
    --   | Nothing => coreFail $ InternalError "Type Data Con is not found for \{show n}"
    -- d <- lookupIdrisTypeDataConDef u
    -- pure $ identSg d

-- ||| Determine the Data constructor for the boxed primitive type.
-- |||
-- ||| The name of terms should coincide the ones that are defined in GHC's ecosystem. This
-- ||| would make the transition easier, I hope.
-- export
-- dataConIdRepForPrimType
--   :  Ref STGCtxt STGContext
--   => (r : PrimRep)
--   -> PrimType
--   -> Core (DataConId (AlgDataCon [r]))
-- dataConIdRepForPrimType r c = do
--   ((AlgDataCon [q]) ** d) <- dataConIdForPrimType c
--     | other => coreFail $ InternalError
--                         $ unwords
--                           [ "dataConIdRepForPrimType:"
--                           , show c, "got an unexpectly shaped binder:"
--                           , show other, "expected:" -- , show (AlgDataCon [r])
--                           ]
--   let Just Refl = semiDecEq r q
--     | Nothing => coreFail $ InternalError
--                           $ unwords
--                             [ "dataConIdRepForPrimType:"
--                             , show c, "doesn't have the expected"
--                             , show r, "found"
--                             , show q
--                             ]
--   pure d

||| Always creates a fresh binder, its main purpose to create a binder which won't be used, mainly StgCase
export
nonused : Ref STGCtxt STGContext => Core (SBinder (SingleValue LiftedRep))
nonused = mkFreshSBinderStr LocalScope emptyFC "nonused"

export
nonusedRep : Ref STGCtxt STGContext => (rep : RepType) -> Core (SBinder rep)
nonusedRep rep = mkFreshSBinderStr LocalScope emptyFC "nonused"

-- ||| Create binders for STG local variables that are not directly compiled from ANF local variables.
-- -- export
-- localBinder : Ref STGCtxt STGContext => FC -> Core (SBinder (SingleValue LiftedRep))
-- localBinder fc = mkFreshSBinderStr LocalScope fc "local"

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

-- ||| Create a case expression with one Alt which matches the one data constructor
-- -- export
-- unBox
--   :  (v1  : SBinder (SingleValue LiftedRep))
--   -> {q   : DataConRep}
--   -> (d1  : DataConId q)
--   -> (t1  : TyConId)
--   -> (cb  : SBinder (SingleValue LiftedRep))
--   -> (v2  : (AltBinderType (AltDataCon (q ** d1))))
--   -> (e   : Expr Core.stgRepType) -- TODO: Fix
--   -> Expr Core.stgRepType -- TODO: Fix
-- unBox v1 d1 t1 cb v2 e =
--   StgCase (AlgAlt t1) (StgApp (binderId v1) [] (SingleValue LiftedRep)) cb
--   [ MkAlt (AltDataCon (q ** d1)) v2 e ]

-- ||| Create an STGCase which represents an boxing of the given SingleValue representation.
-- -- export
-- box
--   :  {rep : PrimRep}
--   -> DataConId (AlgDataCon [rep]) -> SBinder (SingleValue rep) -> Expr (SingleValue rep) 
--   -> Expr (SingleValue LiftedRep)
-- box dataCon varToBind primOpExp
--   = StgCase (PrimAlt rep) primOpExp varToBind
--       [ MkAlt AltDefault () 
--         $ StgConApp dataCon (StgVarArg (binderId varToBind))
--       ]

-- export
-- checkSemiDecEq
--   :  Show a
--   => SemiDecEq a
--   => String -> (exp : a) -> (fnd : a)
--   -> Core (exp = fnd)
-- checkSemiDecEq ctx exp fnd = case (semiDecEq exp fnd) of
--   Nothing   => coreFail $ InternalError $ "\{ctx} has different values. Expected \{show exp} , but found: \{show fnd}"
--   Just Refl => pure Refl

-- export
-- checkDataCon : String -> (r : DataConRep) -> DataConIdSg -> Core (DataConId r)
-- checkDataCon loc expRep c@(foundRep ** d) = do
--   Refl <- checkSemiDecEq loc expRep foundRep
--   pure d

-- renderName : ExtName -> String
-- renderName (MkExtName pkg mdl fn) = pkg ++ "_" ++ concat (intersperse "." mdl) ++ "." ++ fn

-- ||| Ask for a BinderId for the given name, if there is, if not create a Binder and
-- ||| register in the ExtBindMap
-- export
-- extName
--   :  Ref STGCtxt STGContext
--   => ExtName
--   -> Core BinderIdSg
-- extName e@(MkExtName pkg mdl fn) = do
--   ?todo10
--   -- extBind <- lookupExtBinds e
--   -- case extBind of
--   --   Nothing => do
--   --     binder <- map mkSBinderSg $ mkSBinder {rep=SingleValue LiftedRep} (mkSrcSpan emptyFC) Trm (HsName e)
--   --     insertExtBinds e binder
--   --     pure $ getSBinderIdSg binder
--   --   Just b => pure $ getSBinderIdSg b

-- export
-- registerHardcodedExtTopIds
--   :  Ref STGCtxt STGContext
--   => Core ()
-- registerHardcodedExtTopIds = do
--   binder <- mkSBinderHardcoded hardcodedVoidHash emptyFC
--   let (unt,mod,fn,_,_) = hardcodedVoidHash
--   let e = MkExtName unt mod fn
--   -- insertExtNameBinder e binder
--   ?todo
--   pure ()

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
        EntryMap = StringMap (StringMap (List SBinderSg))

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

-- export
-- mkBinderIdVar
--   :  Ref STGCtxt STGContext
--   => FC -> Core.Name.Name -> AVar
--   -> Core (BinderId Core.stgRepType)
-- mkBinderIdVar fc n (ALocal x) = MkBinderId <$> uniqueForTerm2 (localVarName n x)
-- mkBinderIdVar fc n ANull      = extNameLR erasedExtName

-- export
-- mkBinderIdVarRep
--   :  Ref STGCtxt STGContext
--   => {rep : RepType} -> FC -> Core.Name.Name -> AVar
--   -> Core (BinderId rep)
-- mkBinderIdVarRep fc n (ALocal x) = MkBinderId <$> uniqueForTerm2 (localVarName n x)
-- mkBinderIdVarRep fc n ANull      = coreFail $ InternalError "mkBinderIdVarRep got Null"
