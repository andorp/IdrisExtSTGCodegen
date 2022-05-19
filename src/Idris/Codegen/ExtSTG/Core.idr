module Idris.Codegen.ExtSTG.Core

import Compiler.ANF
import Core.Context
import Core.Core
import Core.TT
import Data.IOArray
import Libraries.Data.IntMap
import Data.List
import Libraries.Data.StringMap
import Data.String
import Data.Vect
import Idris.Codegen.ExtSTG.STG
import Prelude
import Idris.Codegen.ExtSTG.Context


export
modifySTGCtxt : (Ref STGCtxt STGContext) => (STGContext -> STGContext) -> Core ()
modifySTGCtxt f = do
  ctx <- get STGCtxt
  put STGCtxt (f ctx)

export
getSTGCtxt : (Ref STGCtxt STGContext) => (STGContext -> a) -> Core a
getSTGCtxt g = map g $ get STGCtxt

namespace Uniques

  export
  mkUnique
    :  (Ref STGCtxt STGContext)
    => Char
    -> Core Unique
  mkUnique c = do
    x <- incCounter
    let u = MkUnique c x
    pure u

  export
  uniqueForType
    :  Ref STGCtxt STGContext
    => String
    -> Core Unique
  uniqueForType name = do
    u <- lookupTypeNamespace name
    case u of
      Nothing => do
        v <- mkUnique 'y'
        insertTypeNamespace name v
        pure v
      Just u => do
        pure u

  export
  uniqueForTerm
    :  Ref STGCtxt STGContext
    => String
    -> Core Unique
  uniqueForTerm name = do
    u <- lookupTermNamespace name
    case u of
      Nothing => do
        v <- mkUnique 'e'
        insertTermNamespace name v
        pure v
      Just u => do
        pure u

  public export
  HardcodedUnique : Type
  HardcodedUnique = (String, List String, String, Unique, RepType)

  public export
  hardcodedRepType : HardcodedUnique -> RepType
  hardcodedRepType (_,_,_,_,r) = r

  ||| void# in STG
  public export
  hardcodedVoidHash : HardcodedUnique
  hardcodedVoidHash = ("ghc-prim", ["GHC", "Prim"], "void#", MkUnique '0' 21, SingleValue VoidRep)

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

public export
data BinderKind
  = TermBinder
  | TypeBinder

export
mkSBinderHardcoded
  :  Ref STGCtxt STGContext
  => (h : HardcodedUnique)
  -> FC -> Core (SBinder (hardcodedRepType h))
mkSBinderHardcoded (_,_,binderName,unique,repType) fc = do
  let binderId = MkBinderId unique
  let typeSig = "mkSBinder: hardcodedSig"
  let details = VanillaId
  let info = "mkSBinder: IdInfo"
  let defLoc = mkSrcSpan fc
  pure $ MkSBinder
    { binderName    = binderName
    , binderId      = binderId
    , binderTypeSig = typeSig
    , binderScope   = HaskellExported
    , binderDetails = details
    , binderInfo    = info
    , binderDefLoc  = defLoc
    }

-- TODO: Remove export
-- TODO: Remove/replace topLevel parameter
export
mkSBinder
  :  Ref STGCtxt STGContext
  => BinderKind -> Scope -> FC -> String
  -> Core (SBinder Core.stgRepType)
mkSBinder binderKind scope fc binderName = do
  binderId <- MkBinderId <$> case binderKind of
                               TermBinder => uniqueForTerm binderName
                               TypeBinder => uniqueForType binderName
  let typeSig = "mkSBinder: typeSig"
  let details = VanillaId
  let info    = "mkSBinder: IdInfo"
  let defLoc  = mkSrcSpan fc
  pure $ MkSBinder
    { binderName    = binderName
    , binderId      = binderId
    , binderTypeSig = typeSig
    , binderScope   = scope
    , binderDetails = details
    , binderInfo    = info
    , binderDefLoc  = defLoc
    }

-- TODO: Remove export
-- TODO: Remove/replace topLevel parameter
export
mkSBinderRep
  :  Ref STGCtxt STGContext
  => BinderKind -> Scope -> (rep : RepType) -> FC -> String
  -> Core (SBinder rep)
mkSBinderRep binderKind scope rep fc binderName = do
  binderId <- MkBinderId <$> case binderKind of
                               TermBinder => uniqueForTerm binderName
                               TypeBinder => uniqueForType binderName
  let typeSig = "mkSBinder: typeSig"
  let details = VanillaId
  let info    = "mkSBinder: IdInfo"
  let defLoc  = mkSrcSpan fc
  pure $ MkSBinder
    { binderName    = binderName
    , binderId      = binderId
    , binderTypeSig = typeSig
    , binderScope   = scope
    , binderDetails = details
    , binderInfo    = info
    , binderDefLoc  = defLoc
    }

-- export
mkSBinderTyCon
  :  Ref STGCtxt STGContext
  => FC -> String -- TODO : Constant
  -> Core (SBinder Core.stgRepType)
mkSBinderTyCon = mkSBinder TypeBinder GlobalScope

||| Create a top-level binder for a given name. Mainly, used in STG.String module
export
mkSBinderTopLevel
  :  Ref STGCtxt STGContext
  => String
  -> Core (SBinder Core.stgRepType)
mkSBinderTopLevel = mkSBinder TermBinder GlobalScope emptyFC

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

binderStr : Core.Name.Name -> String
binderStr (NS ns n@(UN (Field _))) = show ns ++ ".(" ++ binderStr n ++ ")"
binderStr (NS ns n) = show ns ++ "." ++ binderStr n
binderStr (UN x) = show x
binderStr (MN x y) = "{" ++ x ++ ":" ++ show y ++ "}"
binderStr (PV n d) = "{P:" ++ binderStr n ++ ":" ++ show d ++ "}"
binderStr (DN str n) = str ++ "*" ++ binderStr n
binderStr (Nested (outer, idx) inner) = show outer ++ ":" ++ show idx ++ ":" ++ binderStr inner
binderStr (CaseBlock outer i) = "case:block:in:" ++ outer ++ "*" ++ show i
binderStr (WithBlock outer i) = "with:block:in:" ++ outer ++ "*" ++ show i
binderStr (Resolved x) = "$resolved" ++ show x

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

export
mkSBinderStr
  :  Ref STGCtxt STGContext
  => FC -> String
  -> Core (SBinder Core.stgRepType)
mkSBinderStr = mkSBinder TermBinder GlobalScope

||| Create a binder for a function that is defined in another STG module.
||| Primary use case for this is the STG-FFI, or exported from the module.
export
mkSBinderExtId
  :  Ref STGCtxt STGContext
  => FC -> String
  -> Core (SBinder Core.stgRepType)
mkSBinderExtId = mkSBinder TermBinder HaskellExported

||| Always return a new binder for the given name adding the counter at the end of the name.
||| Used in defining local variables.
export
mkFreshSBinderStr
  :  Ref STGCtxt STGContext
  => Scope -> FC -> String
  -> Core (SBinder Core.stgRepType)
mkFreshSBinderStr scope fc binderName = do
  unique@(MkUnique _ c) <- mkUnique 'l'
  binderId <- MkBinderId <$> mkUnique 'l'
  let typeSig = "mkSBinder: typeSig"
  let details = VanillaId
  let info    = "mkSBinder: IdInfo"
  let defLoc  = mkSrcSpan fc
  pure $ MkSBinder
    { binderName    = (binderName ++ ":" ++ show c)
    , binderId      = binderId
    , binderTypeSig = typeSig
    , binderScope   = scope
    , binderDetails = details
    , binderInfo    = info
    , binderDefLoc  = defLoc
    }

||| Always return a new binder for the given name adding the counter at the end of the name.
||| Used in defining local variables.
export
mkFreshSBinderRepStr
  :  Ref STGCtxt STGContext
  => Scope -> (rep : RepType) -> FC -> String
  -> Core (SBinder rep)
mkFreshSBinderRepStr scope rep fc binderName = do
  unique@(MkUnique _ c) <- mkUnique 'l'
  binderId <- MkBinderId <$> mkUnique 'l'
  let typeSig = "mkSBinder: typeSig"
  let details = VanillaId
  let info    = "mkSBinder: IdInfo"
  let defLoc  = mkSrcSpan fc
  pure $ MkSBinder
    { binderName    = (binderName ++ ":" ++ show c)
    , binderId      = binderId
    , binderTypeSig = typeSig
    , binderScope   = scope
    , binderDetails = details
    , binderInfo    = info
    , binderDefLoc  = defLoc
    }

export
mkSBinderVar
  :  Ref STGCtxt STGContext
  => FC -> Core.Name.Name -> AVar
  -> Core (SBinder Core.stgRepType)
mkSBinderVar fc n (ALocal x) = mkSBinder TermBinder LocalScope fc (binderStr n ++ ":" ++ show x)
mkSBinderVar fc n ANull      = coreFail $ InternalError $ "mkSBinderVar " ++ show fc ++ " " ++ binderStr n ++ " ANull"

export
mkBinderIdVar
  :  Ref STGCtxt STGContext
  => FC -> Core.Name.Name -> (r : RepType) -> AVar
  -> Core (BinderId r)
mkBinderIdVar fc n r (ALocal x) = MkBinderId <$> uniqueForTerm (binderStr n ++ ":" ++ show x)
mkBinderIdVar fc n r ANull      = coreFail $ InternalError $ "mkBinderIdVar " ++ show fc ++ " " ++ binderStr n ++ " ANull"

||| Create a StdVarArg for the Argument of a function application.
|||
||| If the argument is ANull/erased, then it returns a NulAddr literal
export
mkStgArg
  :  Ref STGCtxt STGContext
  => FC -> Core.Name.Name -> AVar
  -> Core ArgSg
mkStgArg fc n a@(ALocal _) = mkArgSg . StgVarArg <$> (mkBinderIdVar fc n stgRepType a)
mkStgArg _  _ ANull        = pure $ mkArgSg $ StgLitArg $ LitNullAddr
-- Question: Is that a right value for erased argument?
-- Answer: This is not right, this should be Lifted. Make a global erased value, with its binder
--         that is referred here.

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

||| The unit where the Idris STG backend puts every definitions,
||| primitives and used defined codes
export
MAIN_UNIT : String
MAIN_UNIT = "main"

||| The module name where Idris STG backend puts every definitions,
||| primitives and user defined codes
export
MAIN_MODULE : String
MAIN_MODULE = "Main"

export
dataConNameForPrimType
  :  Ref STGCtxt STGContext
  => PrimType
  -> Core String
dataConNameForPrimType IntType     = pure "I#"
dataConNameForPrimType IntegerType = pure "GMPInt" -- TODO: This should be GMP int
dataConNameForPrimType Bits8Type   = pure "W8#"
dataConNameForPrimType Bits16Type  = pure "W16#"
dataConNameForPrimType Bits32Type  = pure "W32#"
dataConNameForPrimType Bits64Type  = pure "W64#"
-- dataConNameForPrimType StringType  = pure "IdrString" -- TODO: Figure this out.
dataConNameForPrimType CharType    = pure "C#"
dataConNameForPrimType DoubleType  = pure "D#"
dataConNameForPrimType WorldType   = pure "World"
dataConNameForPrimType other = coreFail $ UserError $ "No data constructor for " ++ show other

export
typeConNameForPrimType
  :  Ref STGCtxt STGContext
  => PrimType
  -> Core (String, String, String)
typeConNameForPrimType IntType     = pure ("ghc-prim", "GHC.Types", "Int")
typeConNameForPrimType IntegerType = pure (MAIN_UNIT, MAIN_MODULE, "GMPInt") -- TODO: This should be GMP int
typeConNameForPrimType Bits8Type   = pure ("base", "GHC.Word", "Word8")
typeConNameForPrimType Bits16Type  = pure ("base", "GHC.Word", "Word16")
typeConNameForPrimType Bits32Type  = pure ("base", "GHC.Word", "Word32")
typeConNameForPrimType Bits64Type  = pure ("base", "GHC.Word", "Word64")
-- typeConNameForPrimType StringType  = pure "IdrString" -- TODO: Figure this out.
typeConNameForPrimType CharType    = pure ("ghc-prim", "GHC.Types", "Char")
typeConNameForPrimType DoubleType  = pure ("ghc-prim", "GHC.Types", "Double")
typeConNameForPrimType WorldType   = pure ("main", "Idris.Runtime.World", "World")
typeConNameForPrimType other = coreFail $ UserError $ "No type constructor for " ++ show other

||| Create a TyConId for the given idris primtive type.
export
tyConIdForPrimType
  :  Ref STGCtxt STGContext
  => PrimType
  -> Core TyConId
tyConIdForPrimType c = do
  (_,_,t) <- typeConNameForPrimType c
  MkTyConId <$> uniqueForType t

namespace DataTypes

  export
  checkDefinedDataCon
    :  Ref STGCtxt STGContext
    => Unique
    -> Core (Maybe SDataConSg)
  checkDefinedDataCon u = do
    dcs <- getDataCons u
    case dcs of
      Nothing  => pure Nothing
      Just [d] => pure $ Just d
      Just ds  => coreFail $ InternalError $ "Non unique datatype for DataCon: " ++ show (u, ds)

  export
  checkDefinedSTyCon
    :  Ref STGCtxt STGContext
    => TyConId
    -> Core (Maybe STyCon)
  checkDefinedSTyCon (MkTyConId u) = do
    tcs <- getTyConIds u
    case tcs of
      Nothing  => pure Nothing
      Just [t] => pure $ Just t
      Just ts  => coreFail $ InternalError $ "Non unqiue typecon for TyCon:" ++ show (u,ts)

  dataTypeList : DataTypeMap -> List (UnitId, List (ModuleName, List STyCon))
  dataTypeList = map (mapFst MkUnitId) . Data.StringMap.toList . map (map (mapFst MkModuleName) . Data.StringMap.toList)

  export
  createSTyCon
    :  Ref STGCtxt STGContext
    => (STG.Name, SrcSpan) -> List (STG.Name, DataConRep, SrcSpan)
    -> Core STyCon
  createSTyCon (tName,tSpan) dCons = do
    ds <- traverse (\(dName, drep, span) => pure $ mkSDataConSg $
                      mkSDataCon
                        drep
                        dName
                        (MkDataConId !(uniqueForTerm dName))
                        !(mkSBinderTopLevel dName)
                        span
                   )
                   dCons
    pure $ MkSTyCon tName (MkTyConId !(uniqueForType tName)) ds tSpan

  ||| Register an STG datatype under the compilation unit and module name.
  export
  defineDataType
    :  Ref STGCtxt STGContext
    => UnitId -> ModuleName -> STyCon
    -> Core ()
  defineDataType u m s = addDataType u m s

  ||| Return all the STG data type definition that were registered during the compilation
  export
  getDefinedDataTypes
    :  Ref STGCtxt STGContext
    => Core (List (UnitId, List (ModuleName, List STyCon)))
  getDefinedDataTypes = map dataTypeList getDataTypes

||| Creates a DataConId for the given data constructor name, checks if the name is already have
||| a definition, if not throw an InternalError
export
mkDataConIdStr
  :  Ref STGCtxt STGContext
  => String
  -> Core DataConIdSg
mkDataConIdStr n = do
  Just (r ** d) <- checkDefinedDataCon !(uniqueForTerm n)
    | Nothing => coreFail $ InternalError $ "DataCon is not defined: " ++ n
  pure $ (r ** ident d)

||| Creates a DataConId for the given data constructor name, checks if the name is already have
||| a definition, if not throw an InternalError
export
mkDataConId
  :  Ref STGCtxt STGContext
  => Core.Name.Name -- Name of the fully qualified data constructor (not an Idris primitive type)
  -> Core DataConIdSg
mkDataConId n = mkDataConIdStr (binderStr n)

export
mkTyConIdStr
  :  Ref STGCtxt STGContext
  => String
  -> Core TyConId
mkTyConIdStr n = do
  tyConId <- MkTyConId <$> uniqueForType n
  Just _ <- checkDefinedSTyCon tyConId
    | Nothing => coreFail $ InternalError $ "TyCon is not defined: " ++ n
  pure tyConId

||| Determine the Data constructor for the boxed primitive type.
|||
||| The name of terms should coincide the ones that are defined in GHC's ecosystem. This
||| would make the transition easier, I hope.
export
dataConIdForPrimType
  :  Ref STGCtxt STGContext
  => PrimType
  -> Core DataConIdSg
dataConIdForPrimType c = mkDataConIdStr !(dataConNameForPrimType c)

||| Determine the Data constructor for the boxed primitive type.
|||
||| The name of terms should coincide the ones that are defined in GHC's ecosystem. This
||| would make the transition easier, I hope.
export
dataConIdRepForPrimType
  :  Ref STGCtxt STGContext
  => (r : PrimRep)
  -> PrimType
  -> Core (DataConId (AlgDataCon [r]))
dataConIdRepForPrimType r c = do
  ((AlgDataCon [q]) ** d) <- dataConIdForPrimType c
    | other => coreFail $ InternalError
                        $ unwords
                          [ "dataConIdRepForPrimType:"
                          , show c, "got an unexpectly shaped binder:"
                          , show other, "expected:" -- , show (AlgDataCon [r])
                          ]
  let Just Refl = semiDecEq r q
    | Nothing => coreFail $ InternalError
                          $ unwords
                            [ "dataConIdRepForPrimType:"
                            , show c, "doesn't have the expected"
                            , show r, "found"
                            , show q
                            ]
  pure d

||| Always creates a fresh binder, its main purpose to create a binder which won't be used, mainly StgCase
export
nonused : Ref STGCtxt STGContext => Core (SBinder (SingleValue LiftedRep))
nonused = mkFreshSBinderStr LocalScope emptyFC "nonused"

export
nonusedRep : Ref STGCtxt STGContext => (rep : RepType) -> Core (SBinder rep)
nonusedRep rep = mkFreshSBinderRepStr LocalScope rep emptyFC "nonused"

||| Create binders for STG local variables that are not directly compiled from ANF local variables.
export
localBinder : Ref STGCtxt STGContext => FC -> Core (SBinder (SingleValue LiftedRep))
localBinder fc = mkFreshSBinderStr LocalScope fc "local"

||| Create binders for STG local variables that are not directly compiled from ANF local variables.
export
localBinderRep : Ref STGCtxt STGContext => FC -> (rep : RepType) -> Core (SBinder rep)
localBinderRep fc rep = mkFreshSBinderRepStr LocalScope rep fc "local"

export
topLevel : {r : RepType} -> SBinder (SingleValue LiftedRep) -> List SBinderSg -> Expr r -> TopBinding
topLevel n as body
  = StgTopLifted
  $ StgNonRec n
  $ StgRhsClosure ReEntrant as body

||| Create a case expression with one Alt which matches the one data constructor
export
unBox
  :  (v1  : SBinder (SingleValue LiftedRep))
  -> {q   : DataConRep}
  -> (d1  : DataConId q)
  -> (t1  : TyConId)
  -> (cb  : SBinder (SingleValue LiftedRep))
  -> (v2  : (AltBinderType (AltDataCon (q ** d1))))
  -> (e   : Expr Core.stgRepType) -- TODO: Fix
  -> Expr Core.stgRepType -- TODO: Fix
unBox v1 d1 t1 cb v2 e =
  StgCase (AlgAlt t1) (StgApp (binderId v1) [] (SingleValue LiftedRep)) cb
  [ MkAlt (AltDataCon (q ** d1)) v2 e ]

||| Create an STGCase which represents an boxing of the given SingleValue representation.
export
box
  :  {rep : PrimRep}
  -> DataConId (AlgDataCon [rep]) -> SBinder (SingleValue rep) -> Expr (SingleValue rep) 
  -> Expr (SingleValue LiftedRep)
box dataCon varToBind primOpExp
  = StgCase (PrimAlt rep) primOpExp varToBind
      [ MkAlt AltDefault () 
        $ StgConApp dataCon (StgVarArg (binderId varToBind))
      ]

export
checkSemiDecEq
  :  Show a
  => SemiDecEq a
  => String -> (exp : a) -> (fnd : a)
  -> Core (exp = fnd)
checkSemiDecEq ctx exp fnd = case (semiDecEq exp fnd) of
  Nothing   => coreFail $ InternalError $ "\{ctx} has different values. Expected \{show exp} , but found: \{show fnd}"
  Just Refl => pure Refl

export
checkDataCon : String -> (r : DataConRep) -> DataConIdSg -> Core (DataConId r)
checkDataCon loc expRep c@(foundRep ** d) = do
  Refl <- checkSemiDecEq loc expRep foundRep
  pure d
