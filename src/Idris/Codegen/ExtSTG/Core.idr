module Idris.Codegen.ExtSTG.Core

import Compiler.ANF
import Core.Context
import Core.Core
import Core.TT
import Data.IOArray
import Libraries.Data.IntMap
import Data.List
import Libraries.Data.StringMap
import Data.Strings
import Data.Vect
import Idris.Codegen.ExtSTG.STG
import Prelude


export
logLine : String -> Core ()
logLine msg = coreLift $ putStrLn msg

namespace Counter

  ||| Counter annotation for creating Unique identifiers.
  export
  data Counter : Type where

  export
  mkCounter : Core (Ref Counter Int)
  mkCounter = newRef Counter 0


namespace Uniques

  ||| Uniques annotation for storing unique identifiers associated with names.
  export
  data Uniques : Type where

  export
  UniqueMap : Type
  UniqueMap =
    ( StringMap Unique -- Namespace for types
    , StringMap Unique -- Namespace for terms
    )

  export
  UniqueMapRef : Type
  UniqueMapRef = Ref Uniques UniqueMap

  export
  mkUniques : Core UniqueMapRef
  mkUniques = newRef Uniques (empty, empty)

  export
  mkUnique : {auto _ : Ref Counter Int} -> Char -> Core Unique
  mkUnique c = do
    x <- get Counter
    let u = MkUnique c x
    put Counter (x + 1)
    pure u

  export
  uniqueForType
    :  {auto _ : UniqueMapRef}
    -> {auto _ : Ref Counter Int}
    -> String
    -> Core Unique
  uniqueForType name = do
    (ty,te) <- get Uniques
    case lookup name ty of
      Nothing => do
        u <- mkUnique 'y'
        put Uniques (insert name u ty, te)
        pure u
      Just u => do
        pure u

  export
  uniqueForTerm
    :  {auto _ : UniqueMapRef}
    -> {auto _ : Ref Counter Int}
    -> String
    -> Core Unique
  uniqueForTerm name = do
    (ty,te) <- get Uniques
    case lookup name te of
      Nothing => do
        u <- mkUnique 'e'
        put Uniques (ty,insert name u te)
        pure u
      Just u => do
        pure u

  public export
  HardcodedUnique : Type
  HardcodedUnique = (String, List String, String, Unique, RepType)

  public export
  hardcodedRepType : HardcodedUnique -> RepType
  hardcodedRepType (_,_,_,_,r) = r

  ||| void# in STG
  export
  hardcodedVoidHash : HardcodedUnique
  hardcodedVoidHash = ("ghc-prim", ["GHC", "Prim"], "void#", MkUnique '0' 21, SingleValue VoidRep)

-- TODO: Remove export
public export
stgRepType : RepType
stgRepType = SingleValue LiftedRep

export
mkSrcSpan : FC -> SrcSpan
mkSrcSpan (MkFC file (sl,sc) (el,ec))
  = SsRealSrcSpan (MkRealSrcSpan file (sl + 1) (sc + 1) (el + 1) (ec + 1)) Nothing
mkSrcSpan (MkVirtualFC file (sl,sc) (el,ec))
  = SsRealSrcSpan (MkRealSrcSpan file (sl + 1) (sc + 1) (el + 1) (ec + 1)) Nothing
mkSrcSpan EmptyFC
  = SsUnhelpfulSpan "<no location>"

public export
data BinderKind
  = TermBinder
  | TypeBinder

export
mkSBinderHardcoded
  : UniqueMapRef => Ref Counter Int => (h : HardcodedUnique) -> FC -> Core (SBinder (hardcodedRepType h))
mkSBinderHardcoded (_,_,binderName,unique,repType) fc = do
  let binderId = MkBinderId unique
  let typeSig = "mkSBinder: hardcodedSig"
  let details = VanillaId
  let info = "mkSBinder: IdInfo"
  let defLoc = mkSrcSpan fc
  pure $ MkSBinder
    binderName
    repType
    binderId
    typeSig
    HaskellExported
    details
    info
    defLoc

-- TODO: Remove export
-- TODO: Remove/replace topLevel parameter
export
mkSBinder
  :  {auto _ : UniqueMapRef}
  -> {auto _ : Ref Counter Int}
  -> BinderKind -> Scope -> FC -> String
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
    binderName
    stgRepType
    binderId
    typeSig
    scope
    details
    info
    defLoc

-- TODO: Remove export
-- TODO: Remove/replace topLevel parameter
export
mkSBinderRep
  :  {auto _ : UniqueMapRef}
  -> {auto _ : Ref Counter Int}
  -> BinderKind -> Scope -> (rep : RepType) -> FC -> String
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
    binderName
    rep
    binderId
    typeSig
    scope
    details
    info
    defLoc

-- export
mkSBinderTyCon
  :  {auto _ : UniqueMapRef}
  -> {auto _ : Ref Counter Int}
  -> FC -> String -- TODO : Constant
  -> Core (SBinder Core.stgRepType)
mkSBinderTyCon = mkSBinder TypeBinder GlobalScope

||| Create a top-level binder for a given name. Mainly, used in STG.String module
export
mkSBinderTopLevel
  :  {auto _ : UniqueMapRef}
  -> {auto _ : Ref Counter Int}
  -> String
  -> Core (SBinder Core.stgRepType)
mkSBinderTopLevel = mkSBinder TermBinder GlobalScope emptyFC

||| Create a local binder for a given name. Used in STG.String module
export
mkSBinderLocalStr
  :  {auto _ : UniqueMapRef}
  -> {auto _ : Ref Counter Int}
  -> String
  -> Core (SBinder Core.stgRepType)
mkSBinderLocalStr n = mkSBinder TermBinder LocalScope emptyFC n

||| Create a local binder for a given name. Used in STG.String module
export
mkSBinderRepLocalStr
  :  {auto _ : UniqueMapRef}
  -> {auto _ : Ref Counter Int}
  -> (rep : RepType)
  -> String
  -> Core (SBinder rep)
mkSBinderRepLocalStr r n = mkSBinderRep TermBinder LocalScope r emptyFC n


export
mkSBinderLocal
  :  {auto _ : UniqueMapRef}
  -> {auto _ : Ref Counter Int}
  -> FC -> Core.Name.Name -> Int
  -> Core (SBinder Core.stgRepType)
mkSBinderLocal f n x = mkSBinder TermBinder LocalScope f (show n ++ ":" ++ show x)

export
mkSBinderRepLocal
  :  {auto _ : UniqueMapRef}
  -> {auto _ : Ref Counter Int}
  -> (rep : RepType) -> FC -> Core.Name.Name -> Int
  -> Core (SBinder rep)
mkSBinderRepLocal r f n x = mkSBinderRep TermBinder LocalScope r f (show n ++ ":" ++ show x)


export
mkSBinderName
  :  {auto _ : UniqueMapRef}
  -> {auto _ : Ref Counter Int}
  -> FC -> Core.Name.Name
  -> Core (SBinder Core.stgRepType)
mkSBinderName f n = mkSBinder TermBinder GlobalScope f (show n)

export
mkSBinderStr
  :  {auto _ : UniqueMapRef}
  -> {auto _ : Ref Counter Int}
  -> FC -> String
  -> Core (SBinder Core.stgRepType)
mkSBinderStr = mkSBinder TermBinder GlobalScope

||| Create a binder for a function that is defined in another STG module.
||| Primary use case for this is the STG-FFI, or exported from the module.
export
mkSBinderExtId
  :  {auto _ : UniqueMapRef}
  -> {auto _ : Ref Counter Int}
  -> FC -> String
  -> Core (SBinder Core.stgRepType)
mkSBinderExtId = mkSBinder TermBinder HaskellExported

||| Always return a new binder for the given name adding the counter at the end of the name.
||| Used in defining local variables.
export
mkFreshSBinderStr
  :  {auto _ : UniqueMapRef}
  -> {auto _ : Ref Counter Int}
  -> Scope -> FC -> String
  -> Core (SBinder Core.stgRepType)
mkFreshSBinderStr scope fc binderName = do
  unique@(MkUnique _ c) <- mkUnique 'l'
  binderId <- MkBinderId <$> mkUnique 'l'
  let typeSig = "mkSBinder: typeSig"
  let details = VanillaId
  let info    = "mkSBinder: IdInfo"
  let defLoc  = mkSrcSpan fc
  pure $ MkSBinder
    (binderName ++ ":" ++ show c)
    stgRepType
    binderId
    typeSig
    scope
    details
    info
    defLoc

||| Always return a new binder for the given name adding the counter at the end of the name.
||| Used in defining local variables.
export
mkFreshSBinderRepStr
  :  {auto _ : UniqueMapRef}
  -> {auto _ : Ref Counter Int}
  -> Scope -> (rep : RepType) -> FC -> String
  -> Core (SBinder rep)
mkFreshSBinderRepStr scope rep fc binderName = do
  unique@(MkUnique _ c) <- mkUnique 'l'
  binderId <- MkBinderId <$> mkUnique 'l'
  let typeSig = "mkSBinder: typeSig"
  let details = VanillaId
  let info    = "mkSBinder: IdInfo"
  let defLoc  = mkSrcSpan fc
  pure $ MkSBinder
    (binderName ++ ":" ++ show c)
    rep
    binderId
    typeSig
    scope
    details
    info
    defLoc

export
mkSBinderVar
  :  {auto _ : UniqueMapRef}
  -> {auto _ : Ref Counter Int}
  -> FC -> Core.Name.Name -> AVar
  -> Core (SBinder Core.stgRepType)
mkSBinderVar fc n (ALocal x) = mkSBinder TermBinder LocalScope fc (show n ++ ":" ++ show x)
mkSBinderVar fc n ANull      = coreFail $ InternalError $ "mkSBinderVar " ++ show fc ++ " " ++ show n ++ " ANull"

export
mkBinderIdVar
  :  {auto _ : UniqueMapRef}
  -> {auto _ : Ref Counter Int}
  -> FC -> Core.Name.Name -> (r : RepType) -> AVar
  -> Core (BinderId r)
mkBinderIdVar fc n r (ALocal x) = MkBinderId <$> uniqueForTerm (show n ++ ":" ++ show x)
mkBinderIdVar fc n r ANull      = coreFail $ InternalError $ "mkBinderIdVar " ++ show fc ++ " " ++ show n ++ " ANull"

||| Create a StdVarArg for the Argument of a function application.
|||
||| If the argument is ANull/erased, then it returns a NulAddr literal
export
mkStgArg
  :  {auto _ : UniqueMapRef}
  -> {auto _ : Ref Counter Int}
  -> FC -> Core.Name.Name -> AVar
  -> Core ArgSg
mkStgArg fc n a@(ALocal _) = mkArgSg . StgVarArg <$> (mkBinderIdVar fc n stgRepType a)
mkStgArg _  _ ANull        = pure $ mkArgSg $ StgLitArg $ LitNullAddr
-- Question: Is that a right value for erased argument?
-- Answer: This is not right, this should be Lifted. Make a global erased value, with its binder
--         that is referred here.

||| Lookup a binder based on the name encoded as String
export
mkBinderIdStr
  :  {auto _ : UniqueMapRef}
  -> {auto _ : Ref Counter Int}
  -> String
  -> Core (BinderId Core.stgRepType)
mkBinderIdStr = map MkBinderId . uniqueForTerm -- TODO: Is this right?

export
mkBinderIdName
  :  {auto _ : UniqueMapRef}
  -> {auto _ : Ref Counter Int}
  -> Core.Name.Name
  -> Core (BinderId Core.stgRepType)
mkBinderIdName = map MkBinderId . uniqueForTerm . show -- TODO: Is this right?

export
dataConNameForConstant
  :  {auto _ : UniqueMapRef}
  -> {auto _ : Ref Counter Int}
  -> Constant
  -> Core String
dataConNameForConstant IntType     = pure "I#"
dataConNameForConstant IntegerType = pure "GMPInt" -- TODO: This should be GMP int
dataConNameForConstant Bits8Type   = pure "W8#"
dataConNameForConstant Bits16Type  = pure "W16#"
dataConNameForConstant Bits32Type  = pure "W32#"
dataConNameForConstant Bits64Type  = pure "W64#"
-- dataConNameForConstant StringType  = pure "IdrString" -- TODO: Figure this out.
dataConNameForConstant CharType    = pure "C#"
dataConNameForConstant DoubleType  = pure "D#"
dataConNameForConstant WorldType   = pure "IdrWorld"
dataConNameForConstant other = coreFail $ UserError $ "No data constructor for " ++ show other

export
typeConNameForConstant
  :  {auto _ : UniqueMapRef}
  -> {auto _ : Ref Counter Int}
  -> Constant
  -> Core String
typeConNameForConstant IntType     = pure "Int"
typeConNameForConstant IntegerType = pure "IInt" -- TODO: This should be GMP int
typeConNameForConstant Bits8Type   = pure "Word8"
typeConNameForConstant Bits16Type  = pure "Word16"
typeConNameForConstant Bits32Type  = pure "Word32"
typeConNameForConstant Bits64Type  = pure "Word64"
-- typeConNameForConstant StringType  = pure "IdrString" -- TODO: Figure this out.
typeConNameForConstant CharType    = pure "Char"
typeConNameForConstant DoubleType  = pure "Double"
typeConNameForConstant WorldType   = pure "IdrWorld"
typeConNameForConstant other = coreFail $ UserError $ "No data constructor for " ++ show other

||| Create a TyConId for the given idris primtive type.
export
tyConIdForConstant
  :  {auto _ : UniqueMapRef}
  -> {auto _ : Ref Counter Int}
  -> Constant
  -> Core TyConId
tyConIdForConstant c = pure $ MkTyConId !(uniqueForType !(typeConNameForConstant c))

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

namespace DataTypes

  ||| DataType annotation for auto Refs to store the defined STG datatypes during compilation.
  export
  data DataTypes : Type where

  ||| Defined datatypes in STG during the compilation of the module.
  DataTypeMap : Type
  DataTypeMap = StringMap {-UnitId-} (StringMap {-ModuleName-} (List STyCon))

  DataConIdMap : Type
  DataConIdMap = StringMap {-Unique-} (List SDataConSg) -- Should be unique

  TyConIdMap : Type
  TyConIdMap = StringMap {-Unique-} (List STyCon) -- Should be unique

  export
  DataTypeMapRef : Type
  DataTypeMapRef = Ref DataTypes (DataTypeMap, DataConIdMap, TyConIdMap)

  ||| Create the Reference that holds the DataTypeMap
  export
  mkDataTypes : Core DataTypeMapRef
  mkDataTypes = newRef DataTypes (empty, empty, empty)

  addDataType
    : UnitId -> ModuleName -> STyCon
    -> (DataTypeMap, DataConIdMap, TyConIdMap) -> (DataTypeMap, DataConIdMap, TyConIdMap)
  addDataType (MkUnitId u) (MkModuleName m) s (dm,dc,tc) =
    ( merge (singleton u (singleton m [s])) dm
    , foldl merge dc $ map (\d => singleton (show (dataConUnique (ident (snd d)))) [d]) s.DataCons
    , merge (singleton (show (tyConUnique s.Id)) [s]) tc
    )

  export
  checkDefinedDataCon : DataTypeMapRef => Unique -> Core (Maybe SDataConSg)
  checkDefinedDataCon u = do
    (_,dc,_) <- get DataTypes
    case lookup (show u) dc of
      Nothing  => pure Nothing
      Just [d] => pure $ Just d
      Just ds  => coreFail $ InternalError $ "Non unique datatype for DataCon: " ++ show u

  export
  checkDefinedSTyCon : DataTypeMapRef => TyConId -> Core (Maybe STyCon)
  checkDefinedSTyCon (MkTyConId u) = do
    (_,_,tc) <- get DataTypes
    case lookup (show u) tc of
      Nothing  => pure Nothing
      Just [t] => pure $ Just t
      Just ts  => coreFail $ InternalError $ "Non unqiue typecon for TyCon:" ++ show (u,ts)

  dataTypeList : DataTypeMap -> List (UnitId, List (ModuleName, List STyCon))
  dataTypeList = map (mapFst MkUnitId) . Data.StringMap.toList . map (map (mapFst MkModuleName) . Data.StringMap.toList)

  export
  createSTyCon
    :  {auto _ : UniqueMapRef}
    -> {auto _ : Ref Counter Int}
    -> (STG.Name, SrcSpan) -> List (STG.Name, DataConRep, SrcSpan)
    -> Core STyCon
  createSTyCon (tName,tSpan) dCons = do
    ds <- traverse (\(dName, drep, span) => pure $ mkSDataConSg $
                      MkSDataCon
                        dName
                        drep
                        (MkDataConId !(uniqueForTerm dName))
                        !(mkSBinderTopLevel dName)
                        span
                   )
                   dCons
    pure $ MkSTyCon tName (MkTyConId !(uniqueForType tName)) ds tSpan

  ||| Register an STG datatype under the compilation unit and module name.
  export
  defineDataType : {auto _ : DataTypeMapRef} -> UnitId -> ModuleName -> STyCon -> Core ()
  defineDataType u m s = do
    x <- get DataTypes
    put DataTypes (addDataType u m s x)

  ||| Return all the STG data type definition that were registered during the compilation
  export
  getDefinedDataTypes : {auto _ : DataTypeMapRef} -> Core (List (UnitId, List (ModuleName, List STyCon)))
  getDefinedDataTypes = map (dataTypeList . fst) $ get DataTypes

||| Creates a DataConId for the given data constructor name, checks if the name is already have
||| a definition, if not throw an InternalError
export
mkDataConIdStr
  : UniqueMapRef
  => Ref Counter Int
  => DataTypeMapRef
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
  : UniqueMapRef
  => Ref Counter Int
  => DataTypeMapRef
  => Core.Name.Name -- Name of the fully qualified data constructor (not an Idris primitive type)
  -> Core DataConIdSg
mkDataConId n = mkDataConIdStr (show n)

export
mkTyConIdStr
  : UniqueMapRef
  => Ref Counter Int
  => DataTypeMapRef
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
dataConIdForConstant
  :  UniqueMapRef
  => Ref Counter Int
  => DataTypeMapRef
  => Constant
  -> Core DataConIdSg
dataConIdForConstant c = mkDataConIdStr !(dataConNameForConstant c)

||| Determine the Data constructor for the boxed primitive type.
|||
||| The name of terms should coincide the ones that are defined in GHC's ecosystem. This
||| would make the transition easier, I hope.
export
dataConIdRepForConstant
  : UniqueMapRef
  => Ref Counter Int
  => DataTypeMapRef
  => (r : PrimRep)
  -> Constant
  -> Core (DataConId (AlgDataCon [r]))
dataConIdRepForConstant r c = do
  ((AlgDataCon [q]) ** d) <- dataConIdForConstant c
    | other => coreFail $ InternalError
                        $ unwords
                          [ "dataConIdRepForConstant:"
                          , show c, "got an unexpectly shaped binder:"
                          , show other, "expected:" -- , show (AlgDataCon [r])
                          ]
  let Just Refl = semiDecEq r q
    | Nothing => coreFail $ InternalError
                          $ unwords
                            [ "dataConIdRepForConstant:"
                            , show c, "doesn't have the expected"
                            , show r, "found"
                            , show q
                            ]
  pure d

||| Always creates a fresh binder, its main purpose to create a binder which won't be used, mainly StgCase
export
nonused : UniqueMapRef => Ref Counter Int => Core (SBinder (SingleValue LiftedRep))
nonused = mkFreshSBinderStr LocalScope emptyFC "nonused"

export
nonusedRep : UniqueMapRef => Ref Counter Int => (rep : RepType) -> Core (SBinder rep)
nonusedRep rep = mkFreshSBinderRepStr LocalScope rep emptyFC "nonused"

export
topLevel : {r : RepType} -> SBinder (SingleValue LiftedRep) -> List SBinderSg -> Expr r -> TopBinding
topLevel n as body
  = StgTopLifted
  $ StgNonRec n
  $ StgRhsClosure ReEntrant as body

||| Create a case expression with one Alt which matches the one data constructor
export
unBox
  :  (v1     : SBinder (SingleValue LiftedRep))
  -> {q      : DataConRep}
  -> (d1     : DataConId q)
  -> (t1     : TyConId)
  -> (cb     : SBinder (SingleValue LiftedRep))
  -> (v2     : (AltBinderType (AltDataCon (q ** d1))))
  -> (e      : Expr Core.stgRepType) -- TODO: Fix
  -> Expr Core.stgRepType -- TODO: Fix
unBox v1 d1 t1 cb v2 e =
  StgCase (AlgAlt t1) (StgApp (binderId v1) [] (SingleValue LiftedRep)) cb
  [ MkAlt (AltDataCon (q ** d1)) v2 e ]

export
checkSemiDecEq : Show a => SemiDecEq a => String -> (x : a) -> (y : a) -> Core (x = y)
checkSemiDecEq ctx x y = case (semiDecEq x y) of
  Nothing   => coreFail $ InternalError $ ctx ++ " different values: " ++ show (x, y)
  Just Refl => pure Refl

export
checkDataCon : String -> (r : DataConRep) -> DataConIdSg -> Core (DataConId r)
checkDataCon loc r c@(q ** d) = do
  Refl <- checkSemiDecEq loc r q
  pure d
