module Idris.Codegen.ExtSTG.Core

import Compiler.ANF
import Core.Context
import Core.Core
import Core.TT
import Data.IOArray
import Data.IntMap
import Data.List
import Data.StringMap
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

-- TODO: Remove export
export
stgRepType : RepType
stgRepType = SingleValue UnliftedRep

export
mkSrcSpan : FC -> SrcSpan
mkSrcSpan (MkFC file (sl,sc) (el,ec))
  = SsRealSrcSpan (MkRealSrcSpan file (sl + 1) (sc + 1) (el + 1) (ec + 1)) Nothing
mkSrcSpan EmptyFC
  = SsUnhelpfulSpan "<no location>"

public export
data BinderKind
  = TermBinder
  | TypeBinder

-- TODO: Remove export
-- TODO: Remove/replace topLevel parameter
export
mkSBinder
  :  {auto _ : UniqueMapRef}
  -> {auto _ : Ref Counter Int}
  -> BinderKind -> Scope -> FC -> String
  -> Core SBinder
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
    binderId
    stgRepType
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
  -> Core SBinder
mkSBinderTyCon = mkSBinder TypeBinder GlobalScope

||| Create a top-level binder for a given name. Used in STG.String module
export
mkSBinderTopLevel
  :  {auto _ : UniqueMapRef}
  -> {auto _ : Ref Counter Int}
  -> String
  -> Core SBinder
mkSBinderTopLevel = mkSBinder TermBinder GlobalScope emptyFC

||| Create a local binder for a given name. Used in STG.String module
export
mkSBinderLocalStr
  :  {auto _ : UniqueMapRef}
  -> {auto _ : Ref Counter Int}
  -> String
  -> Core SBinder
mkSBinderLocalStr n = mkSBinder TermBinder LocalScope emptyFC n

export
mkSBinderLocal
  :  {auto _ : UniqueMapRef}
  -> {auto _ : Ref Counter Int}
  -> FC -> Core.Name.Name -> Int
  -> Core SBinder
mkSBinderLocal f n x = mkSBinder TermBinder LocalScope f (show n ++ ":" ++ show x)

export
mkSBinderName
  :  {auto _ : UniqueMapRef}
  -> {auto _ : Ref Counter Int}
  -> FC -> Core.Name.Name
  -> Core SBinder
mkSBinderName f n = mkSBinder TermBinder GlobalScope f (show n)

export
mkSBinderStr
  :  {auto _ : UniqueMapRef}
  -> {auto _ : Ref Counter Int}
  -> FC -> String
  -> Core SBinder
mkSBinderStr = mkSBinder TermBinder GlobalScope

||| Create a binder for a function that is defined in another STG module.
||| Primary use case for this is the STG-FFI.
export
mkSBinderExtId
  :  {auto _ : UniqueMapRef}
  -> {auto _ : Ref Counter Int}
  -> FC -> String
  -> Core SBinder
mkSBinderExtId = mkSBinder TermBinder HaskellExported

||| Always return a new binder for the given name adding the counter at the end of the name.
||| Used in defining local variables.
export
mkFreshSBinderStr
  :  {auto _ : UniqueMapRef}
  -> {auto _ : Ref Counter Int}
  -> Scope -> FC -> String
  -> Core SBinder
mkFreshSBinderStr scope fc binderName = do
  unique@(MkUnique _ c) <- mkUnique 'l'
  binderId <- MkBinderId <$> mkUnique 'l'
  let typeSig = "mkSBinder: typeSig"
  let details = VanillaId
  let info    = "mkSBinder: IdInfo"
  let defLoc  = mkSrcSpan fc
  pure $ MkSBinder
    (binderName ++ ":" ++ show c)
    binderId
    stgRepType
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
  -> Core SBinder
mkSBinderVar fc n (ALocal x) = mkSBinder TermBinder LocalScope fc (show n ++ ":" ++ show x)
mkSBinderVar fc n ANull      = coreFail $ InternalError $ "mkSBinderVar " ++ show fc ++ " " ++ show n ++ " ANull"

export
mkBinderIdVar
  :  {auto _ : UniqueMapRef}
  -> {auto _ : Ref Counter Int}
  -> FC -> Core.Name.Name -> AVar
  -> Core BinderId
mkBinderIdVar fc n (ALocal x) = MkBinderId <$> uniqueForTerm (show n ++ ":" ++ show x)
mkBinderIdVar fc n ANull      = coreFail $ InternalError $ "mkBinderIdVar " ++ show fc ++ " " ++ show n ++ " ANull"

||| Create a StdVarArg for the Argument of a function application.
|||
||| If the argument is ANull/erased, then it returns a NulAddr literal
export
mkStgArg
  :  {auto _ : UniqueMapRef}
  -> {auto _ : Ref Counter Int}
  -> FC -> Core.Name.Name -> AVar
  -> Core Arg
mkStgArg fc n a@(ALocal _) = StgVarArg <$> mkBinderIdVar fc n a
mkStgArg _  _ ANull        = pure $ StgLitArg $ LitNullAddr
-- Question: Is that a right value for erased argument?
-- Answer: This is not right, this should be Lifted. Make a global erased value, with its binder
--         that is referred here.

||| Lookup a binder based on the name encoded as String
export
mkBinderIdStr
  :  {auto _ : UniqueMapRef}
  -> {auto _ : Ref Counter Int}
  -> String
  -> Core BinderId
mkBinderIdStr = map MkBinderId . uniqueForTerm

export
mkBinderIdName
  :  {auto _ : UniqueMapRef}
  -> {auto _ : Ref Counter Int}
  -> Core.Name.Name
  -> Core BinderId
mkBinderIdName = map MkBinderId . uniqueForTerm . show

export
dataConNameForConstant
  :  {auto _ : UniqueMapRef}
  -> {auto _ : Ref Counter Int}
  -> Constant
  -> Core String
dataConNameForConstant IntType     = pure "I#"
dataConNameForConstant IntegerType = pure "IdrisInteger" -- TODO: This should be GMP int
dataConNameForConstant Bits8Type   = pure "W8#"
dataConNameForConstant Bits16Type  = pure "W16#"
dataConNameForConstant Bits32Type  = pure "W32#"
dataConNameForConstant Bits64Type  = pure "W64#"
-- dataConNameForConstant StringType  = pure "IdrString" -- TODO: Figure this out.
dataConNameForConstant CharType    = pure "C#"
dataConNameForConstant DoubleType  = pure "D#"
dataConNameForConstant WorldType   = pure "IdrWorld"
dataConNameForConstant other = coreFail $ UserError $ "No data constructor for " ++ show other


||| Determine the Data constructor for the boxed primitive type.
|||
||| The name of terms should coincide the ones that are defined in GHC's ecosystem. This
||| would make the transition easier, I hope.
export
dataConIdForConstant
  :  {auto _ : UniqueMapRef}
  -> {auto _ : Ref Counter Int}
  -> Constant
  -> Core DataConId
dataConIdForConstant c = pure $ MkDataConId !(uniqueForTerm !(dataConNameForConstant c))

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
public export
MAIN_UNIT : String
MAIN_UNIT = "main"

||| The module name where Idris STG backend puts every definitions,
||| primitives and user defined codes
public export
MAIN_MODULE : String
MAIN_MODULE = "Main"

namespace DataTypes

  ||| DataType annotation for auto Refs to store the defined STG datatypes during compilation.
  export
  data DataTypes : Type where

  ||| Defined datatypes in STG during the compilation of the module.
  DataTypeMap : Type
  DataTypeMap = StringMap {-UnitId-} (StringMap {-ModuleName-} (List STyCon))

  export
  DataTypeMapRef : Type
  DataTypeMapRef = Ref DataTypes DataTypeMap

  ||| Create the Reference that holds the DataTypeMap
  export
  mkDataTypes : Core DataTypeMapRef
  mkDataTypes = newRef DataTypes empty

  addDataType : UnitId -> ModuleName -> STyCon -> DataTypeMap -> DataTypeMap
  addDataType (MkUnitId u) (MkModuleName m) s = merge (singleton u (singleton m [s]))

  dataTypeList : DataTypeMap -> List (UnitId, List (ModuleName, List STyCon))
  dataTypeList = map (mapFst MkUnitId) . Data.StringMap.toList . map (map (mapFst MkModuleName) . Data.StringMap.toList)

  -- HERE
  public export
  createSTyCon
    :  {auto _ : UniqueMapRef}
    -> {auto _ : Ref Counter Int}
    -> STG.Name -> List (STG.Name, DataConRep)
    -> Core STyCon
  createSTyCon tName dCons = do
    ds <- traverse (\(dName, drep) => pure $
                      MkSDataCon
                        dName
                        (MkDataConId !(uniqueForTerm dName))
                        drep
                        !(mkSBinderTopLevel dName)
                        (SsUnhelpfulSpan dName)
                   )
                   dCons
    pure $ MkSTyCon tName (MkTyConId !(uniqueForType tName)) ds (SsUnhelpfulSpan tName)

  ||| Register an STG datatype under the compilation unit and module name.
  public export
  defineDataType : {auto _ : DataTypeMapRef} -> UnitId -> ModuleName -> STyCon -> Core ()
  defineDataType u m s = do
    x <- get DataTypes
    put DataTypes (addDataType u m s x)

  ||| Return all the STG data type definition that were registered during the compilation
  export
  getDefinedDataTypes : {auto _ : DataTypeMapRef} -> Core (List (UnitId, List (ModuleName, List STyCon)))
  getDefinedDataTypes = map dataTypeList $ get DataTypes

||| Creates a DataConId for the given data constructor name.
export
mkDataConId
  :  {auto _ : UniqueMapRef}
  -> {auto _ : Ref Counter Int}
  -> Core.Name.Name -- Name of the fully qualified data constructor (not an Idris primitive type)
  -> Core DataConId
mkDataConId n = MkDataConId <$> uniqueForTerm (show n)
