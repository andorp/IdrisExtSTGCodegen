module Idris.Codegen.ExtSTG.TTtoSTG

import Prelude
import Compiler.ANF
import Idris.Codegen.ExtSTG.STG
import Core.Core
import Core.Context
import Core.TT
import Data.Vect
import Data.List
import Data.StringMap
import Data.IntMap
import Data.Strings
import Data.IOArray

{-
Implementation notes

 * Idris primitive types are represented as Boxed values in STG. They are unboxed when they are applied to
   primitive operations, because primitive operations in STG work unboxed values. And they are unboxed
   when Idris' case expression matches on primitive values.
 * Handle of String literals from Idris to STG is different. In Idris ANF we can case match on string literals,
   but in STG it is not possible. In this case we have to generate a ifthenelse like embedded case matching
   for STG where the case scrutinee is a primitive operation to compare the value from the literal found
   in the case alternatives. Which introduces the next problem, string literals in STG are top-binders and
   they are represented as Addr# by the CMM. Having LitString with String is very misleading.
   So the points here:
   - When the code generator finds a String matcing case, it creates a top level bindings for String,
     which is considered as Addr#
   - When the Strings values are created, eg via readLine, they should do ByteArray allocations, which
     will be handled bye the garbage collector. When implementing a String Equality check (we must know
     the representation of the string OR just default back to Addr# ???)
     This step needs to be further checking TODO
   - String comparism primitive cStrCmp must be implemented in STG to solve this problem, this should a top
     level binding, whith a recursive function cStrCmp function.
   - The case chain which represents the ifthenelse chain should use the cStrCmp function.
 * Datatypes in STG and Idris ANF's IR are similar.
   - In STG datatypes are created by the STyCon which associates a type name with a Unique
     identifier and the list of data constructors SDataCon with their unique IDs too.
   - In ANF the data constructors and type constructors occupy the same namespace and the ones which
     are used only appear in the structure of the ANF program. This is not enough information for
     the STG backend to generate STG data type definitions.
   - Because there is only partial information in the ANF program, there is a need to look into
     the GlobalDef Context which is part of the Core compiler abstraction via `Ref Ctxt Defs`.
     From that source of information we have to remap type names and constructor names,
     which is currently done by a hack. All this detail can be found in the:
     TConsAndDCons namespace
 * TODO: Write about Erased
 * TODO: Integer and String and AnyPtr
 * ...

TODOs
[+] Remove (Name, Name, Name) parameter from StgApp
[+] Add FC information to binders
[+] Write DataCon -> TypeCon association
[+] Separate STG Type and Term namespaces
[+] Fix compileDataConId
[+] Implement primitive values marshalled to the right GHC boxed primitives
[.] Implement Erased values, erased variables
[ ] Implement Crash primitive
[ ] Handle primitive case matches accordingly
[ ] Generate STG main entry
[ ] Handle String matches with ifthenelse chains, using stringEq primop from STG
    - Create a test program which reads from input.
[ ] Implement casting
[.] Implement primitive operations
    [ ] ShiftL/ShiftR for Word needs a wrapper: Differences in parameters at STG and ANF side.
    [ ] DoubleFloor/Ceiling also needs a wrapper function as in STG the result is an Integer.
    [ ] Check if the BelieveMe operation is correct in STG
[ ] FFI calls AExtPrim
    [ ] Create a test program which FFI calls into a library.
    [ ] Foreign definitions should be looked up from a file, which can be modified by the user.
[ ] Module compilation
[ ] ...
-}

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

  export
  defineDataType : {auto _ : DataTypeMapRef} -> UnitId -> ModuleName -> STyCon -> Core ()
  defineDataType u m s = do
    x <- get DataTypes
    put DataTypes (addDataType u m s x)

  export
  getDefinedDataTypes : {auto _ : DataTypeMapRef} -> Core (List (UnitId, List (ModuleName, List STyCon)))
  getDefinedDataTypes = map dataTypeList $ get DataTypes



stgRepType : RepType
stgRepType = SingleValue UnliftedRep

mkSrcSpan : FC -> SrcSpan
mkSrcSpan (MkFC file (sl,sc) (el,ec))
  = SsRealSrcSpan (MkRealSrcSpan file (sl + 1) (sc + 1) (el + 1) (ec + 1)) Nothing
mkSrcSpan EmptyFC
  = SsUnhelpfulSpan "<no location>"

mkSBinder
  :  {auto _ : UniqueMapRef}
  -> {auto _ : Ref Counter Int}
  -> FC -> Bool -> String
  -> Core SBinder
mkSBinder fc topLevel binderName = do
  binderId <- MkBinderId <$> uniqueForTerm binderName
  let typeSig = "mkSBinder: typeSig"
  let scope   = GlobalScope
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

mkSBinderTyCon
  :  {auto _ : UniqueMapRef}
  -> {auto _ : Ref Counter Int}
  -> FC -> String -- TODO : Constant
  -> Core SBinder
mkSBinderTyCon fc = mkSBinder fc False

mkSBinderLocal
  :  {auto _ : UniqueMapRef}
  -> {auto _ : Ref Counter Int}
  -> FC -> Core.Name.Name -> Int
  -> Core SBinder
mkSBinderLocal f n x = mkSBinder f False (show n ++ ":" ++ show x)

mkSBinderName
  :  {auto _ : UniqueMapRef}
  -> {auto _ : Ref Counter Int}
  -> FC -> Core.Name.Name
  -> Core SBinder
mkSBinderName f n = mkSBinder f True $ show n

mkSBinderStr
  :  {auto _ : UniqueMapRef}
  -> {auto _ : Ref Counter Int}
  -> FC -> String
  -> Core SBinder
mkSBinderStr fc = mkSBinder fc True

mkSBinderVar
  :  {auto _ : UniqueMapRef}
  -> {auto _ : Ref Counter Int}
  -> FC -> Core.Name.Name -> AVar
  -> Core SBinder
mkSBinderVar fc n (ALocal x) = mkSBinder fc False (show n ++ ":" ++ show x)
mkSBinderVar fc n ANull      = coreFail $ InternalError $ "mkSBinderVar " ++ show fc ++ " " ++ show n ++ " ANull"

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
mkStgArg
  :  {auto _ : UniqueMapRef}
  -> {auto _ : Ref Counter Int}
  -> FC -> Core.Name.Name -> AVar
  -> Core SArg
mkStgArg fc n a@(ALocal _) = StgVarArg <$> mkBinderIdVar fc n a
mkStgArg _  _ ANull        = pure $ StgLitArg $ LitNullAddr
-- Question: Is that a right value for erased argument?
-- Answer: This is not right, this should be Lifted. Make a global erased value, with its binder
--         that is referred here.

mkBinderIdName
  :  {auto _ : UniqueMapRef}
  -> {auto _ : Ref Counter Int}
  -> Core.Name.Name
  -> Core BinderId
mkBinderIdName = map MkBinderId . uniqueForTerm . show

||| Creates a DataConId for the given data constructor name.
compileDataConId
  :  {auto _ : UniqueMapRef}
  -> {auto _ : Ref Counter Int}
  -> Core.Name.Name -- Name of the fully qualified data constructor (not an Idris primitive type)
  -> Core DataConId
compileDataConId n = MkDataConId <$> uniqueForTerm (show n)

||| PrimType when the constant is compiled insides the box.
constantToPrimRep : Constant -> Core (List PrimRep)
constantToPrimRep IntType     = pure [IntRep]
constantToPrimRep IntegerType = pure [IntRep] -- TODO: This is not the right representation for integer
constantToPrimRep Bits8Type   = pure [Word8Rep]
constantToPrimRep Bits16Type  = pure [Word16Rep]
constantToPrimRep Bits32Type  = pure [Word32Rep]
constantToPrimRep Bits64Type  = pure [Word64Rep]
constantToPrimRep DoubleType  = pure [DoubleRep]
constantToPrimRep StringType  = pure [AddrRep]
constantToPrimRep CharType    = pure [Word8Rep] -- TODO: Check if this is the right type for Chars?
constantToPrimRep WorldType   = pure []
constantToPrimRep other = coreFail $ InternalError $ "No PrimRep for " ++ show other

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
typeConNameForConstant StringType  = pure "IdrString" -- TODO: Figure this out.
typeConNameForConstant CharType    = pure "Char"
typeConNameForConstant DoubleType  = pure "Double"
typeConNameForConstant WorldType   = pure "IdrWorld"
typeConNameForConstant other = coreFail $ UserError $ "No data constructor for " ++ show other

||| Create a TyConId for the given idris primtive type.
tyConIdForConstant
  :  {auto _ : UniqueMapRef}
  -> {auto _ : Ref Counter Int}
  -> Constant
  -> Core TyConId
tyConIdForConstant c = pure $ MkTyConId !(uniqueForType !(typeConNameForConstant c))

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
dataConNameForConstant StringType  = pure "IdrString" -- TODO: Figure this out.
dataConNameForConstant CharType    = pure "C#"
dataConNameForConstant DoubleType  = pure "D#"
dataConNameForConstant WorldType   = pure "IdrWorld"
dataConNameForConstant other = coreFail $ UserError $ "No data constructor for " ++ show other

||| Determine the Data constructor for the boxed primitive type.
|||
||| The name of terms should coincide the ones that are defined in GHC's ecosystem. This
||| would make the transition easier, I hope.
dataConIdForConstant
  :  {auto _ : UniqueMapRef}
  -> {auto _ : Ref Counter Int}
  -> Constant
  -> Core DataConId
dataConIdForConstant c = pure $ MkDataConId !(uniqueForTerm !(dataConNameForConstant c))

||| Determinie the Data constructor for the boxed primitive value.
||| Used in creating PrimVal
|||
||| The name of terms should coincide the ones that are defined in GHC's ecosystem. This
||| would make the transition easier, I hope.
dataConIdForValueConstant
  :  {auto _ : UniqueMapRef}
  -> {auto _ : Ref Counter Int}
  -> Constant
  -> Core DataConId
dataConIdForValueConstant (I _)    = MkDataConId <$> uniqueForTerm "I#"
dataConIdForValueConstant (BI _)   = MkDataConId <$> uniqueForTerm "IdrInteger" -- TODO: This should be GMP int
dataConIdForValueConstant (B8 _)   = MkDataConId <$> uniqueForTerm "W8#"
dataConIdForValueConstant (B16 _)  = MkDataConId <$> uniqueForTerm "W16#"
dataConIdForValueConstant (B32 _)  = MkDataConId <$> uniqueForTerm "W32#"
dataConIdForValueConstant (B64 _)  = MkDataConId <$> uniqueForTerm "W64#"
dataConIdForValueConstant (Str _)  = MkDataConId <$> uniqueForTerm "IdrString" -- TODO: Figure this out.
dataConIdForValueConstant (Ch _)   = MkDataConId <$> uniqueForTerm "C#"
dataConIdForValueConstant (Db _)   = MkDataConId <$> uniqueForTerm "D#"
dataConIdForValueConstant WorldVal = MkDataConId <$> uniqueForTerm "IdrWorld"
dataConIdForValueConstant other   = coreFail $ InternalError $ "dataConIdForValueConstant " ++ show other

||| The unit where the Idris STG backend puts every definitions,
||| primitives and used defined codes
MAIN_UNIT : String
MAIN_UNIT = "main"

||| The module name where Idris STG backend puts every definitions,
||| primitives and user defined codes
MAIN_MODULE : String
MAIN_MODULE = "Main"

ERASED_TYPE_NAME : String
ERASED_TYPE_NAME = "Erased"

ERASED_CON_NAME : String
ERASED_CON_NAME = "Erased"

ERASED_TOPLEVEL_NAME : String
ERASED_TOPLEVEL_NAME = "idrisErasedValue"

defineErasedADT
  :  {auto _ : UniqueMapRef}
  -> {auto _ : Ref Counter Int}
  -> {auto _ : DataTypeMapRef}
  -> Core ()
defineErasedADT = do
  t <- MkTyConId   <$> uniqueForType ERASED_TYPE_NAME
  n <- MkDataConId <$> uniqueForTerm ERASED_CON_NAME
  d <- pure $ MkSTyCon ERASED_TYPE_NAME t
                         [ MkSDataCon ERASED_CON_NAME n
                                        (AlgDataCon [])
                                        !(mkSBinderStr emptyFC ("mk" ++ ERASED_CON_NAME))
                                        (SsUnhelpfulSpan "<no location>") ]
                         (SsUnhelpfulSpan "<no location>")
  defineDataType (MkUnitId MAIN_UNIT) (MkModuleName MAIN_MODULE) d

definePrimitiveDataType
  :  {auto _ : UniqueMapRef}
  -> {auto _ : Ref Counter Int}
  -> {auto _ : DataTypeMapRef}
  -> (String, String, Constant)
  -> Core ()
definePrimitiveDataType (u, m, c) = do
  t <- typeConNameForConstant c
  n <- dataConNameForConstant c
  d <- pure $ MkSTyCon t !(tyConIdForConstant c)
                         [ MkSDataCon n !(dataConIdForConstant c)
                                        (AlgDataCon !(constantToPrimRep c))
                                        !(mkSBinderStr emptyFC ("mk" ++ n))
                                        (SsUnhelpfulSpan "<no location>") ]
                         (SsUnhelpfulSpan "<no location>")
  defineDataType (MkUnitId u) (MkModuleName m) d

||| Create the primitive types section in the STG module.
|||
||| Idris primitive types are represented as boxed values in STG, with a datatype with one constructor.
||| Eg: data IdrInt = IdrInt #IntRep
definePrimitiveDataTypes
  :  {auto _ : UniqueMapRef}
  -> {auto _ : Ref Counter Int}
  -> {auto _ : DataTypeMapRef}
  -> Core ()
definePrimitiveDataTypes = traverse_ definePrimitiveDataType
 [ ("ghc-prim", "GHC.Types",  IntType)
 , (MAIN_UNIT,  MAIN_MODULE,  IntegerType) -- TODO: This is bad, GMP Integer is needed here
 , ("base",     "GHC.Word",   Bits8Type)
 , ("base",     "GHC.Word",   Bits16Type)
 , ("base",     "GHC.Word",   Bits32Type)
 , ("base",     "GHC.Word",   Bits64Type)
 , ("ghc-prim", "GHC.Types",  CharType)
 , ("ghc-prim", "GHC.Types",  DoubleType)
 , (MAIN_UNIT,  MAIN_MODULE,  StringType)
 , (MAIN_UNIT,  MAIN_MODULE,  WorldType)
 ]

||| Creates a multilevel case statement block to unwrap/wrap the primitive values
||| around a two parameter STG primitive function call.
binPrimOp
  :  {auto _ : UniqueMapRef}
  -> {auto _ : Ref Counter Int}
  -> FC -> Core.Name.Name
  -> Constant -> StgOp -> Vect 2 AVar -> Constant
  -> Core SExpr
binPrimOp fc n ty op as rt = do
  [arg1, arg2] <- traverseVect (mkBinderIdVar fc n) as
  -- As we box everyting, and the result will be Lifted
  let resultType = SingleValue LiftedRep
  [primRep] <- constantToPrimRep ty
    | other => coreFail $ InternalError $ "Invalid PrimRep in binPrimOp: " ++ show ty
  let resultTypeName = Nothing
  pure $ StgCase
          (StgApp arg1 [] resultType)
          !(mkSBinderLocal fc n 3)
          !(AlgAlt <$> tyConIdForConstant ty)
          [ MkAlt !(AltDataCon <$> dataConIdForConstant ty) [!(mkSBinderLocal fc n 4)]
             (StgCase
                (StgApp arg2 [] resultType)
                !(mkSBinderLocal fc n 4)
                !(AlgAlt <$> tyConIdForConstant ty)
                [ MkAlt !(AltDataCon <$> dataConIdForConstant ty) [!(mkSBinderLocal fc n 5)]
                    (StgCase
                      (StgOpApp op
                        [ !(StgVarArg <$> mkBinderIdVar fc n (ALocal 4))
                        , !(StgVarArg <$> mkBinderIdVar fc n (ALocal 5))
                        ]
                        resultType -- TODO: Unboxed PrimRep like Int16Rep
                        resultTypeName)
                      !(mkSBinderLocal fc n 6)
                      (PrimAlt primRep)
                      [ MkAlt AltDefault []
                          (StgConApp !(dataConIdForConstant rt) [!(StgVarArg <$> mkBinderIdVar fc n (ALocal 6))] [])
                      ])
                ])
          ]

||| Creates a multilevel case statement block to unwrap/wrap the primitive values
||| around a two parameter STG primitive function call.
unaryPrimOp
  :  {auto _ : UniqueMapRef}
  -> {auto _ : Ref Counter Int}
  -> FC -> Core.Name.Name
  -> Constant -> StgOp -> Vect 1 AVar -> Constant
  -> Core SExpr
unaryPrimOp fc n ty op as rt = do
  [arg1] <- traverseVect (mkBinderIdVar fc n) as
  -- As we box everyting, and the result will be Lifted
  let resultType = SingleValue LiftedRep
  [primRep] <- constantToPrimRep ty
    | other => coreFail $ InternalError $ "Invalid PrimRep in unaryPrimOp: " ++ show ty
  let resultTypeName = Nothing
  pure $ StgCase
          (StgApp arg1 [] resultType)
          !(mkSBinderLocal fc n 3)
          !(AlgAlt <$> tyConIdForConstant ty)
          [ MkAlt !(AltDataCon <$> dataConIdForConstant ty) [!(mkSBinderLocal fc n 4)]
             (StgCase
                (StgOpApp op
                  [!(StgVarArg <$> mkBinderIdVar fc n (ALocal 4))]
                  resultType -- TODO: Unboxed PrimRep like Int16Rep
                  resultTypeName)
                !(mkSBinderLocal fc n 5)
                (PrimAlt primRep)
                [ MkAlt AltDefault []
                    (StgConApp !(dataConIdForConstant rt) [!(StgVarArg <$> mkBinderIdVar fc n (ALocal 5))] [])
                ])
          ]


compilePrimOp
  :  {auto _ : UniqueMapRef}
  -> {auto _ : Ref Counter Int}
  -> FC -> Core.Name.Name -> PrimFn arity -> Vect arity AVar
  -> Core SExpr
compilePrimOp {arity=2} fc n (Add ty) as = do
  op <- case ty of
    IntType     => pure $ StgPrimOp "+#"
    IntegerType => pure $ StgPrimOp "+#" -- TODO: No GMP Integer
    Bits8Type   => pure $ StgPrimOp "plusWord8#"
    Bits16Type  => pure $ StgPrimOp "plusWord16#"
    Bits32Type  => pure $ StgPrimOp "plusWord#" -- TODO
    Bits64Type  => pure $ StgPrimOp "plusWord#"
    DoubleType  => pure $ StgPrimOp "+##"
    _           => throw $ InternalError $ "Type is not for Add: " ++ show ty
  binPrimOp fc n ty op as ty

compilePrimOp {arity=2} fc n (Sub ty) as = do
  op <- case ty of
    IntType     => pure $ StgPrimOp "-#"
    IntegerType => pure $ StgPrimOp "-#" -- TODO: No GMP Integer
    Bits8Type   => pure $ StgPrimOp "subWord8#"
    Bits16Type  => pure $ StgPrimOp "subWord16#"
    Bits32Type  => pure $ StgPrimOp "subWord#" -- TODO
    Bits64Type  => pure $ StgPrimOp "subWord#"
    DoubleType  => pure $ StgPrimOp "-##"
    _           => throw $ InternalError $ "Type is not for Sub: " ++ show ty
  binPrimOp fc n ty op as ty

compilePrimOp {arity=2} fc n (Mul ty) as = do
  op <- case ty of
    IntType     => pure $ StgPrimOp "*#"
    IntegerType => pure $ StgPrimOp "*#" -- TODO: No GMP Integer
    Bits8Type   => pure $ StgPrimOp "timesWord8#"
    Bits16Type  => pure $ StgPrimOp "timesWord16#"
    Bits32Type  => pure $ StgPrimOp "timesWord#" -- TODO
    Bits64Type  => pure $ StgPrimOp "timesWord#"
    DoubleType  => pure $ StgPrimOp "*##"
    _           => throw $ InternalError $ "Type is not for Mul: " ++ show ty
  binPrimOp fc n ty op as ty

compilePrimOp {arity=2} fc n (Div ty) as = do
  op <- case ty of
    IntType     => pure $ StgPrimOp "quotInt#"
    IntegerType => pure $ StgPrimOp "quotInt#" -- TODO: No GMP Integer
    Bits8Type   => pure $ StgPrimOp "quotWord8#"
    Bits16Type  => pure $ StgPrimOp "quotWord16#"
    Bits32Type  => pure $ StgPrimOp "quotWord#" -- TODO
    Bits64Type  => pure $ StgPrimOp "quotWord#"
    DoubleType  => pure $ StgPrimOp "/##"
    _           => throw $ InternalError $ "Type is not for Div: " ++ show ty
  binPrimOp fc n ty op as ty

compilePrimOp {arity=2} fc n (Mod ty) as = do
  op <- case ty of
    IntType     => pure $ StgPrimOp "remInt#"
    IntegerType => pure $ StgPrimOp "remInt#" -- TODO: No GMP Integer
    Bits8Type   => pure $ StgPrimOp "remWord8#"
    Bits16Type  => pure $ StgPrimOp "remWord16#"
    Bits32Type  => pure $ StgPrimOp "remWord#" -- TODO
    Bits64Type  => pure $ StgPrimOp "remWord#"
    _           => throw $ InternalError $ "Type is not for Mod: " ++ show ty
  binPrimOp fc n ty op as ty

compilePrimOp {arity=1} fc n (Neg ty) as = do
  op <- case ty of
    IntType     => pure $ StgPrimOp "negateInt#"
    IntegerType => pure $ StgPrimOp "negateInt#" -- TODO: No GMP Integer
    DoubleType  => pure $ StgPrimOp "negateDouble#"
    _           => throw $ InternalError $ "Type is not for Div: " ++ show ty
  unaryPrimOp fc n ty op as ty

compilePrimOp {arity=2} fc n (ShiftL ty) as = do
  op <- case ty of
    IntType     => pure $ StgPrimOp "uncheckedIShiftL#"
    IntegerType => pure $ StgPrimOp "uncheckedIShiftL#" -- TODO: No GMP Integer
    Bits8Type   => throw $ InternalError $ "TODO: Needs parameter conversion for ShiftL: " ++ show ty
    Bits16Type  => throw $ InternalError $ "TODO: Needs parameter conversion for ShiftL: " ++ show ty
    Bits32Type  => throw $ InternalError $ "TODO: Needs parameter conversion for ShiftL: " ++ show ty
    Bits64Type  => throw $ InternalError $ "TODO: Needs parameter conversion for ShiftL: " ++ show ty
    _           => throw $ InternalError $ "Type is not for ShiftL: " ++ show ty
  binPrimOp fc n ty op as ty

compilePrimOp {arity=2} fc n (ShiftR ty) as = do
  op <- case ty of
    IntType     => pure $ StgPrimOp "uncheckedIShiftRL#"
    IntegerType => pure $ StgPrimOp "uncheckedIShiftRL#" -- TODO: No GMP Integer
    Bits8Type   => throw $ InternalError $ "TODO: Needs parameter conversion for ShiftR: " ++ show ty
    Bits16Type  => throw $ InternalError $ "TODO: Needs parameter conversion for ShiftR: " ++ show ty
    Bits32Type  => throw $ InternalError $ "TODO: Needs parameter conversion for ShiftR: " ++ show ty
    Bits64Type  => throw $ InternalError $ "TODO: Needs parameter conversion for ShiftR: " ++ show ty
    _           => throw $ InternalError $ "Type is not for ShiftR: " ++ show ty
  binPrimOp fc n ty op as ty

compilePrimOp {arity=2} fc n (BAnd ty) as = do
  op <- case ty of
    IntType     => pure $ StgPrimOp "andI#"
    IntegerType => pure $ StgPrimOp "andI#" -- TODO: No GMP Integer
    Bits8Type   => pure $ StgPrimOp "and#" -- TODO: This is defined for Word# not for Word8#. Same below...
    Bits16Type  => pure $ StgPrimOp "and#"
    Bits32Type  => pure $ StgPrimOp "and#" -- TODO
    Bits64Type  => pure $ StgPrimOp "and#"
    _           => throw $ InternalError $ "Type is not for BAnd: " ++ show ty
  binPrimOp fc n ty op as ty

compilePrimOp {arity=2} fc n (BOr ty) as = do
  op <- case ty of
    IntType     => pure $ StgPrimOp "orI#"
    IntegerType => pure $ StgPrimOp "orI#" -- TODO: No GMP Integer
    Bits8Type   => pure $ StgPrimOp "or#" -- TODO: This is defined for Word# not for Word8#. Same below...
    Bits16Type  => pure $ StgPrimOp "or#"
    Bits32Type  => pure $ StgPrimOp "or#" -- TODO
    Bits64Type  => pure $ StgPrimOp "or#"
    _           => throw $ InternalError $ "Type is not for BOr: " ++ show ty
  binPrimOp fc n ty op as ty

compilePrimOp {arity=2} fc n (BXOr ty) as = do
  op <- case ty of
    IntType     => pure $ StgPrimOp "xorI#"
    IntegerType => pure $ StgPrimOp "xorI#" -- TODO: No GMP Integer
    Bits8Type   => pure $ StgPrimOp "xor#" -- TODO: This is defined for Word# not for Word8#. Same below...
    Bits16Type  => pure $ StgPrimOp "xor#"
    Bits32Type  => pure $ StgPrimOp "xor#" -- TODO
    Bits64Type  => pure $ StgPrimOp "xor#"
    _           => throw $ InternalError $ "Type is not for BXOr: " ++ show ty
  binPrimOp fc n ty op as ty

compilePrimOp {arity=2} fc n (LT ty) as = do
  op <- case ty of
    IntType     => pure $ StgPrimOp "<#"
    IntegerType => pure $ StgPrimOp "<#" -- TODO: No GMP Integer
    Bits8Type   => pure $ StgPrimOp "ltWord8#"
    Bits16Type  => pure $ StgPrimOp "ltWord16#"
    Bits32Type  => pure $ StgPrimOp "ltWord#" -- TODO
    Bits64Type  => pure $ StgPrimOp "ltWord#"
    CharType    => pure $ StgPrimOp "ltWord8#"
    DoubleType  => pure $ StgPrimOp "<##"
    _           => throw $ InternalError $ "Type is not for LT: " ++ show ty
  binPrimOp fc n ty op as IntType

compilePrimOp {arity=2} fc n (LTE ty) as = do
  op <- case ty of
    IntType     => pure $ StgPrimOp "<=#"
    IntegerType => pure $ StgPrimOp "<=#" -- TODO: No GMP Integer
    Bits8Type   => pure $ StgPrimOp "leWord8#"
    Bits16Type  => pure $ StgPrimOp "leWord16#"
    Bits32Type  => pure $ StgPrimOp "leWord#" -- TODO
    Bits64Type  => pure $ StgPrimOp "leWord#"
    CharType    => pure $ StgPrimOp "leWord8#"
    DoubleType  => pure $ StgPrimOp "<=##"
    _           => throw $ InternalError $ "Type is not for LTE: " ++ show ty
  binPrimOp fc n ty op as IntType

compilePrimOp {arity=2} fc n (EQ ty) as = do
  op <- case ty of
    IntType     => pure $ StgPrimOp "==#"
    IntegerType => pure $ StgPrimOp "==#" -- TODO: No GMP Integer
    Bits8Type   => pure $ StgPrimOp "eqWord8#"
    Bits16Type  => pure $ StgPrimOp "eqWord16#"
    Bits32Type  => pure $ StgPrimOp "eqWord#" -- TODO
    Bits64Type  => pure $ StgPrimOp "eqWord#"
    CharType    => pure $ StgPrimOp "eqWord8#"
    DoubleType  => pure $ StgPrimOp "==##"
    _           => throw $ InternalError $ "Type is not for EQ: " ++ show ty
  binPrimOp fc n ty op as IntType

compilePrimOp {arity=2} fc n (GTE ty) as = do
  op <- case ty of
    IntType     => pure $ StgPrimOp ">=#"
    IntegerType => pure $ StgPrimOp ">=#" -- TODO: No GMP Integer
    Bits8Type   => pure $ StgPrimOp "geWord8#"
    Bits16Type  => pure $ StgPrimOp "geWord16#"
    Bits32Type  => pure $ StgPrimOp "geWord#" -- TODO
    Bits64Type  => pure $ StgPrimOp "geWord#"
    CharType    => pure $ StgPrimOp "geWord8#"
    DoubleType  => pure $ StgPrimOp ">=##"
    _           => throw $ InternalError $ "Type is not for GTE: " ++ show ty
  binPrimOp fc n ty op as IntType

compilePrimOp {arity=2} fc n (GT ty) as = do
  op <- case ty of
    IntType     => pure $ StgPrimOp ">#"
    IntegerType => pure $ StgPrimOp ">#" -- TODO: No GMP Integer
    Bits8Type   => pure $ StgPrimOp "gtWord8#"
    Bits16Type  => pure $ StgPrimOp "gtWord16#"
    Bits32Type  => pure $ StgPrimOp "gtWord#" -- TODO
    Bits64Type  => pure $ StgPrimOp "gtWord#"
    CharType    => pure $ StgPrimOp "gtWord8#"
    DoubleType  => pure $ StgPrimOp ">##"
    _           => throw $ InternalError $ "Type is not for GT: " ++ show ty
  binPrimOp fc n ty op as IntType

-- String is represented as a NULL ending byte array
-- What are the operations we need?
-- Allocate a byte array: newByteArray# ?
-- Index: readCharArray# ?
-- Write chars: writeCharArray# ?
-- Copy: copyMutableByteArray#

{-
String literals must be toplevel STG definitions. That are represented as Addr in STG.
copyAddrToByteArray# copies values from Addr to ByteArray
Idris string module that contains the STG definition of String Handling.
-}

-- Creating ByteArray, which returns a Pointer. AnyPtr in Idris?
--     Use ByteArray primops
--     https://github.com/grin-compiler/ghc-whole-program-compiler-project/blob/master/external-stg-interpreter/lib/Stg/Interpreter/PrimOp/ByteArray.hs
--     Char and WideChar
--     StrLength : PrimFn 1 - Start from the pointer and count the number of non-zero elements.
--     StrHead : PrimFn 1 - Index first element in the bytearray
--     StrTail : PrimFn 1 - Copy the content of the byte array, from index 1
--     StrIndex : PrimFn 2 - Index nth element in the bytearray.
--     StrCons : PrimFn 2 - Allocate a new bytearray which has length of n+1, copy first char, copy byte array
--     StrAppend : PrimFn 2 - Allocate a new bytearray which has length if n+m copy first, copy second
--     StrReverse : PrimFn 1 - allocate the same bytearray and copy values
--     StrSubstr : PrimFn 3 - Copy the content of the byte array

compilePrimOp {arity=1} fc n DoubleExp as = unaryPrimOp fc n DoubleType (StgPrimOp "expDouble#") as DoubleType
compilePrimOp {arity=1} fc n DoubleLog as = unaryPrimOp fc n DoubleType (StgPrimOp "logDouble#") as DoubleType
compilePrimOp {arity=1} fc n DoubleSin as = unaryPrimOp fc n DoubleType (StgPrimOp "sinDouble#") as DoubleType
compilePrimOp {arity=1} fc n DoubleCos as = unaryPrimOp fc n DoubleType (StgPrimOp "cosDouble#") as DoubleType
compilePrimOp {arity=1} fc n DoubleTan as = unaryPrimOp fc n DoubleType (StgPrimOp "tanDouble#") as DoubleType
compilePrimOp {arity=1} fc n DoubleASin as = unaryPrimOp fc n DoubleType (StgPrimOp "asinDouble#") as DoubleType
compilePrimOp {arity=1} fc n DoubleACos as = unaryPrimOp fc n DoubleType (StgPrimOp "acosDouble#") as DoubleType
compilePrimOp {arity=1} fc n DoubleATan as = unaryPrimOp fc n DoubleType (StgPrimOp "atanDouble#") as DoubleType
compilePrimOp {arity=1} fc n DoubleSqrt as = unaryPrimOp fc n DoubleType (StgPrimOp "sqrtDouble#") as DoubleType

--     DoubleFloor : PrimFn 1
--     DoubleCeiling : PrimFn 1
--     Cast : Constant -> Constant -> PrimFn 1 -- What is the semantics for this? Check in the official backend.

--     BelieveMe : PrimFn 3
compilePrimOp {arity=3} fc n BelieveMe [_,_,a] =
  pure (StgApp !(mkBinderIdVar fc n a) [] (SingleValue LiftedRep))

--     Crash : PrimFn 2 -- What are the parameters for this?
--     Use this FFI call to crash the haskell runtime.
--     https://github.com/grin-compiler/ghc-whole-program-compiler-project/blob/master/external-stg-interpreter/lib/Stg/Interpreter/FFI.hs#L178-L183
--     1b3f15ca69ea443031fa69a488c660a2c22182b8
compilePrimOp _ _ p as
  = pure
  $ StgLit
  $ LitString
  $ "compilePrimOp " ++ show p ++ " " ++ show as

-- TODO: Create ifthenelse chain for String literals
||| Compile constant for case alternative.
compileAltConstant : Constant -> Core Lit
compileAltConstant (I i)   = pure $ LitNumber LitNumInt $ cast i
compileAltConstant (BI i)  = pure $ LitNumber LitNumWord i
compileAltConstant (B8 i)  = pure $ LitNumber LitNumWord $ cast i
compileAltConstant (B16 i) = pure $ LitNumber LitNumWord $ cast i
compileAltConstant (B32 i) = pure $ LitNumber LitNumWord $ cast i
compileAltConstant (B64 i) = pure $ LitNumber LitNumWord64 i
-- compileAltConstant (Str s) = coreFail $ InternalError $ "Case alternative on Sring: " ++ show s -- pure $ LitString s
compileAltConstant (Str s) = pure $ LitString s
compileAltConstant (Ch c)  = pure $ LitChar c
compileAltConstant (Db d)  = pure $ LitDouble d
compileAltConstant c = coreFail $ InternalError $ "compileAltConstant " ++ show c

||| Compile constant for APrimVal, Boxing a value in STG.
compileConstant : Constant -> Core Lit
compileConstant (I i)   = pure $ LitNumber LitNumInt $ cast i
compileConstant (BI i)  = pure $ LitNumber LitNumWord i
compileConstant (B8 i)  = pure $ LitNumber LitNumWord $ cast i
compileConstant (B16 i) = pure $ LitNumber LitNumWord $ cast i
compileConstant (B32 i) = pure $ LitNumber LitNumWord $ cast i
compileConstant (B64 i) = pure $ LitNumber LitNumWord64 i
compileConstant (Str s) = pure $ LitString s
compileConstant (Ch c)  = pure $ LitChar c
compileConstant (Db d)  = pure $ LitDouble d
compileConstant c = coreFail $ InternalError $ "compileAltConstant " ++ show c


resolvedNameId
  :  {auto _ : Ref Ctxt Defs}
  -> Core.Name.Name
  -> Core Int
resolvedNameId n = do
  (Resolved r) <- toResolvedNames n
    | _ => coreFail $ InternalError $ "Name doesn't have resolved id: " ++ show n
  pure r

||| Names of the data constructors must be associated with their STyCons, this information
||| is needed for the code generator when we generate STG case expressions
namespace DataConstructorsToTypeDefinitions

  ||| Datatypes annotation for Ref
  export
  data ADTs : Type where

  ||| Associates the resolved names with the TyCon.
  export
  ADTMap : Type
  ADTMap = IntMap STyCon

  ||| Create the ADTs reference
  export
  mkADTs : Core (Ref ADTs ADTMap)
  mkADTs = newRef ADTs empty

  ||| Constructors annotation for Ref.
  data Cons : Type where

  ConsMap : Type
  ConsMap = IntMap Core.Name.Name

  mkConsMap : Core (Ref Cons ConsMap)
  mkConsMap = newRef Cons empty

  ||| Register the consturctor name for the STyCon
  export
  registerDataConToTyCon
    :  {auto _ : Ref ADTs ADTMap}
    -> {auto _ : Ref Ctxt Defs}
    -> STyCon
    -> Core.Name.Name
    -> Core ()
  registerDataConToTyCon s n = do
    r <- resolvedNameId n
    m <- get ADTs
    case lookup r m of
      Just st => coreFail
               $ InternalError
               $ show !(toFullNames n) ++ " is already registered for " ++ STyCon.Name st
      Nothing => do
        put ADTs (insert r s m)

  ||| Lookup if there is an ADT defined for the given name either for type name or data con name.
  export
  lookupTyCon
    :  {auto _ : Ref ADTs ADTMap}
    -> {auto _ : Ref Ctxt Defs}
    -> Core.Name.Name
    -> Core (Maybe STyCon)
  lookupTyCon n = lookup <$> resolvedNameId n <*> get ADTs



mutual
  compileANF
    :  {auto _ : UniqueMapRef}
    -> {auto _ : Ref Counter Int}
    -> {auto _ : Ref ADTs ADTMap}
    -> {auto _ : Ref Ctxt Defs}
    -> Core.Name.Name -> ANF
    -> Core SExpr
  compileANF funName (AV fc var)
    = pure $ StgApp !(mkBinderIdVar fc funName var) [] stgRepType

  compileANF funToCompile (AAppName fc funToCall args)
    = pure $ StgApp !(mkBinderIdName funToCall)
                    !(traverse (mkStgArg fc funToCompile) args)
                    stgRepType

  compileANF funToCompile (AUnderApp fc funToCall _ args)
    = pure $ StgApp !(mkBinderIdName funToCall)
                    !(traverse (mkStgArg fc funToCompile) args)
                    stgRepType

  compileANF funName (AApp fc closure arg)
    = pure $ StgApp !(mkBinderIdVar fc funName closure)
                    [!(mkStgArg fc funName arg)]
                    stgRepType

  compileANF funName (ALet fc var expr body) = do
    pure $ StgLet
      (StgNonRec
        !(mkSBinderLocal fc funName var)
        (StgRhsClosure Updatable [] !(compileANF funName expr)))
      !(compileANF funName body)

  -- TODO: Implement
  compileANF _ (ACon fc name tag args)
    -- Lookup the constructor based on the name/tag
    -- create an STG constructor and convert the arguments
    = pure $ StgLit $ LitString $ "ACon" ++ show tag ++ " " ++ show args

  compileANF funName (AOp fc prim args)
    = compilePrimOp fc funName prim args

  -- TODO: Implement
  compileANF _ (AExtPrim _ name args)
    = pure $ StgLit $ LitString $ "AExtPrim " ++ show name ++ " " ++ show args

  compileANF funName (AConCase fc scrutinee alts mdef) = do
    -- Compute the alt-type
    altType <- do
      -- Lookup the STyCon definition from the alt names
      namesAndTyCons
        <- traverse
            (\(MkAConAlt name tag args body) => map (name,) (lookupTyCon name))
            alts
      -- Check if there is exactly one STyCon definition is found
      [] <- pure $ mapMaybe
                    (\(n,x) => case x of { Nothing => Just n; _ => Nothing})
                    namesAndTyCons
        | nonDefinedConstructors => coreFail $ InternalError $ unlines $
            "Constructors not having type information: " :: map show nonDefinedConstructors
      tyCon <- case mapMaybe snd namesAndTyCons of
                []  => coreFail $ InternalError $ "No type constructor is found for: " ++ show fc
                [t] => pure $ STyCon.Id t
                ts  => case nub (map STyCon.Id ts) of
                         []  => coreFail $ InternalError "Impossible case in compile AConCase"
                         [t] => pure t
                         ts  => coreFail $ InternalError $ "More than TyCon found for: " ++ show fc
      -- Use the STyCon definition in the altType
      pure $ AlgAlt tyCon
    scrutBinder <- mkBinderIdVar fc funName scrutinee
    let stgScrutinee = StgApp scrutBinder [] stgRepType
    binder <- mkSBinderVar fc funName scrutinee
    stgDefAlt <- maybe
      (pure [])
      (\x => do
        stgBody <- compileANF funName x
        pure [MkAlt AltDefault [] stgBody])
      mdef
    stgAlts <- traverse (compileConAlt fc funName) alts
    pure $ StgCase stgScrutinee binder altType (stgDefAlt ++ stgAlts)

  compileANF funName (AConstCase fc scrutinee alts mdef) = do
    -- TODO: Unbox with case and match on primitves with the according representation.
    let altType = PrimAlt UnliftedRep -- Question: Is this the right reptype?
    scrutBinder <- mkBinderIdVar fc funName scrutinee
    let stgScrutinee = StgApp scrutBinder [] stgRepType
    binder <- mkSBinderVar fc funName scrutinee
    stgDefAlt <- maybe
      (pure [])
      (\x => do
        stgBody <- compileANF funName x
        pure [MkAlt AltDefault [] stgBody])
      mdef
    stgAlts <- traverse (compileConstAlt funName) alts
    pure $ StgCase stgScrutinee binder altType (stgDefAlt ++ stgAlts)

  compileANF _ (APrimVal _ c)
   = StgConApp
      <$> dataConIdForValueConstant c
          -- TODO: Make this mapping safer with indexed type
      <*> (traverse (map StgLitArg . compileConstant)
                    (case c of { WorldVal => [] ; other => [other] }))
      <*> (pure [])

  -- TODO: Implement: Fix toplevel binder with one constructor
  compileANF _ (AErased _)
    = pure $ StgApp (MkBinderId !(uniqueForTerm ERASED_TOPLEVEL_NAME)) [] (SingleValue LiftedRep)

  -- TODO: Implement: Use Crash primop. errorBlech2 for reporting error ("%s", msg)
  compileANF _ (ACrash _ msg)
    = pure $ StgLit $ LitString $ "ACrash " ++ msg

  compileConAlt
    :  {auto _ : UniqueMapRef}
    -> {auto _ : Ref Counter Int}
    -> {auto _ : Ref ADTs ADTMap}
    -> {auto _ : Ref Ctxt Defs}
    -> FC -> Core.Name.Name -> AConAlt
    -> Core SAlt
  compileConAlt fc funName c@(MkAConAlt name tag args body) = do
    stgArgs     <- traverse (mkSBinderLocal fc funName) args
    stgBody     <- compileANF funName body
    stgDataCon  <- compileDataConId name
    pure $ MkAlt (AltDataCon stgDataCon) stgArgs stgBody

  compileConstAlt
    :  {auto _ : UniqueMapRef}
    -> {auto _ : Ref Counter Int}
    -> {auto _ : Ref ADTs ADTMap}
    -> {auto _ : Ref Ctxt Defs}
    -> Core.Name.Name -> AConstAlt
    -> Core SAlt
  compileConstAlt funName (MkAConstAlt constant body) = do
    stgBody <- compileANF funName body
    lit <- compileAltConstant constant
    pure $ MkAlt (AltLit lit) [] stgBody

erasedTopBinding
  :  {auto _ : UniqueMapRef}
  -> {auto _ : Ref Counter Int}
  -> Core STopBinding
erasedTopBinding = do
  dtc <- MkDataConId <$> uniqueForTerm ERASED_CON_NAME
  rhs <- pure $ StgRhsCon dtc []
  erasedNameBinder <- mkSBinderStr emptyFC ERASED_TOPLEVEL_NAME
  binding <- pure $ StgNonRec erasedNameBinder rhs
  pure $ StgTopLifted binding


compileTopBinding
  :  {auto _ : UniqueMapRef}
  -> {auto _ : Ref Counter Int}
  -> {auto _ : Ref Ctxt Defs}
  -> {auto _ : Ref ADTs ADTMap}
  -> (Core.Name.Name, ANFDef)
  -> Core (Maybe STopBinding)
compileTopBinding (funName,MkAFun args body) = do
--  coreLift $ putStrLn $ "Compiling: " ++ show funName
  funBody       <- compileANF funName body
  funArguments  <- traverse (mkSBinderLocal emptyFC funName) args
  funNameBinder <- mkSBinderName emptyFC funName
  rhs           <- pure $ StgRhsClosure ReEntrant funArguments funBody
  -- Question: Is Reentrant OK here?
  binding       <- pure $ StgNonRec funNameBinder rhs
  -- Question: Is non-recursice good here? Test it.
  pure $ Just $ StgTopLifted binding
compileTopBinding (name,con@(MkACon tag arity)) =
  -- Covered in the LearnDataTypes section
  pure Nothing
compileTopBinding (name,MkAForeign css fargs rtype) = do
  coreLift $ putStrLn $ "Skipping foreign: " ++ show name
  pure Nothing
compileTopBinding (name,MkAError body) = do
  coreLift $ putStrLn $ "Skipping error: " ++ show name
  pure Nothing



||| Global definition mapping for types and data constructors.
namespace TConsAndDCons

  ||| Label for TAC reference
  data TAC : Type where

  ||| Types and data constructors
  record TyAndCnstrs where
    constructor MkTyAndCnstrs
    Types : IntMap GlobalDef
    Cntrs : IntMap GlobalDef

  mkTACRef : Core (Ref TAC TyAndCnstrs)
  mkTACRef = newRef TAC (MkTyAndCnstrs empty empty)

  addTypeOrCnst
    :  {auto _ : Ref TAC TyAndCnstrs}
    -> {auto _ : Ref Ctxt Defs}
    -> GlobalDef
    -> Core ()
  addTypeOrCnst g = case definition g of
    TCon _ _ _ _ _ _ _ _ => do
      r <- resolvedNameId (fullname g)
      MkTyAndCnstrs t c <- get TAC
      -- TODO: Check if resolved named already in
      let t1 = insert r g t
      put TAC (MkTyAndCnstrs t1 c)
    DCon _ _ _ => do
      r <- resolvedNameId (fullname g)
      MkTyAndCnstrs t c <- get TAC
      let c1 = insert r g c
      put TAC (MkTyAndCnstrs t c1)
    _ => pure ()

  number : List a -> List (Int, a)
  number = iter 0 []
    where
      iter : Int -> List (Int, a) -> List a -> List (Int, a)
      iter n acc []      = reverse acc
      iter n acc (x::xs) = iter (n+1) ((n,x)::acc) xs

  ||| Learn the TCons and DCons from the Defs context.
  ||| This is a helper to define datatypes
  learnTConsAndCons
    :  {auto _ : Ref Ctxt Defs}
    -> {auto _ : Ref TAC TyAndCnstrs}
    -> Core ()
  learnTConsAndCons = do
    context <- gamma <$> get Ctxt
    traverse_
      (\n => do
        mdef <- lookupCtxtExactI n context
        case mdef of
          Nothing => pure ()
          Just (_, def) => addTypeOrCnst def)
      !(allNames context)

  ||| Create an SDataCon for STyCon when creating the type definitions for STG.
  createSTGDataCon
    :  {auto _ : UniqueMapRef}
    -> {auto _ : Ref Counter Int}
    -> GlobalDef
    -> Core SDataCon
  createSTGDataCon g = do
    let fullName = fullname g
    (DCon _ arity _) <- pure $ definition g
      | other => coreFail $ InternalError $ "createSTGDataCon found other than DCon: " ++ show other
    let arity' : Int = (cast arity) - cast (length (eraseArgs g))
    if arity' < 0
      then coreFail $ InternalError $ unlines
            [ "Negative arity after erased arguments:"
            , show fullName
            , "Full arity: " ++ show arity
            , "Erased arity: " ++ show arity'
            , "Erasable arguments: " ++ show (eraseArgs g)
            ]
      else pure $ MkSDataCon
                    (show fullName)
                    !(compileDataConId fullName)
                    (AlgDataCon (replicate (fromInteger (cast arity')) LiftedRep))
                    !(mkSBinder emptyFC True ("mk" ++ show fullName))
                    (mkSrcSpan (location g))

  createSTGTyCon
    :  {auto _ : UniqueMapRef}
    -> {auto _ : Ref Counter Int}
    -> List SDataCon
    -> GlobalDef
    -> Core STyCon
  createSTGTyCon stgDataCons g = do
    let fullName = fullname g
    (TCon _ _ _ _ _ _ _ _) <- pure $ definition g
      | other => coreFail $ InternalError $ "createSTGTyCon found other than TCon: " ++ show other
    pure $ MkSTyCon
      (show fullName)
      (MkTyConId !(uniqueForType (show fullName))) -- TODO: Where else do we use STG type names?
      stgDataCons
      (mkSrcSpan (location g))

  ||| Compiles the learn TCons and their DCons
  defineDataTypes
    :  {auto _ : UniqueMapRef}
    -> {auto _ : Ref Counter Int}
    -> {auto _ : Ref Ctxt Defs}
    -> {auto _ : DataTypeMapRef}
    -> {auto _ : Ref ADTs ADTMap}
    -> {auto _ : Ref TAC TyAndCnstrs}
    -> Core ()
  defineDataTypes = do
    -- Define all the STyCon for the TCon we found, removing the used TCons and DCons along the way.
    -- Keeping the DCons which does not have TCons, for those we should create their own STyCons
    -- There are mainly typeclass dictionary instances.
    MkTyAndCnstrs types constructors <- get TAC
    -- TODO: Write check if all the constructors are used.
    traverse_
      (\(r, g) =>
        case definition g of
          -- TODO: Add datatype with constructors and remove the constructors from the cnstrs list
          def@(TCon _ parampos detpos _ _ _ datacons _) => do
            -- Create DataCons, looking up resolved IDs
            resolveds <- traverse resolvedNameId datacons
            datacons <- traverse
                (\rd => case lookup rd constructors of
                  Nothing => coreFail
                           $ InternalError
                           $ "defineDatatypes: Data constructor is not found: "
                              ++ show !(toFullNames (Resolved rd))
                  Just dg => createSTGDataCon dg)
              resolveds
            -- Create TyCon and attach DataCons
            sTyCon <- createSTGTyCon datacons g
            traverse (registerDataConToTyCon sTyCon . Resolved) resolveds
            defineDataType (MkUnitId MAIN_UNIT) (MkModuleName MAIN_MODULE) sTyCon
            pure ()
          _ => pure ())
      (toList types)

  ||| Create STG datatypes from filtering out the TCon and DCon definitions from Defs
  createDataTypes
    :  {auto _ : UniqueMapRef}
    -> {auto _ : Ref Counter Int}
    -> {auto _ : Ref Ctxt Defs}
    -> {auto _ : DataTypeMapRef}
    -> {auto _ : Ref ADTs ADTMap}
    -> Core ()
  createDataTypes = do
    tac <- mkTACRef
    learnTConsAndCons
    defineDataTypes

-- We compile only one enormous module
export
compileModule
  :  {auto _ : UniqueMapRef}
  -> {auto _ : Ref Counter Int}
  -> {auto _ : DataTypeMapRef}
  -> {auto _ : Ref Ctxt Defs}
  -> List (Core.Name.Name, ANFDef)
  -> Core SModule
compileModule anfDefs = do
  adts <- mkADTs
  definePrimitiveDataTypes
  defineErasedADT
  createDataTypes
  let phase              = "Main"
  let moduleUnitId       = MkUnitId "MainUnit"
  let name               = MkModuleName "Main" -- : ModuleName
  let sourceFilePath     = "some.idr" -- : String
  let foreignStubs       = NoStubs -- : ForeignStubs -- ???
  let hasForeignExported = False -- : Bool
  let dependency         = [] -- : List (UnitId, List ModuleName)
  let externalTopIds     = [] -- : List (UnitId, List (ModuleName, List idBnd))
  erasedTopLevel         <- erasedTopBinding
  compiledTopBindings    <- mapMaybe id <$> traverse compileTopBinding anfDefs
  let topBindings        = erasedTopLevel :: compiledTopBindings
  tyCons                 <- getDefinedDataTypes -- : List (UnitId, List (ModuleName, List tcBnd))
  let foreignFiles       = [] -- : List (ForeignSrcLang, FilePath)
  pure $ MkModule
    phase
    moduleUnitId
    name
    sourceFilePath
    foreignStubs
    hasForeignExported
    dependency
    externalTopIds
    tyCons
    topBindings
    foreignFiles

{-
data AVar : Type where
     ALocal : Int -> AVar
     ANull : AVar -- Erased variable

data ANF where
    -- Reference a variable
    AV : FC -> AVar -> ANF
    | StgApp with zero argument, it works on local variables.

    -- Apply a function to the list of arguments
    AAppName : FC -> Name -> List AVar -> ANF
    -- StgApp

    -- Function application with less arguments than needed.
    AUnderApp : FC -> Name -> (missing : Nat) -> (args : List AVar) -> ANF
    -- StgApp ???
    -- Let underapp, RhsClosure

    -- Apply a closure to an argument
    AApp : FC -> (closure : AVar) -> (arg : AVar) -> ANF
    -- StgApp ???

    -- Create a let binding
    ALet : FC -> (var : Int) -> ANF -> ANF -> ANF
    -- StgLet
    -- StgLetNoEscape
    = StgNonRec idBnd (Rhs' idBnd idOcc dcOcc tcOcc)
    | StgRec    (List (idBnd, Rhs' idBnd idOcc dcOcc tcOcc))
    = StgRhsClosure UpdateFlag (List idBnd) (Expr' idBnd idOcc dcOcc tcOcc)
    | StgRhsCon dcOcc (List (Arg' idOcc))
    -- Write a recursive let example and investigate its IR: No recursive lets are allowed.

    -- Create a con value. -- TODO: What is the tag parameter?
    ACon : FC -> Name -> (tag : Maybe Int) -> List AVar -> ANF
    -- StgConApp: how to add types

    -- Apply a primitive to some arguments
    AOp : FC -> PrimFn arity -> Vect arity AVar -> ANF
    -- StgOpApp

    -- Apply an external primitive to some arguments
    AExtPrim : FC -> Name -> List AVar -> ANF
    -- StgOpApp
    -- StgFCallOp

    -- Case expression that matches some Con values
    AConCase : FC -> AVar -> List AConAlt -> Maybe ANF -> ANF
    | StgCase expr idBnd alttype alts

    -- Case expression that matches some constant values
    AConstCase : FC -> AVar -> List AConstAlt -> Maybe ANF -> ANF
    | StgCase expr idBnd alttype alts
    | Simple values represented as our boxed types

    -- Create a primitive value
    APrimVal : FC -> Constant -> ANF
    | StgLit Lit
    | We need to box the simple values

    -- Erased ANF
    AErased : FC -> ANF
    | Represent as (StgLit LitNullAddr) ?
    | Generate any trash

    -- Runtime error
    ACrash : FC -> String -> ANF
    | Represent as StgApp error?
    | Impossbile Or not?
    | There is a primop which does that

  public export
  data AConAlt : Type where
       MkAConAlt : Name -> (tag : Maybe Int) -> (args : List Int) ->
                   ANF -> AConAlt

  record Alt' (idBnd : Type) (idOcc : Type) (dcOcc : Type) (tcOcc : Type) where
    constructor MkAlt
    Con     : AltCon' dcOcc
    Binders : List idBnd
    RHS     : Expr' idBnd idOcc dcOcc tcOcc
  data AltCon' dcOcc
    = AltDataCon dcOcc
    | AltLit     Lit
    | AltDefault

  public export
  data AConstAlt : Type where
       MkAConstAlt : Constant -> ANF -> AConstAlt

  record Alt' (idBnd : Type) (idOcc : Type) (dcOcc : Type) (tcOcc : Type) where
    constructor MkAlt
    Con     : AltCon' dcOcc
    Binders : List idBnd
    RHS     : Expr' idBnd idOcc dcOcc tcOcc
  data AltCon' dcOcc
    = AltDataCon dcOcc
    | AltLit     Lit
    | AltDefault

public export
data ANFDef : Type where
  MkAFun : (args : List Int) -> ANF -> ANFDef
  MkACon : (tag : Maybe Int) -> (arity : Nat) -> ANFDef
  MkAForeign : (ccs : List String) -> (fargs : List CFType) ->
              CFType -> ANFDef
  MkAError : ANF -> ANFDef

public export
record CompileData where
  constructor MkCompileData
  mainExpr : CExp [] -- main expression to execute. This also appears in
                     -- the definitions below as MN "__mainExpression" 0
  namedDefs : List (Name, FC, NamedDef)
  lambdaLifted : List (Name, LiftedDef)
       -- ^ lambda lifted definitions, if required. Only the top level names
       -- will be in the context, and (for the moment...) I don't expect to
       -- need to look anything up, so it's just an alist.
  anf : List (Name, ANFDef)
       -- ^ lambda lifted and converted to ANF (all arguments to functions
       -- and constructors transformed to either variables or Null if erased)
  vmcode : List (Name, VMDef)
       -- ^ A much simplified virtual machine code, suitable for passing
       -- to a more low level target such as C

RepType: How doubles are represented? Write an example program: Boxed vs Unboxed
-}

{-
https://gitlab.haskell.org/ghc/ghc/-/blob/master/compiler/GHC/Builtin/primops.txt.pp
-}

{-
How to represent String and idris FFI calls?
-}
