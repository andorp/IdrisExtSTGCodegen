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
 * TODO: Write about ADT mapping
 * ...

TODOs
[+] Remove (Name, Name, Name) parameter from StgApp
[+] Add FC information to binders
[ ] Implement Erased values, erased variables
[ ] Implement Crash primitive
[ ] Handle primitive case matches accordingly
[ ] Generate STG main entry
[ ] Handle String matches with ifthenelse chains, using stringEq primop from STG
    - Create a test program which reads from input.
[ ] Implement primitive operations
[.] Write DataCon -> TypeCon association
[ ] Separate STG Type and Term namespaces
[ ] FFI calls AExtPrim
    - Create a test program which FFI calls into a library.
[ ] Module compilation
[ ] ...
-}

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
  UniqueMap = StringMap Unique

  export
  mkUniques : Core (Ref Uniques UniqueMap)
  mkUniques = newRef Uniques empty

  export
  mkUnique : {auto _ : Ref Counter Int} -> Core Unique
  mkUnique = do
    x <- get Counter
    let u = MkUnique 'i' x
    put Counter (x + 1)
    pure u

  export
  getUnique
    :  {auto _ : Ref Uniques UniqueMap}
    -> {auto _ : Ref Counter Int}
    -> String
    -> Core Unique
  getUnique name = do
    x <- get Uniques
    case lookup name x of
      Nothing => do
        u <- mkUnique
        put Uniques (insert name u x)
        pure u
      Just u => do
        pure u


namespace DataTypes

  ||| DataType annotation for auto Refs to store the defined STG datatypes during compilation.
  export
  data DataTypes : Type where

  ||| Defined datatypes in STG during the compilation of the module.
  export
  DataTypeMap : Type
  DataTypeMap = StringMap {-UnitId-} (StringMap {-ModuleName-} (List STyCon))

  ||| Create the Reference that holds the DataTypeMap
  export
  mkDataTypes : Core (Ref DataTypes DataTypeMap)
  mkDataTypes = newRef DataTypes empty

  addDataType : UnitId -> ModuleName -> STyCon -> DataTypeMap -> DataTypeMap
  addDataType (MkUnitId u) (MkModuleName m) s = merge (singleton u (singleton m [s]))

  dataTypeList : DataTypeMap -> List (UnitId, List (ModuleName, List STyCon))
  dataTypeList = map (mapFst MkUnitId) . Data.StringMap.toList . map (map (mapFst MkModuleName) . Data.StringMap.toList)

  export
  defineDataType : {auto _ : Ref DataTypes DataTypeMap} -> UnitId -> ModuleName -> STyCon -> Core ()
  defineDataType u m s = do
    x <- get DataTypes
    put DataTypes (addDataType u m s x)

  export
  getDefinedDataTypes : {auto _ : Ref DataTypes DataTypeMap} -> Core (List (UnitId, List (ModuleName, List STyCon)))
  getDefinedDataTypes = map dataTypeList $ get DataTypes



stgRepType : RepType
stgRepType = SingleValue UnliftedRep

mkSrcSpan : FC -> SrcSpan
mkSrcSpan (MkFC file (sl,sc) (el,ec)) = SsRealSrcSpan (MkRealSrcSpan file sl sc el ec) Nothing
mkSrcSpan EmptyFC                     = SsUnhelpfulSpan "<no location>"

mkSBinder
  :  {auto _ : Ref Uniques UniqueMap}
  -> {auto _ : Ref Counter Int}
  -> FC -> Bool -> String
  -> Core SBinder
mkSBinder fc topLevel binderName = do
  binderId <- MkBinderId <$> getUnique binderName
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
  :  {auto _ : Ref Uniques UniqueMap}
  -> {auto _ : Ref Counter Int}
  -> FC -> String -- TODO : Constant
  -> Core SBinder
mkSBinderTyCon fc = mkSBinder fc False

mkSBinderLocal
  :  {auto _ : Ref Uniques UniqueMap}
  -> {auto _ : Ref Counter Int}
  -> FC -> Core.Name.Name -> Int
  -> Core SBinder
mkSBinderLocal f n x = mkSBinder f False (show n ++ ":" ++ show x)

mkSBinderName
  :  {auto _ : Ref Uniques UniqueMap}
  -> {auto _ : Ref Counter Int}
  -> FC -> Core.Name.Name
  -> Core SBinder
mkSBinderName f n = mkSBinder f True $ show n

mkSBinderStr
  :  {auto _ : Ref Uniques UniqueMap}
  -> {auto _ : Ref Counter Int}
  -> FC -> String
  -> Core SBinder
mkSBinderStr fc = mkSBinder fc True

mkSBinderVar
  :  {auto _ : Ref Uniques UniqueMap}
  -> {auto _ : Ref Counter Int}
  -> FC -> Core.Name.Name -> AVar
  -> Core SBinder
mkSBinderVar fc n (ALocal x) = mkSBinder fc False (show n ++ ":" ++ show x)
mkSBinderVar fc n ANull      = coreFail $ InternalError $ "mkSBinderVar " ++ show fc ++ " " ++ show n ++ " ANull"

mkBinderIdVar
  :  {auto _ : Ref Uniques UniqueMap}
  -> {auto _ : Ref Counter Int}
  -> FC -> Core.Name.Name -> AVar
  -> Core BinderId
mkBinderIdVar fc n (ALocal x) = MkBinderId <$> getUnique (show n ++ ":" ++ show x)
mkBinderIdVar fc n ANull      = coreFail $ InternalError $ "mkBinderIdVar " ++ show fc ++ " " ++ show n ++ " ANull"

||| Create a StdVarArg for the Argument of a function application.
|||
||| If the argument is ANull/erased, then it returns a NulAddr literal
mkStgArg
  :  {auto _ : Ref Uniques UniqueMap}
  -> {auto _ : Ref Counter Int}
  -> FC -> Core.Name.Name -> AVar
  -> Core SArg
mkStgArg fc n a@(ALocal _) = StgVarArg <$> mkBinderIdVar fc n a
mkStgArg _  _ ANull        = pure $ StgLitArg $ LitNullAddr
-- Question: Is that a right value for erased argument?
-- Answer: This is not right, this should be Lifted. Make a global erased value, with its binder
--         that is referred here.

mkBinderIdName
  :  {auto _ : Ref Uniques UniqueMap}
  -> {auto _ : Ref Counter Int}
  -> Core.Name.Name
  -> Core BinderId
mkBinderIdName = map MkBinderId . getUnique . show

compileDataConId
  :  {auto _ : Ref Uniques UniqueMap}
  -> {auto _ : Ref Counter Int}
  -> Maybe Int -- What does Nothing mean here for DataConId
  -> Core DataConId
compileDataConId Nothing  = coreFail $ UserError "MkDataConId <$> mkUnique"
compileDataConId (Just t) = MkDataConId <$> getUnique ("DataCon:" ++ show t)

||| RepType when the constant is compiled to a boxed value, behind a DataCon.
||| TODO: Refactor to use Core
constantToTypeRep : Constant -> Core RepType
constantToTypeRep IntType     = pure $ SingleValue LiftedRep
constantToTypeRep IntegerType = pure $ SingleValue LiftedRep
constantToTypeRep Bits8Type   = pure $ SingleValue LiftedRep
constantToTypeRep Bits16Type  = pure $ SingleValue LiftedRep
constantToTypeRep Bits32Type  = pure $ SingleValue LiftedRep
constantToTypeRep Bits64Type  = pure $ SingleValue LiftedRep
constantToTypeRep StringType  = pure $ SingleValue LiftedRep
constantToTypeRep CharType    = pure $ SingleValue LiftedRep
constantToTypeRep DoubleType  = pure $ SingleValue LiftedRep
constantToTypeRep other = coreFail $ InternalError $ "No TypeRep for " ++ show other

||| PrimType when the constant is compiled insides the box.
||| TODO: Refactor to use Core
constantToPrimRep : Constant -> Core PrimRep
constantToPrimRep IntType     = pure IntRep
constantToPrimRep IntegerType = pure IntRep -- TODO: This is not the right representation for integer
constantToPrimRep Bits8Type   = pure Word8Rep
constantToPrimRep Bits16Type  = pure Word16Rep
constantToPrimRep Bits32Type  = pure Word32Rep
constantToPrimRep Bits64Type  = pure Word64Rep
constantToPrimRep DoubleType  = pure DoubleRep
constantToPrimRep StringType  = pure AddrRep
constantToPrimRep other = coreFail $ InternalError $ "No PrimRep for " ++ show other

||| Create a TyConId for the given idris primtive type.
tyConIdForConstant
  :  {auto _ : Ref Uniques UniqueMap}
  -> {auto _ : Ref Counter Int}
  -> Constant
  -> Core TyConId
tyConIdForConstant IntType     = MkTyConId <$> getUnique "type:IdrInt"
tyConIdForConstant IntegerType = MkTyConId <$> getUnique "type:IdrInteger"
tyConIdForConstant Bits8Type   = MkTyConId <$> getUnique "type:IdrBits8"
tyConIdForConstant Bits16Type  = MkTyConId <$> getUnique "type:IdrBits16"
tyConIdForConstant Bits32Type  = MkTyConId <$> getUnique "type:IdrBits32"
tyConIdForConstant Bits64Type  = MkTyConId <$> getUnique "type:IdrBits64"
tyConIdForConstant StringType  = MkTyConId <$> getUnique "type:IdrString"
tyConIdForConstant CharType    = MkTyConId <$> getUnique "type:IdrChar"
tyConIdForConstant DoubleType  = MkTyConId <$> getUnique "type:IdrDouble"
tyConIdForConstant WorldType   = MkTyConId <$> getUnique "type:IdrWorld"
tyConIdForConstant other = coreFail $ UserError $ "No type constructor for " ++ show other

dataConIdForConstant
  :  {auto _ : Ref Uniques UniqueMap}
  -> {auto _ : Ref Counter Int}
  -> Constant
  -> Core DataConId
dataConIdForConstant IntType     = MkDataConId <$> getUnique "IdrInt"
dataConIdForConstant IntegerType = MkDataConId <$> getUnique "IdrInteger"
dataConIdForConstant Bits8Type   = MkDataConId <$> getUnique "IdrBits8"
dataConIdForConstant Bits16Type  = MkDataConId <$> getUnique "IdrBits16"
dataConIdForConstant Bits32Type  = MkDataConId <$> getUnique "IdrBits32"
dataConIdForConstant Bits64Type  = MkDataConId <$> getUnique "IdrBits64"
dataConIdForConstant StringType  = MkDataConId <$> getUnique "IdrString"
dataConIdForConstant CharType    = MkDataConId <$> getUnique "IdrChar"
dataConIdForConstant DoubleType  = MkDataConId <$> getUnique "IdrDouble"
dataConIdForConstant WorldType   = MkDataConId <$> getUnique "IdrWorld"
dataConIdForConstant other = coreFail $ UserError $ "No data constructor for " ++ show other

dataConIdForValueConstant
  :  {auto _ : Ref Uniques UniqueMap}
  -> {auto _ : Ref Counter Int}
  -> Constant
  -> Core DataConId
dataConIdForValueConstant (I _)    = MkDataConId <$> getUnique "IdrInt"
dataConIdForValueConstant (BI _)   = MkDataConId <$> getUnique "IdrInteger"
dataConIdForValueConstant (B8 _)   = MkDataConId <$> getUnique "IdrBits8"
dataConIdForValueConstant (B16 _)  = MkDataConId <$> getUnique "IdrBits16"
dataConIdForValueConstant (B32 _)  = MkDataConId <$> getUnique "IdrBits32"
dataConIdForValueConstant (B64 _)  = MkDataConId <$> getUnique "IdrBits32"
dataConIdForValueConstant (Str _)  = MkDataConId <$> getUnique "IdrString"
dataConIdForValueConstant (Ch _)   = MkDataConId <$> getUnique "IdrChar"
dataConIdForValueConstant (Db _)   = MkDataConId <$> getUnique "IdrDouble"
dataConIdForValueConstant WorldVal = MkDataConId <$> getUnique "IdrWorld"
dataConIdForValueConstant other   = coreFail $ InternalError $ "dataConIdForValueConstant " ++ show other

definePrimitiveDataType
  :  {auto _ : Ref Uniques UniqueMap}
  -> {auto _ : Ref Counter Int}
  -> {auto _ : Ref DataTypes DataTypeMap}
  -> (String, String, String, String, List PrimRep)
  -> Core ()
definePrimitiveDataType (u, m, t, c, fs) = do
  d <- pure $ MkSTyCon t (MkTyConId !(getUnique ("type:" ++ t)))
                         [ MkSDataCon c (MkDataConId !(getUnique c))
                                        (AlgDataCon fs)
                                        !(mkSBinderStr emptyFC ("mk" ++ t))
                                        (SsUnhelpfulSpan "<no location>") ]
                         (SsUnhelpfulSpan "<no location>")
  defineDataType (MkUnitId u) (MkModuleName m) d

||| Create the primitive types section in the STG module.
|||
||| Idris primitive types are represented as boxed values in STG, with a datatype with one constructor.
||| Eg: data IdrInt = IdrInt #IntRep
definePrimitiveDataTypes
  :  {auto _ : Ref Uniques UniqueMap}
  -> {auto _ : Ref Counter Int}
  -> {auto _ : Ref DataTypes DataTypeMap}
  -> Core ()
definePrimitiveDataTypes = traverse_ definePrimitiveDataType
 [ ("u", "m", "IdrInt"    , "IdrInt"    , [IntRep])
 , ("u", "m", "IdrInteger", "IdrInteger", [IntRep]) -- TODO: This is bad, GMP Integer is needed here
 , ("u", "m", "IdrBits8"  , "IdrBits8"  , [Word8Rep])
 , ("u", "m", "IdrBits16" , "IdrBits16" , [Word16Rep])
 , ("u", "m", "IdrBits32" , "IdrBits32" , [Word32Rep])
 , ("u", "m", "IdrBits64" , "IdrBits64" , [Word64Rep])
 , ("u", "m", "IdrChar"   , "IdrChar"   , [Word8Rep])
 , ("u", "m", "IdrDouble" , "IdrDouble" , [DoubleRep])
 , ("u", "m", "IdrString" , "IdrString" , [AddrRep])
 , ("u", "m", "IdrWorld"  , "IdrWorld"  , [])
 ]
-- ^^ TODO: Unit and module IDs should come from the following table, otherwise FFI wont work in GHC.
-- https://github.com/grin-compiler/ghc-whole-program-compiler-project/blob/master/external-stg-interpreter/lib/Stg/Interpreter/Rts.hs#L43-L61
-- 1b3f15ca69ea443031fa69a488c660a2c22182b8

compilePrimOp
  :  {auto _ : Ref Uniques UniqueMap}
  -> {auto _ : Ref Counter Int}
  -> FC -> Core.Name.Name -> PrimFn arity -> Vect arity AVar
  -> Core SExpr
compilePrimOp {arity=2} fc n (Add ty) as = do
  op <- case ty of
    IntType     => pure $ StgPrimOp "+#"
    IntegerType => pure $ StgPrimOp "Add IntegerType" -- TODO: No GMP Integer
    Bits8Type   => pure $ StgPrimOp "plusWord#"
    Bits16Type  => pure $ StgPrimOp "plusWord#"
    Bits32Type  => pure $ StgPrimOp "plusWord#"
    Bits64Type  => pure $ StgPrimOp "plusWord#"
    DoubleType  => pure $ StgPrimOp "+##"
    _           => throw $ InternalError $ "Type is not for adding: " ++ show ty
  [arg1, arg2] <- traverseVect (mkBinderIdVar fc n) as
  resultType <- constantToTypeRep ty
  primRep <- constantToPrimRep ty
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
                          (StgConApp !(dataConIdForConstant ty) [!(StgVarArg <$> mkBinderIdVar fc n (ALocal 6))] [])
                      ])
                ])
          ]
--  pure $ StgOpApp op (toList args) resultType resultTypeName
  -- Case (StgApp (Var)) of
  --   [ AltDataCon val => StgOpApp...
  --   ]
  --   AltType (AlgAlt for primitive-type)

--     Add : (ty : Constant) -> PrimFn 2
--     Sub : (ty : Constant) -> PrimFn 2
--     Mul : (ty : Constant) -> PrimFn 2
--     Div : (ty : Constant) -> PrimFn 2
--     Mod : (ty : Constant) -> PrimFn 2
--     Neg : (ty : Constant) -> PrimFn 1
--     ShiftL : (ty : Constant) -> PrimFn 2
--     ShiftR : (ty : Constant) -> PrimFn 2

--     BAnd : (ty : Constant) -> PrimFn 2
--     BOr : (ty : Constant) -> PrimFn 2
--     BXOr : (ty : Constant) -> PrimFn 2

--     LT  : (ty : Constant) -> PrimFn 2
--     LTE : (ty : Constant) -> PrimFn 2
--     EQ  : (ty : Constant) -> PrimFn 2
--     GTE : (ty : Constant) -> PrimFn 2
--     GT  : (ty : Constant) -> PrimFn 2

--     Use ByteArray primops
--     https://github.com/grin-compiler/ghc-whole-program-compiler-project/blob/master/external-stg-interpreter/lib/Stg/Interpreter/PrimOp/ByteArray.hs
--     Char and WideChar
--     StrLength : PrimFn 1
--     StrHead : PrimFn 1
--     StrTail : PrimFn 1
--     StrIndex : PrimFn 2
--     StrCons : PrimFn 2
--     StrAppend : PrimFn 2
--     StrReverse : PrimFn 1
--     StrSubstr : PrimFn 3

--     DoubleExp : PrimFn 1
--     DoubleLog : PrimFn 1
--     DoubleSin : PrimFn 1
--     DoubleCos : PrimFn 1
--     DoubleTan : PrimFn 1
--     DoubleASin : PrimFn 1
--     DoubleACos : PrimFn 1
--     DoubleATan : PrimFn 1
--     DoubleSqrt : PrimFn 1
--     DoubleFloor : PrimFn 1
--     DoubleCeiling : PrimFn 1

--     Cast : Constant -> Constant -> PrimFn 1 -- What is the semantics for this? Check in the official backend.
--     BelieveMe : PrimFn 3
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


mutual
  compileANF
    :  {auto _ : Ref Uniques UniqueMap}
    -> {auto _ : Ref Counter Int}
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
    binding <- do
      binder  <- mkSBinderLocal fc funName var
      stgExpr <- compileANF funName expr
      pure $ StgNonRec binder $ StgRhsClosure Updatable [] stgExpr
    stgBody <- compileANF funName body
    pure $ StgLet binding stgBody

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
--    altType <- AltAlg <$> ?wat -- Question: How to get the right typecon for this?
--                               -- Answer: We need to have the DataConId -> TyConId mapping
    let altType = PolyAlt -- TODO: Fix this
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
    = pure $ StgLit $ LitNullAddr

  -- TODO: Implement: Use Crash primop. errorBlech2 for reporting error ("%s", msg)
  compileANF _ (ACrash _ msg)
    = pure $ StgLit $ LitString $ "ACrash " ++ msg

  compileConAlt
    :  {auto _ : Ref Uniques UniqueMap}
    -> {auto _ : Ref Counter Int}
    -> FC -> Core.Name.Name -> AConAlt
    -> Core SAlt
  compileConAlt fc funName (MkAConAlt name tag args body) = do
    stgArgs     <- traverse (mkSBinderLocal fc funName) args
    stgBody     <- compileANF funName body
    stgDataCon  <- compileDataConId tag
    pure $ MkAlt (AltDataCon stgDataCon) stgArgs stgBody

  compileConstAlt
    :  {auto _ : Ref Uniques UniqueMap}
    -> {auto _ : Ref Counter Int}
    -> Core.Name.Name -> AConstAlt
    -> Core SAlt
  compileConstAlt funName (MkAConstAlt constant body) = do
    stgBody <- compileANF funName body
    lit <- compileAltConstant constant
    pure $ MkAlt (AltLit lit) [] stgBody



resolvedNameId
  :  {auto _ : Ref Ctxt Defs}
  -> Core.Name.Name
  -> Core Int
resolvedNameId n = do
  (Resolved r) <- toResolvedNames n
    | _ => coreFail $ InternalError $ "Name doesn't have resolved id: " ++ show n
  pure r

getTyCon
  :  {auto _ : Ref Ctxt Defs}
  -> Core.Name.Name
  -> Core (Maybe (Int, Def))
getTyCon n = do
  r <- resolvedNameId n
  c <- gamma <$> get Ctxt
  mdef <- lookupDefExact n c
  case mdef of
    Just t@(TCon _ _ _ _ _ _ _ _) => pure $ Just (r, t)
    _                             => pure Nothing

getDataCon
  :  {auto _ : Ref Ctxt Defs}
  -> Core.Name.Name
  -> Core (Maybe (Int, Def))
getDataCon n = do
  r <- resolvedNameId n
  c <- gamma <$> get Ctxt
  mdef <- lookupDefExact n c
  case mdef of
    Just d@(DCon _ _ _) => pure $ Just (r, d)
    _                   => pure Nothing

compileTopBinding
  :  {auto _ : Ref Uniques UniqueMap}
  -> {auto _ : Ref Counter Int}
  -> {auto _ : Ref Ctxt Defs}
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

-- Datatypes and the connection to their types needs to be reconstructed as STG
-- needs this information when the case expression is generated.
namespace LearnDataTypes

  ||| Datatypes annotation for Ref
  export
  data ADTs : Type where

  ||| Associates the resolved names with the TyCon.
  export
  ADTMap : Type
  ADTMap = (IntMap (STyCon, Def, List Def), List STyCon)

  ||| Create the ADTs reference
  export
  mkADTs : Core (Ref ADTs ADTMap)
  mkADTs = newRef ADTs (empty, [])

  -- TODO: Documentational comment.
  learnDataType
    :  {auto _ : Ref Ctxt Defs}
    -> {auto _ : Ref ADTs ADTMap}
    -> {auto _ : Ref Uniques UniqueMap}
    -> {auto _ : Ref Counter Int}
    -> Core.Name.Name
    -> Core ()
  learnDataType n = do
    tcon <- getTyCon n
    case tcon of
      Nothing => pure ()
      Just (tid, tcon@(TCon _ _ _ _ _ _ dts _)) => do
        -- Question: Why tags are always 100 for these TCons?
        Just dcons <- sequence <$> traverse getDataCon dts
           | Nothing => coreFail $ InternalError $ "At least one of the data constructors was not resolved from: " ++ show dts
        (dcm, ts) <- get ADTs

        -- Check if the IDs aren't associated with ADTs already, if so, that could be a problem.
        let resolvedIDs = tid :: map fst dcons
        [] <- pure $ mapMaybe (\i => (\(_,d,_) => (i, d)) <$> lookup i dcm) resolvedIDs
           | defineds => coreFail $ InternalError $ unlines $ "IDs are already associated with ADTs:" :: map show defineds

        -- Create the Data constructors
        unorderedStgDataCons <- catMaybes <$> traverse
          (\(did, dcon) => case dcon of
              (DCon order arity _) => do
                fullName <- toFullNames (Resolved did)
                -- TODO: Remove catMaybe, make this safer.
                pure $ Just ( order
                            , MkSDataCon
                                (show fullName)
                                (MkDataConId !(getUnique (show fullName)))
                                (AlgDataCon (replicate arity LiftedRep))
                                !(mkSBinder emptyFC True ("mk" ++ show fullName)) -- TODO: replace emptyFC, define TopLevel
                                (SsUnhelpfulSpan "TODO: stgDataCon")
                            )
              _ => pure Nothing)
          dcons
        let stgDataCons = map snd $ sortBy (\(o1,_) , (o2,_) => compare o1 o2) unorderedStgDataCons

        -- Create the Type constructor
        typeFullName <- toFullNames (Resolved tid)
        -- TODO: Separate types and data constructor namespace
        stgTyCon <- pure $ MkSTyCon
                             (show typeFullName)
                             (MkTyConId !(getUnique (show typeFullName)))
                             stgDataCons
                             (SsUnhelpfulSpan "TODO: stgTyCon")

        -- Register the type for the dataconstructor ids and type id
        let idsToSTyCon = fromList $ map (\i => (i, (stgTyCon, tcon, map snd dcons))) resolvedIDs
        put ADTs (mergeLeft dcm idsToSTyCon, stgTyCon :: ts)
      _ => pure ()

  registerLearntDataTypes
    :  {auto _ : Ref DataTypes DataTypeMap}
    -> {auto _ : Ref ADTs ADTMap}
    -> Core ()
  registerLearntDataTypes = do
    (_, stgTyCons) <- get ADTs
    traverse_ (defineDataType (MkUnitId "MainUnit") (MkModuleName "Main")) stgTyCons
    -- TODO: Remove magic constants

  ||| Discover the datatypes from ACon, when the ACon stands for an ADT.
  export
  learnDataTypes
    :  {auto _ : Ref Ctxt Defs}
    -> {auto _ : Ref ADTs ADTMap}
    -> {auto _ : Ref Uniques UniqueMap}
    -> {auto _ : Ref Counter Int}
    -> {auto _ : Ref DataTypes DataTypeMap}
    -> List (Core.Name.Name, ANFDef)
    -> Core ()
  learnDataTypes defs = do
    Core.traverse_
      (\(n,d) => case d of
        con@(MkACon _ _) => learnDataType n
        _                => pure ())
      defs
    registerLearntDataTypes


  ||| Lookup if there is an ADT defined for the given name either for type name or data con name.
  export
  lookupTyCon
    :  {auto _ : Ref ADTs ADTMap}
    -> {auto _ : Ref Ctxt Defs}
    -> Core.Name.Name
    -> Core (Maybe STyCon)
  lookupTyCon n = do
    (dcm, _) <- get ADTs
    (Resolved i) <- toResolvedNames n
      | other => coreFail $ InternalError $ "Name doesn't have resolution: " ++ show other
    pure $ fst <$> lookup i dcm

-- We compile only one enormous module
export
compileModule
  :  {auto _ : Ref Uniques UniqueMap}
  -> {auto _ : Ref Counter Int}
  -> {auto _ : Ref DataTypes DataTypeMap}
  -> {auto _ : Ref Ctxt Defs}
  -> List (Core.Name.Name, ANFDef)
  -> Core SModule
compileModule anfDefs = do
  definePrimitiveDataTypes
  adts <- mkADTs
  learnDataTypes anfDefs
  let phase              = "Main"
  let moduleUnitId       = MkUnitId "MainUnit"
  let name               = MkModuleName "Main" -- : ModuleName
  let sourceFilePath     = "some.idr" -- : String
  let foreignStubs       = NoStubs -- : ForeignStubs -- ???
  let hasForeignExported = False -- : Bool
  let dependency         = [] -- : List (UnitId, List ModuleName)
  let externalTopIds     = [] -- : List (UnitId, List (ModuleName, List idBnd))
  topBindings            <- mapMaybe id <$> traverse compileTopBinding anfDefs
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
constTag : Constant -> Int
-- 1 = ->, 2 = Type
constTag IntType = 3
constTag IntegerType = 4
constTag Bits8Type = 5
constTag Bits16Type = 6
constTag Bits32Type = 7
constTag Bits64Type = 8
constTag StringType = 9
constTag CharType = 10
constTag DoubleType = 11
constTag WorldType = 12 -- How to represent the World type in STG?
constTag _ = 0

public export
data PrimFn : Nat -> Type where
     Add : (ty : Constant) -> PrimFn 2
     Sub : (ty : Constant) -> PrimFn 2
     Mul : (ty : Constant) -> PrimFn 2
     Div : (ty : Constant) -> PrimFn 2
     Mod : (ty : Constant) -> PrimFn 2
     Neg : (ty : Constant) -> PrimFn 1
     ShiftL : (ty : Constant) -> PrimFn 2
     ShiftR : (ty : Constant) -> PrimFn 2

     BAnd : (ty : Constant) -> PrimFn 2
     BOr : (ty : Constant) -> PrimFn 2
     BXOr : (ty : Constant) -> PrimFn 2

     LT  : (ty : Constant) -> PrimFn 2
     LTE : (ty : Constant) -> PrimFn 2
     EQ  : (ty : Constant) -> PrimFn 2
     GTE : (ty : Constant) -> PrimFn 2
     GT  : (ty : Constant) -> PrimFn 2

     StrLength : PrimFn 1
     StrHead : PrimFn 1
     StrTail : PrimFn 1
     StrIndex : PrimFn 2
     StrCons : PrimFn 2
     StrAppend : PrimFn 2
     StrReverse : PrimFn 1
     StrSubstr : PrimFn 3

     DoubleExp : PrimFn 1
     DoubleLog : PrimFn 1
     DoubleSin : PrimFn 1
     DoubleCos : PrimFn 1
     DoubleTan : PrimFn 1
     DoubleASin : PrimFn 1
     DoubleACos : PrimFn 1
     DoubleATan : PrimFn 1
     DoubleSqrt : PrimFn 1
     DoubleFloor : PrimFn 1
     DoubleCeiling : PrimFn 1

     Cast : Constant -> Constant -> PrimFn 1
     BelieveMe : PrimFn 3
     Crash : PrimFn 2

https://gitlab.haskell.org/ghc/ghc/-/blob/master/compiler/GHC/Builtin/primops.txt.pp
-}

{-
How to represent String and idris FFI calls?
-}
