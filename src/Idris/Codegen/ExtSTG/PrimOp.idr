module Idris.Codegen.ExtSTG.PrimOp

import Data.Vect
import Core.Core
import Core.Context
import Compiler.ANF
import Data.List
import Idris.Codegen.ExtSTG.STG
import Idris.Codegen.ExtSTG.Core
import Idris.Codegen.ExtSTG.String


||| PrimType when the constant is compiled insides the box.
export
constantToPrimRep : Constant -> Core (List PrimRep)
constantToPrimRep IntType     = pure [IntRep]
constantToPrimRep IntegerType = pure [IntRep] -- TODO: This is not the right representation for integer
constantToPrimRep Bits8Type   = pure [Word8Rep]
constantToPrimRep Bits16Type  = pure [Word16Rep]
constantToPrimRep Bits32Type  = pure [Word32Rep]
constantToPrimRep Bits64Type  = pure [Word64Rep]
constantToPrimRep DoubleType  = pure [DoubleRep]
-- constantToPrimRep StringType  = pure [AddrRep]
constantToPrimRep CharType    = pure [Word8Rep] -- TODO: Check if this is the right type for Chars?
constantToPrimRep WorldType   = pure []
constantToPrimRep other = coreFail $ InternalError $ "No PrimRep for " ++ show other

||| Creates a multilevel case statement block to unwrap/wrap the primitive values
||| around a two parameter STG primitive function call.
binPrimOp
  :  UniqueMapRef
  => Ref Counter Int
  => DataTypeMapRef
  => FC -> Core.Name.Name
  -> Constant -> StgOp -> Vect 2 AVar -> Constant
  -> Core (Expr Core.stgRepType)
binPrimOp fc n ty op as rt = do
  [arg1, arg2] <- traverseVect (mkBinderIdVar fc n Core.stgRepType) as
  ((AlgDataCon [rep]) ** dc) <- dataConIdForConstant ty
    | wrongRep => coreFail $ InternalError $ "DataConId has wrong RepType: " ++ show (fc, n, wrongRep)
  n4 <- mkSBinderRepLocal (SingleValue rep) fc n 4
  n5 <- mkSBinderRepLocal (SingleValue rep) fc n 5
  let resultTypeName = Nothing
  pure $ StgCase
          (StgApp arg1 [] Core.stgRepType)
          !(mkSBinderLocal fc n 3)
          !(AlgAlt <$> tyConIdForConstant ty)
          [ MkAlt (AltDataCon (mkDataConIdPi dc)) n4
             (StgCase
                (StgApp arg2 [] Core.stgRepType)
                !(mkSBinderLocal fc n 4)
                !(AlgAlt <$> tyConIdForConstant ty)
                [ MkAlt (AltDataCon (mkDataConIdPi dc)) n5
                    (StgCase
                      (StgOpApp op
                        [ !(StgVarArg . mkBinderIdPi <$> mkBinderIdVar fc n Core.stgRepType (ALocal 4))
                        , !(StgVarArg . mkBinderIdPi <$> mkBinderIdVar fc n Core.stgRepType (ALocal 5))
                        ]
                        Core.stgRepType -- TODO: Unboxed PrimRep like Int16Rep
                        resultTypeName)
                      !(mkSBinderLocal fc n 6)
                      (PrimAlt rep)
                      [ MkAlt AltDefault ()
                          (StgConApp !(dataConIdForConstant rt)
                                     [!(StgVarArg . mkBinderIdPi <$> mkBinderIdVar fc n Core.stgRepType (ALocal 6))]
                                     [])
                      ])
                ])
          ]

||| Creates a multilevel case statement block to unwrap/wrap the primitive values
||| around a two parameter STG primitive function call.
unaryPrimOp
  :  UniqueMapRef
  => Ref Counter Int
  => DataTypeMapRef
  => FC -> Core.Name.Name
  -> Constant -> StgOp -> Vect 1 AVar -> Constant
  -> Core (Expr Core.stgRepType)
unaryPrimOp fc n ty op as rt = do
  [arg1] <- traverseVect (mkBinderIdVar fc n Core.stgRepType) as
  -- As we box everyting, and the result will be Lifted
  let resultTypeName = Nothing
  ((AlgDataCon [rep]) ** dc) <- dataConIdForConstant ty
    | wrongRep => coreFail $ InternalError $ "DataConId has wrong RepType: " ++ show (fc,n,wrongRep)
  n4 <- mkSBinderRepLocal (SingleValue rep) fc n 4
  pure $ StgCase
          (StgApp arg1 [] Core.stgRepType)
          !(mkSBinderLocal fc n 3)
          !(AlgAlt <$> tyConIdForConstant ty)
          [ MkAlt (AltDataCon (mkDataConIdPi dc)) n4
             (StgCase
                (StgOpApp op
                  [!(StgVarArg . mkBinderIdPi <$> mkBinderIdVar fc n Core.stgRepType (ALocal 4))]
                  Core.stgRepType -- TODO: Unboxed PrimRep like Int16Rep
                  resultTypeName)
                !(mkSBinderLocal fc n 5)
                (PrimAlt rep)
                [ MkAlt AltDefault ()
                    (StgConApp !(dataConIdForConstant rt) [!(StgVarArg . mkBinderIdPi <$> mkBinderIdVar fc n Core.stgRepType (ALocal 5))] [])
                ])
          ]

-- TODO: Lookup the name of the already defined STG function. Mainly it is used to lookup
-- String helper functions.
definedFunction
  :  {auto _ : UniqueMapRef}
  -> {auto _ : Ref Counter Int}
  -> String
  -> Core (BinderId Core.stgRepType)
definedFunction = mkBinderIdStr

export
compilePrimOp
  :  UniqueMapRef
  => Ref Counter Int
  => DataTypeMapRef
  => FC -> Core.Name.Name -> PrimFn arity -> Vect arity AVar
  -> Core (Expr Core.stgRepType)
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

{-
String literals must be toplevel STG definitions. That are represented as Addr in STG.
Idris string module that contains the STG definition of String Handling.

Idris:
String constants are instroduced with the Str constructor.
The runtime system of the backend should create runtime values from the String constants.
String operations should work on the runtime representation of the string values and return
new String value.

STG:
String constants are represented as LitString which will be represented as an Addr# inside a
top-level definition. When we create an STG-String value from the Idris.Str literal we need to
look-up the top-level STG definition for the String literal and extract the Addr# value
wrap it in a constructor that the STG.String Operation could handle.
I need an example for the top-level String constant in STG.
-}

compilePrimOp {arity=1} fc n StrLength as =
  pure (StgApp !(definedFunction "Idris.String.strLength")
               !(traverse (map (StgVarArg . mkBinderIdPi) . mkBinderIdVar fc n Core.stgRepType) $ toList as)
               (SingleValue LiftedRep))

-- TODO: Appropiate Char handling.
compilePrimOp {arity=1} fc n StrHead as =
  pure (StgApp !(definedFunction "Idris.String.strHead")
               !(traverse (map (StgVarArg . mkBinderIdPi) . mkBinderIdVar fc n Core.stgRepType) $ toList as)
               (SingleValue LiftedRep))

compilePrimOp {arity=1} fc n StrTail as =
  pure (StgApp !(definedFunction "Idris.String.strTail")
               !(traverse (map (StgVarArg . mkBinderIdPi) . mkBinderIdVar fc n Core.stgRepType) $ toList as)
               (SingleValue LiftedRep))

compilePrimOp {arity=2} fc n StrIndex as =
  pure (StgApp !(definedFunction "Idris.String.strIndex")
               !(traverse (map (StgVarArg . mkBinderIdPi) . mkBinderIdVar fc n Core.stgRepType) $ toList as)
               (SingleValue LiftedRep))

compilePrimOp {arity=2} fc n StrCons as =
  pure (StgApp !(definedFunction "Idris.String.strCons")
               !(traverse (map (StgVarArg . mkBinderIdPi) . mkBinderIdVar fc n Core.stgRepType) $ toList as)
               (SingleValue LiftedRep))

compilePrimOp {arity=2} fc n StrAppend as =
  pure (StgApp !(definedFunction "Idris.String.strAppend")
               !(traverse (map (StgVarArg . mkBinderIdPi) . mkBinderIdVar fc n Core.stgRepType) $ toList as)
               (SingleValue LiftedRep))

compilePrimOp {arity=1} fc n StrReverse as =
  pure (StgApp !(definedFunction "Idris.String.strReverse")
               !(traverse (map (StgVarArg . mkBinderIdPi) . mkBinderIdVar fc n Core.stgRepType) $ toList as)
               (SingleValue LiftedRep))

compilePrimOp {arity=3} fc n StrSubstr as =
  pure (StgApp !(definedFunction "Idris.String.strSubstr")
               !(traverse (map (StgVarArg . mkBinderIdPi) . mkBinderIdVar fc n Core.stgRepType) $ toList as)
               (SingleValue LiftedRep))

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
  pure (StgApp !(mkBinderIdVar fc n Core.stgRepType a) [] (SingleValue LiftedRep))

--     Crash : PrimFn 2 -- What are the parameters for this?
--     Use this FFI call to crash the haskell runtime.
--     https://github.com/grin-compiler/ghc-whole-program-compiler-project/blob/master/external-stg-interpreter/lib/Stg/Interpreter/FFI.hs#L178-L183
--     1b3f15ca69ea443031fa69a488c660a2c22182b8
compilePrimOp _ _ p as
  = pure
  $ StgApp (!(definedFunction STRING_FROM_ADDR))
           [ StgLitArg $ LitString $ "compilePrimOp " ++ show p ++ " " ++ show as
           ]
           (SingleValue LiftedRep)
