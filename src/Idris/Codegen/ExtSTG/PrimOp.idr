module Idris.Codegen.ExtSTG.PrimOp

import Data.Vect
import Core.Core
import Core.Context
import Compiler.ANF
import Data.List
import Idris.Codegen.ExtSTG.STG
import Idris.Codegen.ExtSTG.Core
-- import Idris.Codegen.ExtSTG.String
import Idris.Codegen.ExtSTG.Context
import Idris.Codegen.ExtSTG.ExternalTopIds

||| PrimType when the constant is compiled insides the box.
export
constantToPrimRep : PrimType -> Core (List PrimRep)
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
  : {name : String} -> {primOpRep, retPrimOpRep: PrimRep}
  -> Ref STGCtxt STGContext
  => FC -> Core.Name.Name
  -> PrimType -> PrimOp name [primOpRep, primOpRep] retPrimOpRep -> Vect 2 AVar -> PrimType
  -> Core (Expr Core.stgRepType)
binPrimOp fc n ty op as rt = do
  [arg1, arg2] <- traverseVect (mkBinderIdVar fc n Core.stgRepType) as
  ((AlgDataCon [rep]) ** dc) <- dataConIdForPrimType ty
    | wrongRep => coreFail $ InternalError $ "DataConId has wrong RepType: " ++ show (fc, n, wrongRep)
  Refl <- checkSemiDecEq ("binPrimOp:" ++ show n ++ " " ++ PrimOp.name op) rep primOpRep
  n4 <- localBinderRep fc (SingleValue primOpRep)
  n5 <- localBinderRep fc (SingleValue primOpRep)
  n6 <- localBinderRep fc (SingleValue retPrimOpRep)
  let resultTypeName = Nothing
  pure
    $ StgCase
        (AlgAlt !(tyConIdForPrimType ty))
        (StgApp arg1 [] Core.stgRepType)
        !nonused
        [ MkAlt (AltDataCon (mkDataConIdSg dc)) n4
          $ StgCase
              (AlgAlt !(tyConIdForPrimType ty))
              (StgApp arg2 [] Core.stgRepType)
              !nonused
              [ MkAlt (AltDataCon (mkDataConIdSg dc)) n5
                $ StgCase
                    (PrimAlt retPrimOpRep)
                    (StgOpApp op
                      [ StgVarArg (getBinderId n4)
                      , StgVarArg (getBinderId n5)
                      ])
                    n6
                    [ MkAlt AltDefault () (StgConApp !(dataConIdRepForPrimType retPrimOpRep rt) (StgVarArg (getBinderId n6))) ]
              ]
        ]

||| Creates a multilevel case statement block to unwrap/wrap the primitive values
||| around a two parameter STG primitive function call.
unaryPrimOp
  :  {name : String} -> {primOpRep, retPrimOpRep : PrimRep}
  -> Ref STGCtxt STGContext
  => FC
  -> Core.Name.Name
  -> PrimType
  -> PrimOp name [primOpRep] retPrimOpRep
  -> Vect 1 AVar
  -> PrimType
  -> Core (Expr Core.stgRepType)
unaryPrimOp fc n ty op as rt = do
  [arg1] <- traverseVect (mkBinderIdVar fc n Core.stgRepType) as
  -- As we box everyting, and the result will be Lifted
  ((AlgDataCon [rep]) ** dc) <- dataConIdForPrimType ty
    | wrongRep => coreFail $ InternalError $ "DataConId has wrong RepType: " ++ show (fc,n,wrongRep)
  Refl <- checkSemiDecEq ("unaryPrimOp:" ++ show n ++ " " ++ PrimOp.name op) primOpRep rep
  n4 <- localBinderRep fc (SingleValue primOpRep)
  n5 <- localBinderRep fc (SingleValue retPrimOpRep)
  pure $ StgCase
          (AlgAlt !(tyConIdForPrimType ty))
          (StgApp arg1 [] Core.stgRepType)
          !nonused
          [ MkAlt (AltDataCon (mkDataConIdSg dc)) n4
            $ StgCase
                (PrimAlt retPrimOpRep)
                (StgOpApp op (StgVarArg (getBinderId n4)))
                n5
                [ MkAlt AltDefault ()
                    (StgConApp
                      !(dataConIdRepForPrimType retPrimOpRep rt)
                      (StgVarArg (getBinderId n5)))
                ]
          ]

-- TODO: Lookup the name of the already defined STG function. Mainly it is used to lookup
-- String helper functions.
definedFunction
  :  Ref STGCtxt STGContext
  => String
  -> Core (BinderId Core.stgRepType)
definedFunction = mkBinderIdStr

export
compilePrimOp
  :  Ref STGCtxt STGContext
  => FC -> Core.Name.Name -> PrimFn ar -> Vect ar AVar
  -> Core (Expr Core.stgRepType)
compilePrimOp {ar = 2} fc n (Add IntType)      as = binPrimOp fc n IntType    PlusInt     as IntType
compilePrimOp {ar = 2} fc n (Add IntegerType)  as = binPrimOp fc n IntType    PlusInt     as IntegerType
compilePrimOp {ar = 2} fc n (Add Bits8Type)    as = binPrimOp fc n Bits8Type  PlusWord8   as Bits8Type
compilePrimOp {ar = 2} fc n (Add Bits16Type)   as = binPrimOp fc n Bits16Type PlusWord16  as Bits16Type
compilePrimOp {ar = 2} fc n (Add Bits32Type)   as = binPrimOp fc n Bits32Type PlusWord    as Bits32Type
compilePrimOp {ar = 2} fc n (Add Bits64Type)   as = binPrimOp fc n Bits64Type PlusWord    as Bits64Type
compilePrimOp {ar = 2} fc n (Add DoubleType)   as = binPrimOp fc n DoubleType PlusDouble  as DoubleType
compilePrimOp {ar = 2} fc n (Add ty) _ = throw $ InternalError $ "No add for:" ++ show ty

compilePrimOp {ar = 2} fc n (Sub IntType)      as = binPrimOp fc n IntType     SubInt    as IntType
compilePrimOp {ar = 2} fc n (Sub IntegerType)  as = binPrimOp fc n IntegerType SubInt    as IntegerType
compilePrimOp {ar = 2} fc n (Sub Bits8Type)    as = binPrimOp fc n Bits8Type   SubWord8  as Bits8Type
compilePrimOp {ar = 2} fc n (Sub Bits16Type)   as = binPrimOp fc n Bits16Type  SubWord16 as Bits16Type
compilePrimOp {ar = 2} fc n (Sub Bits32Type)   as = binPrimOp fc n Bits32Type  SubWord   as Bits32Type
compilePrimOp {ar = 2} fc n (Sub Bits64Type)   as = binPrimOp fc n Bits64Type  SubWord   as Bits64Type
compilePrimOp {ar = 2} fc n (Sub DoubleType)   as = binPrimOp fc n DoubleType  SubDouble as DoubleType
compilePrimOp {ar = 2} fc n (Sub ty) _ = throw $ InternalError $ "No sub for:" ++ show ty

compilePrimOp {ar = 2} fc n (Mul IntType)      as = binPrimOp fc n IntType     TimesInt    as IntType
compilePrimOp {ar = 2} fc n (Mul IntegerType)  as = binPrimOp fc n IntegerType TimesInt    as IntegerType
compilePrimOp {ar = 2} fc n (Mul Bits8Type)    as = binPrimOp fc n Bits8Type   TimesWord8  as Bits8Type
compilePrimOp {ar = 2} fc n (Mul Bits16Type)   as = binPrimOp fc n Bits16Type  TimesWord16 as Bits16Type
compilePrimOp {ar = 2} fc n (Mul Bits32Type)   as = binPrimOp fc n Bits32Type  TimesWord   as Bits32Type
compilePrimOp {ar = 2} fc n (Mul Bits64Type)   as = binPrimOp fc n Bits64Type  TimesWord   as Bits64Type
compilePrimOp {ar = 2} fc n (Mul DoubleType)   as = binPrimOp fc n DoubleType  TimesDouble as DoubleType
compilePrimOp {ar = 2} fc n (Mul ty) _ = throw $ InternalError $ "No mul for:" ++ show ty

compilePrimOp {ar = 2} fc n (Div IntType)      as = binPrimOp fc n IntType     QuotInt     as IntType
compilePrimOp {ar = 2} fc n (Div IntegerType)  as = binPrimOp fc n IntegerType QuotInt     as IntegerType
compilePrimOp {ar = 2} fc n (Div Bits8Type)    as = binPrimOp fc n Bits8Type   QuotWord8   as Bits8Type
compilePrimOp {ar = 2} fc n (Div Bits16Type)   as = binPrimOp fc n Bits16Type  QuotWord16  as Bits16Type
compilePrimOp {ar = 2} fc n (Div Bits32Type)   as = binPrimOp fc n Bits32Type  QuotWord    as Bits32Type
compilePrimOp {ar = 2} fc n (Div Bits64Type)   as = binPrimOp fc n Bits64Type  QuotWord    as Bits64Type
compilePrimOp {ar = 2} fc n (Div DoubleType)   as = binPrimOp fc n DoubleType  QuotDouble  as DoubleType
compilePrimOp {ar = 2} fc n (Div ty) _ = throw $ InternalError $ "No div for:" ++ show ty

compilePrimOp {ar = 2} fc n (Mod IntType)      as = binPrimOp fc n IntType     RemInt    as IntType
compilePrimOp {ar = 2} fc n (Mod IntegerType)  as = binPrimOp fc n IntegerType RemInt    as IntegerType
compilePrimOp {ar = 2} fc n (Mod Bits8Type)    as = binPrimOp fc n Bits8Type   RemWord8  as Bits8Type
compilePrimOp {ar = 2} fc n (Mod Bits16Type)   as = binPrimOp fc n Bits16Type  RemWord16 as Bits16Type
compilePrimOp {ar = 2} fc n (Mod Bits32Type)   as = binPrimOp fc n Bits32Type  RemWord   as Bits32Type
compilePrimOp {ar = 2} fc n (Mod Bits64Type)   as = binPrimOp fc n Bits64Type  RemWord   as Bits64Type
compilePrimOp {ar = 2} fc n (Mod ty) _ = throw $ InternalError $ "No mod for:" ++ show ty

compilePrimOp {ar = 1} fc n (Neg IntType)      as = unaryPrimOp fc n IntType      NegateInt     as IntType
compilePrimOp {ar = 1} fc n (Neg IntegerType)  as = unaryPrimOp fc n IntegerType  NegateInt     as IntegerType
compilePrimOp {ar = 1} fc n (Neg DoubleType)   as = unaryPrimOp fc n DoubleType   NegateDouble  as DoubleType
compilePrimOp {ar = 1} fc n (Neg ty) _ = throw $ InternalError $ "No neg for:" ++ show ty

compilePrimOp {ar = 2} fc n (ShiftL IntType)     as = binPrimOp fc n IntType IShiftLInt as IntType
compilePrimOp {ar = 2} fc n (ShiftL IntegerType) as = binPrimOp fc n IntType IShiftLInt as IntegerType
compilePrimOp {ar = 2} fc n (ShiftL ty) _ = throw $ InternalError $ "No shiftl for:" ++ show ty

compilePrimOp {ar = 2} fc n (ShiftR IntType)     as = binPrimOp fc n IntType IShiftLRInt as IntType
compilePrimOp {ar = 2} fc n (ShiftR IntegerType) as = binPrimOp fc n IntType IShiftLRInt as IntegerType
compilePrimOp {ar = 2} fc n (ShiftR ty) _ = throw $ InternalError $ "No shiftr for:" ++ show ty

compilePrimOp {ar = 2} fc n (BAnd IntType)     as = binPrimOp fc n IntType     AndInt  as IntType
compilePrimOp {ar = 2} fc n (BAnd IntegerType) as = binPrimOp fc n IntegerType AndInt  as IntegerType
compilePrimOp {ar = 2} fc n (BAnd Bits8Type)   as = binPrimOp fc n Bits8Type   AndWord as Bits8Type
compilePrimOp {ar = 2} fc n (BAnd Bits16Type)  as = binPrimOp fc n Bits16Type  AndWord as Bits16Type
compilePrimOp {ar = 2} fc n (BAnd Bits32Type)  as = binPrimOp fc n Bits32Type  AndWord as Bits32Type
compilePrimOp {ar = 2} fc n (BAnd Bits64Type)  as = binPrimOp fc n Bits64Type  AndWord as Bits64Type
compilePrimOp {ar = 2} fc n (BAnd ty) _ = throw $ InternalError $ "No band for:" ++ show ty

compilePrimOp {ar = 2} fc n (BOr IntType)      as = binPrimOp fc n IntType     OrInt   as IntType
compilePrimOp {ar = 2} fc n (BOr IntegerType)  as = binPrimOp fc n IntegerType OrInt   as IntegerType
compilePrimOp {ar = 2} fc n (BOr Bits8Type)    as = binPrimOp fc n Bits8Type   OrWord  as Bits8Type
compilePrimOp {ar = 2} fc n (BOr Bits16Type)   as = binPrimOp fc n Bits16Type  OrWord  as Bits16Type
compilePrimOp {ar = 2} fc n (BOr Bits32Type)   as = binPrimOp fc n Bits32Type  OrWord  as Bits32Type
compilePrimOp {ar = 2} fc n (BOr Bits64Type)   as = binPrimOp fc n Bits64Type  OrWord  as Bits64Type
compilePrimOp {ar = 2} fc n (BOr ty) _ = throw $ InternalError $ "No bor for:" ++ show ty

compilePrimOp {ar = 2} fc n (BXOr IntType)     as = binPrimOp fc n IntType     XOrInt  as IntType
compilePrimOp {ar = 2} fc n (BXOr IntegerType) as = binPrimOp fc n IntegerType XOrInt  as IntegerType
compilePrimOp {ar = 2} fc n (BXOr Bits8Type)   as = binPrimOp fc n Bits8Type   XOrWord as Bits8Type
compilePrimOp {ar = 2} fc n (BXOr Bits16Type)  as = binPrimOp fc n Bits16Type  XOrWord as Bits16Type
compilePrimOp {ar = 2} fc n (BXOr Bits32Type)  as = binPrimOp fc n Bits32Type  XOrWord as Bits32Type
compilePrimOp {ar = 2} fc n (BXOr Bits64Type)  as = binPrimOp fc n Bits64Type  XOrWord as Bits64Type
compilePrimOp {ar = 2} fc n (BXOr ty) _ = throw $ InternalError $ "No bxor for:" ++ show ty

compilePrimOp {ar = 2} fc n (LT IntType)     as = binPrimOp fc n IntType     LTInt     as IntType
compilePrimOp {ar = 2} fc n (LT IntegerType) as = binPrimOp fc n IntegerType LTInt     as IntType
compilePrimOp {ar = 2} fc n (LT Bits8Type)   as = binPrimOp fc n Bits8Type   LTWord8   as IntType
compilePrimOp {ar = 2} fc n (LT Bits16Type)  as = binPrimOp fc n Bits16Type  LTWord16  as IntType
compilePrimOp {ar = 2} fc n (LT Bits32Type)  as = binPrimOp fc n Bits32Type  LTWord    as IntType
compilePrimOp {ar = 2} fc n (LT Bits64Type)  as = binPrimOp fc n Bits64Type  LTWord    as IntType
compilePrimOp {ar = 2} fc n (LT CharType)    as = binPrimOp fc n CharType    LTWord8   as IntType
compilePrimOp {ar = 2} fc n (LT DoubleType)  as = binPrimOp fc n DoubleType  LTDouble  as IntType
compilePrimOp {ar = 2} fc n (LT ty) _ = throw $ InternalError $ "No lt for:" ++ show ty

compilePrimOp {ar = 2} fc n (LTE IntType)      as = binPrimOp fc n IntType     LTEInt    as IntType
compilePrimOp {ar = 2} fc n (LTE IntegerType)  as = binPrimOp fc n IntType     LTEInt    as IntType
compilePrimOp {ar = 2} fc n (LTE Bits8Type)    as = binPrimOp fc n Bits8Type   LTEWord8  as IntType
compilePrimOp {ar = 2} fc n (LTE Bits16Type)   as = binPrimOp fc n Bits16Type  LTEWord16 as IntType
compilePrimOp {ar = 2} fc n (LTE Bits32Type)   as = binPrimOp fc n Bits32Type  LTEWord   as IntType
compilePrimOp {ar = 2} fc n (LTE Bits64Type)   as = binPrimOp fc n Bits64Type  LTEWord   as IntType
compilePrimOp {ar = 2} fc n (LTE CharType)     as = binPrimOp fc n CharType    LTEWord8  as IntType
compilePrimOp {ar = 2} fc n (LTE DoubleType)   as = binPrimOp fc n DoubleType  LTEDouble as IntType
compilePrimOp {ar = 2} fc n (LTE ty) _ = throw $ InternalError $ "No lte for:" ++ show ty

compilePrimOp {ar = 2} fc n (EQ IntType)     as = binPrimOp fc n IntType     EQInt     as IntType
compilePrimOp {ar = 2} fc n (EQ IntegerType) as = binPrimOp fc n IntType     EQInt     as IntType
compilePrimOp {ar = 2} fc n (EQ Bits8Type)   as = binPrimOp fc n Bits8Type   EQWord8   as IntType
compilePrimOp {ar = 2} fc n (EQ Bits16Type)  as = binPrimOp fc n Bits16Type  EQWord16  as IntType
compilePrimOp {ar = 2} fc n (EQ Bits32Type)  as = binPrimOp fc n Bits32Type  EQWord    as IntType
compilePrimOp {ar = 2} fc n (EQ Bits64Type)  as = binPrimOp fc n Bits64Type  EQWord    as IntType
compilePrimOp {ar = 2} fc n (EQ CharType)    as = binPrimOp fc n CharType    EQWord8   as IntType
compilePrimOp {ar = 2} fc n (EQ DoubleType)  as = binPrimOp fc n DoubleType  EQDouble  as IntType
compilePrimOp {ar = 2} fc n (EQ ty) _ = throw $ InternalError $ "No eq for:" ++ show ty

compilePrimOp {ar = 2} fc n (GTE IntType)      as = binPrimOp fc n IntType     GTEInt    as IntType
compilePrimOp {ar = 2} fc n (GTE IntegerType)  as = binPrimOp fc n IntType     GTEInt    as IntType
compilePrimOp {ar = 2} fc n (GTE Bits8Type)    as = binPrimOp fc n Bits8Type   GTEWord8  as IntType
compilePrimOp {ar = 2} fc n (GTE Bits16Type)   as = binPrimOp fc n Bits16Type  GTEWord16 as IntType
compilePrimOp {ar = 2} fc n (GTE Bits32Type)   as = binPrimOp fc n Bits32Type  GTEWord   as IntType
compilePrimOp {ar = 2} fc n (GTE Bits64Type)   as = binPrimOp fc n Bits64Type  GTEWord   as IntType
compilePrimOp {ar = 2} fc n (GTE CharType)     as = binPrimOp fc n CharType    GTEWord8  as IntType
compilePrimOp {ar = 2} fc n (GTE DoubleType)   as = binPrimOp fc n DoubleType  GTEDouble as IntType
compilePrimOp {ar = 2} fc n (GTE ty) _ = throw $ InternalError $ "No gte for:" ++ show ty

compilePrimOp {ar = 2} fc n (GT IntType)     as = binPrimOp fc n IntType     GTInt     as IntType
compilePrimOp {ar = 2} fc n (GT IntegerType) as = binPrimOp fc n IntType     GTInt     as IntType
compilePrimOp {ar = 2} fc n (GT Bits8Type)   as = binPrimOp fc n Bits8Type   GTWord8   as IntType
compilePrimOp {ar = 2} fc n (GT Bits16Type)  as = binPrimOp fc n Bits16Type  GTWord16  as IntType
compilePrimOp {ar = 2} fc n (GT Bits32Type)  as = binPrimOp fc n Bits32Type  GTWord    as IntType
compilePrimOp {ar = 2} fc n (GT Bits64Type)  as = binPrimOp fc n Bits64Type  GTWord    as IntType
compilePrimOp {ar = 2} fc n (GT CharType)    as = binPrimOp fc n CharType    GTWord8   as IntType
compilePrimOp {ar = 2} fc n (GT DoubleType)  as = binPrimOp fc n DoubleType  GTDouble  as IntType
compilePrimOp {ar = 2} fc n (GT ty) _ = throw $ InternalError $ "No gt for:" ++ show ty

{-
String literals must be toplevel STG definitions. That are represented as Addr in STG.
Idris string module that contains the STG definition of String Handling.

Idris:
String constants are instroduced with the Str constructor.
The runtime system of the backend should create runtme values from the String constants.
String operations should work on the runtime representation of the string values and return
new String value.

STG:
String constants are represented as LitString which will be represented as an Addr# inside a
top-level definition. When we create an STG-String value from the Idris.Str literal we need to
look-up the top-level STG definition for the String literal and extract the Addr# value
wrap it in a constructor that the STG.String Operation could handle.
I need an example for the top-level String constant in STG.
-}

compilePrimOp {ar=1} fc n StrLength as = -- strLength :: Str -> Int
  createExtSTGPureApp
    (MkExtName "main" ["Idris", "String"] "strLength")
    !(traverse (map (mkArgSg . StgVarArg) . mkBinderIdVar fc n Core.stgRepType) $ toList as)

-- TODO: Appropiate Char handling.
compilePrimOp {ar=1} fc n StrHead as = -- strHead :: Str -> Char
  createExtSTGPureApp
    (MkExtName "main" ["Idris", "String"] "strHead")
    !(traverse (map (mkArgSg . StgVarArg) . mkBinderIdVar fc n Core.stgRepType) $ toList as)

compilePrimOp {ar=1} fc n StrTail as = do -- strTail :: Str -> IO Str
  args <- traverse (map (mkArgSg . StgVarArg) . mkBinderIdVar fc n Core.stgRepType) $ toList as
  createExtSTGIOApp
    (MkExtName "main" ["Idris", "String"] "strTail")
    (args ++ [mkArgSg (StgVarArg realWorldHashtag)])

compilePrimOp {ar=2} fc n StrIndex as = do -- strIndex :: Str -> Int -> Char
  createExtSTGPureApp
    (MkExtName "main" ["Idris", "String"] "strIndex")
    !(traverse (map (mkArgSg . StgVarArg) . mkBinderIdVar fc n Core.stgRepType) $ toList as)

compilePrimOp {ar=2} fc n StrCons as = do -- strCons :: Char -> Str -> IO Str
  args <- traverse (map (mkArgSg . StgVarArg) . mkBinderIdVar fc n Core.stgRepType) $ toList as
  createExtSTGIOApp
    (MkExtName "main" ["Idris", "String"] "strCons")
    (args ++ [mkArgSg (StgVarArg realWorldHashtag)])

compilePrimOp {ar=2} fc n StrAppend as = do -- strAppend :: Str -> Str -> IO Str
  args <- traverse (map (mkArgSg . StgVarArg) . mkBinderIdVar fc n Core.stgRepType) $ toList as
  createExtSTGIOApp
    (MkExtName "main" ["Idris", "String"] "strAppend")
    (args ++ [mkArgSg (StgVarArg realWorldHashtag)])

compilePrimOp {ar=1} fc n StrReverse as = do -- strReverse :: Str -> IO Str
  args <- traverse (map (mkArgSg . StgVarArg) . mkBinderIdVar fc n Core.stgRepType) $ toList as
  createExtSTGIOApp
    (MkExtName "main" ["Idris", "String"] "strReverse")
    (args ++ [mkArgSg (StgVarArg realWorldHashtag)])

compilePrimOp {ar=3} fc n StrSubstr as = do -- strSubstr :: Int -> Int -> Str -> IO Str
  args <- traverse (map (mkArgSg . StgVarArg) . mkBinderIdVar fc n Core.stgRepType) $ toList as
  createExtSTGIOApp
    (MkExtName "main" ["Idris", "String"] "strSubstr")
    (args ++ [mkArgSg (StgVarArg realWorldHashtag)])

compilePrimOp {ar=1} fc n DoubleExp as = unaryPrimOp fc n DoubleType ExpDouble as DoubleType
compilePrimOp {ar=1} fc n DoubleLog as = unaryPrimOp fc n DoubleType LogDouble as DoubleType
compilePrimOp {ar=1} fc n DoubleSin as = unaryPrimOp fc n DoubleType SinDouble as DoubleType
compilePrimOp {ar=1} fc n DoubleCos as = unaryPrimOp fc n DoubleType CosDouble as DoubleType
compilePrimOp {ar=1} fc n DoubleTan as = unaryPrimOp fc n DoubleType TanDouble as DoubleType
compilePrimOp {ar=1} fc n DoubleASin as = unaryPrimOp fc n DoubleType ASinDouble as DoubleType
compilePrimOp {ar=1} fc n DoubleACos as = unaryPrimOp fc n DoubleType ACosDouble as DoubleType
compilePrimOp {ar=1} fc n DoubleATan as = unaryPrimOp fc n DoubleType ATanDouble as DoubleType
compilePrimOp {ar=1} fc n DoubleSqrt as = unaryPrimOp fc n DoubleType SqrtDouble as DoubleType

--     DoubleFloor : PrimFn 1
--     DoubleCeiling : PrimFn 1
--     Cast : Constant -> Constant -> PrimFn 1 -- What is the semantics for this? Check in the official backend.

--     BelieveMe : PrimFn 3
-- BeleiveMe should copy the data, but in referential transparance it is not needed.
compilePrimOp {ar=3} fc n BelieveMe [_,_,a] =
  pure (StgApp !(mkBinderIdVar fc n Core.stgRepType a) [] (SingleValue LiftedRep))

--     Crash : PrimFn 2 -- What are the parameters for this?
--     Use this FFI call to crash the haskell runtime.
--     https://github.com/grin-compiler/ghc-whole-program-compiler-project/blob/master/external-stg-interpreter/lib/Stg/Interpreter/FFI.hs#L178-L183
--     1b3f15ca69ea443031fa69a488c660a2c22182b8
compilePrimOp _ _ p as = pure $ StgApp (!(definedFunction "CRASH")) [ ] (SingleValue LiftedRep)
