module Idris.Codegen.ExtSTG.String

import Core.Context
import Core.Core
import Core.TT
import Compiler.ANF
import Data.HVect
import Data.Vect
import Idris.Codegen.ExtSTG.STG
import Idris.Codegen.ExtSTG.Core
import Idris.Codegen.ExtSTG.ADTMap

{-
This module contains the ANF implementation of the String handling primitives.

The high level Haskell codes can be found in the IdristString.hs file.

STG Samples:
https://github.com/csabahruska/manual-stg-experiment/blob/master/StgSample.hs
-}

{-
[x] Str(..)
[x] strEq
[x] strCompare
[x] strLength
[ ] strHead
[ ] strTail
[ ] strIndex
[ ] strCons
[ ] strAppend
[ ] strReverse
[ ] strSubstr

[ ] Fix the Rep types in STG definitions
-}

{-
Values are primitive values and are represented as boxed STG values.
-}


e : FC
e = EmptyFC

STRING_TYPE_NAME : String
STRING_TYPE_NAME = "Idris.String"

STRING_TYPE_LIT_DATACON : String
STRING_TYPE_LIT_DATACON = "Idris.String.Lit"

STRING_TYPE_VAL_DATACON : String
STRING_TYPE_VAL_DATACON = "Idris.String.Val"

litDataConId :  DataTypeMapRef => UniqueMapRef => Ref Counter Int => Core (DataConId (AlgDataCon [LiftedRep]))
litDataConId = do
  dataConId <- mkDataConIdStr STRING_TYPE_LIT_DATACON
  checkDataCon "litDataConId" (AlgDataCon [LiftedRep]) dataConId

valDataConId : DataTypeMapRef => UniqueMapRef => Ref Counter Int => Core DataConIdSg
valDataConId = mkDataConIdStr STRING_TYPE_VAL_DATACON

valDataConId2
  : DataTypeMapRef => UniqueMapRef => Ref Counter Int
  => Core (DataConId (AlgDataCon [LiftedRep]))
valDataConId2 = do
  dataConId <- mkDataConIdStr STRING_TYPE_VAL_DATACON
  checkDataCon "valDataConId2" (AlgDataCon [LiftedRep]) dataConId

export
idrisStringTyConId : DataTypeMapRef => UniqueMapRef => Ref Counter Int => Core TyConId
idrisStringTyConId = mkTyConIdStr STRING_TYPE_NAME

ADDR_TYPE_NAME : String
ADDR_TYPE_NAME = "Idris.String.Addr"

ADDR_DATACON_NAME : String
ADDR_DATACON_NAME = "Idris.String.Addr"

addrTyConId : DataTypeMapRef => UniqueMapRef => Ref Counter Int => Core TyConId
addrTyConId = mkTyConIdStr ADDR_TYPE_NAME

addrDataConId
  :  DataTypeMapRef
  => UniqueMapRef
  => Ref Counter Int
  => Core (DataConId (AlgDataCon [AddrRep]))
addrDataConId = do
  dataConId <- mkDataConIdStr ADDR_DATACON_NAME
  checkDataCon "addrDataConId" (AlgDataCon [AddrRep]) dataConId

BYTEARRAY_TYPE_NAME : String
BYTEARRAY_TYPE_NAME = "Idris.String.ByteArray"

BYTEARRAY_DATACON_NAME : String
BYTEARRAY_DATACON_NAME = "Idris.String.ByteArray"

byteArrayTyConId : DataTypeMapRef => UniqueMapRef => Ref Counter Int => Core TyConId
byteArrayTyConId = mkTyConIdStr BYTEARRAY_TYPE_NAME

byteArrayDataConId
  :  DataTypeMapRef
  => UniqueMapRef
  => Ref Counter Int
  => Core (DataConId (AlgDataCon [UnliftedRep]))
byteArrayDataConId = do
  dataConId <- mkDataConIdStr BYTEARRAY_DATACON_NAME
  checkDataCon "byteArrayDataConId" (AlgDataCon [UnliftedRep]) dataConId

MBYTEARRAY_TYPE_NAME : String
MBYTEARRAY_TYPE_NAME = "Idris.String.MutableByteArray"

MBYTEARRAY_DATACON_NAME : String
MBYTEARRAY_DATACON_NAME = "Idris.String.MutableByteArray"

mutableByteArrayTyConId : DataTypeMapRef => UniqueMapRef => Ref Counter Int => Core TyConId
mutableByteArrayTyConId = mkTyConIdStr MBYTEARRAY_TYPE_NAME

mutableByteArrayDataConId
  :  DataTypeMapRef
  => UniqueMapRef
  => Ref Counter Int
  => Core (DataConId (AlgDataCon [UnliftedRep]))
mutableByteArrayDataConId = do
  dataConId <- mkDataConIdStr MBYTEARRAY_DATACON_NAME
  checkDataCon "mutableByteArrayDataConId" (AlgDataCon [UnliftedRep]) dataConId

UNIT_TYPE_NAME : String
UNIT_TYPE_NAME = "Idris.String.Unit"

UNIT_DATACON_NAME : String
UNIT_DATACON_NAME = "Idris.String.Unit"

export
unitTyConId : DataTypeMapRef => UniqueMapRef => Ref Counter Int => Core TyConId
unitTyConId = mkTyConIdStr UNIT_TYPE_NAME

export
unitDataConId : DataTypeMapRef => UniqueMapRef => Ref Counter Int => Core (DataConId (AlgDataCon []))
unitDataConId = do
  dataConId <- mkDataConIdStr UNIT_DATACON_NAME
  checkDataCon "unitDataConId" (AlgDataCon []) dataConId

export
defineStringTypes
  :  UniqueMapRef
  => Ref Counter Int
  => DataTypeMapRef
  => Ref ADTs ADTMap
  => Core ()
defineStringTypes = do
  let noSpan = SsUnhelpfulSpan
  define
    (STRING_TYPE_NAME, noSpan STRING_TYPE_NAME)
    [ (STRING_TYPE_LIT_DATACON, AlgDataCon [LiftedRep], noSpan STRING_TYPE_LIT_DATACON) -- Idris.String.Addr
    , (STRING_TYPE_VAL_DATACON, AlgDataCon [LiftedRep], noSpan STRING_TYPE_VAL_DATACON) -- Idris.String.ByteArray
    ]
  define
    (ADDR_TYPE_NAME, noSpan ADDR_TYPE_NAME)
    [ (ADDR_DATACON_NAME, AlgDataCon [AddrRep], noSpan ADDR_DATACON_NAME) ]
  define
    (BYTEARRAY_TYPE_NAME, noSpan BYTEARRAY_TYPE_NAME)
    [ (BYTEARRAY_DATACON_NAME, AlgDataCon [ByteArrayRep], noSpan BYTEARRAY_DATACON_NAME) ]
  define
    (MBYTEARRAY_TYPE_NAME, noSpan MBYTEARRAY_TYPE_NAME)
    [ (MBYTEARRAY_DATACON_NAME, AlgDataCon [ByteArrayRep], noSpan MBYTEARRAY_DATACON_NAME) ]
  define
    (UNIT_TYPE_NAME, noSpan UNIT_TYPE_NAME)
    [ (UNIT_DATACON_NAME, AlgDataCon [], noSpan UNIT_DATACON_NAME) ]
  where
    define : (STG.Name, SrcSpan) -> List (STG.Name, DataConRep, SrcSpan) -> Core ()
    define t ds = do
      st <- createSTyCon t ds
      _ <- traverse (registerInternalDataConToTyCon st . UN . fst) ds
      defineDataType (MkUnitId MAIN_UNIT) (MkModuleName MAIN_MODULE) st

indexWord8OffAddr : DataTypeMapRef => UniqueMapRef => Ref Counter Int => Core TopBinding
indexWord8OffAddr = do
  ad1 <- addrDataConId
  ti1 <- dataConIdForConstant IntType
  ((AlgDataCon [IntRep]) ** ti1) <- dataConIdForConstant IntType
    | wrongRep => coreFail $ InternalError $ "DataConId has wrong RepType: " ++ show ("indexWord8OffAddr2", wrongRep)
  v1 <- mkSBinderLocalStr "Idris.String.indexWord8OffAddr1"
  v2 <- mkSBinderLocalStr "Idris.String.indexWord8OffAddr2"
  v3 <- mkSBinderLocalStr "Idris.String.indexWord8OffAddr3"
  v4 <- mkSBinderRepLocalStr (SingleValue AddrRep) "Idris.String.indexWord8OffAddr4"
  v5 <- mkSBinderLocalStr "Idris.String.indexWord8OffAddr4"
  v6 <- mkSBinderRepLocalStr (SingleValue IntRep)   "Idris.String.indexWord8OffAddr4"
  v7 <- mkSBinderRepLocalStr (SingleValue Word8Rep) "Idris.String.indexWord8OffAddr4"
  pure
    $ topLevel !(mkSBinderTopLevel "Idris.String.indexWord8OffAddr") [mkSBinderSg v1,mkSBinderSg v2]
    $ unBox v1 ad1 !addrTyConId v3 v4
    $ unBox v2 ti1 !(tyConIdForConstant IntType) v5 v6
    $ StgCase
        (PrimAlt Word8Rep)
        (StgOpApp IndexWord8OffAddr [StgVarArg $ binderId v4, StgVarArg $ binderId v6])
        v7
        [ MkAlt AltDefault () (StgConApp !(dataConIdRepForConstant Word8Rep Bits8Type) (StgVarArg (binderId v7))) ]

indexWord8Array : DataTypeMapRef => UniqueMapRef => Ref Counter Int => Core TopBinding
indexWord8Array = do
  ((AlgDataCon [IntRep]) ** ad1) <- dataConIdForConstant IntType
    | wrongRep => coreFail $ InternalError $ "DataConId has wrong RepType: " ++ show ("indexWord8Array1", wrongRep)
  ba1     <- byteArrayDataConId
  arr     <- mkSBinderLocalStr "Idris.String.indexWord8Array1"
  i       <- mkSBinderLocalStr "Idris.String.indexWord8Array2"
  arrPrim <- mkSBinderRepLocalStr (SingleValue ByteArrayRep) "Idris.String.indexWord8Array3"
  iPrim   <- mkSBinderRepLocalStr (SingleValue IntRep)       "Idris.String.indexWord8Array4"
  w       <- mkSBinderRepLocalStr (SingleValue Word8Rep)     "Idris.String.indexWord8Array5"
  pure
    $ topLevel !(mkSBinderTopLevel "Idris.String.indexWord8Array") [mkSBinderSg arr,mkSBinderSg i]
    $ unBox arr ba1 !byteArrayTyConId !nonused arrPrim
    $ unBox i   ad1 !(tyConIdForConstant IntType) !nonused iPrim
    $ StgCase
        (PrimAlt Word8Rep)
        (StgOpApp IndexWord8Array [StgVarArg $ binderId arrPrim, StgVarArg $ binderId iPrim])
        w
        [ MkAlt AltDefault () (StgConApp !(dataConIdRepForConstant Word8Rep Bits8Type) (StgVarArg (binderId w))) ]

sizeofByteArray : DataTypeMapRef => UniqueMapRef => Ref Counter Int => Core TopBinding
sizeofByteArray = do -- GHC.Exts.sizeofByteArray#
  v1 <- mkSBinderLocalStr "Idris.String.sizeofByteArray1"
  v2 <- mkSBinderLocalStr "Idris.String.sizeofByteArray2"
  v3 <- mkSBinderRepLocalStr (SingleValue UnliftedRep) "Idris.String.sizeofByteArray3"
  v4 <- mkSBinderRepLocalStr (SingleValue IntRep)      "Idris.String.sizeofByteArray4"
  ba1 <- byteArrayDataConId
  pure
    $ StgTopLifted
    $ StgNonRec !(mkSBinderTopLevel "Idris.String.sizeofByteArray")
    $ StgRhsClosure ReEntrant [mkSBinderSg v1]
    $ StgCase (AlgAlt !byteArrayTyConId) (StgApp (binderId v1) [] (SingleValue LiftedRep)) v2
      [ MkAlt (AltDataCon (mkDataConIdSg ba1)) v3
      $ StgCase
          (PrimAlt IntRep)
          (StgOpApp SizeOfByteArray (StgVarArg (binderId v3)))
          v4
          [ MkAlt AltDefault () (StgConApp !(dataConIdRepForConstant IntRep IntType) (StgVarArg (binderId v4))) ]
      ]

export
STRING_FROM_ADDR : String
STRING_FROM_ADDR = "Idris.String.stringFromAddr"

export
stringFromAddrBinderId2 : UniqueMapRef => Ref Counter Int => Core (BinderId (SingleValue LiftedRep))
stringFromAddrBinderId2 = mkBinderIdStr STRING_FROM_ADDR

export
stringFromAddrBinderId : UniqueMapRef => Ref Counter Int => Core BinderIdSg
stringFromAddrBinderId = map mkBinderIdSg $ mkBinderIdStr STRING_FROM_ADDR

-- Wrap an Addr# with (Idris.String.Val (Idris.String.Addr Addr#)) because String primitives are
-- implemented in ANF and compileANF compiles primitive types to Boxed types.
stringFromAddr : DataTypeMapRef => UniqueMapRef => Ref Counter Int => Core TopBinding
stringFromAddr = do
  v1 <- mkSBinderRepLocalStr (SingleValue AddrRep) "Idris.String.stringFromAddr1"
  v2 <- mkSBinderLocalStr "Idris.String.stringFromAddr2"
  pure
    $ StgTopLifted
    $ StgNonRec !(mkSBinderTopLevel STRING_FROM_ADDR)
    $ StgRhsClosure ReEntrant [mkSBinderSg v1]
    $ StgCase (AlgAlt !addrTyConId) (StgConApp !addrDataConId (StgVarArg (binderId v1))) v2
      [ MkAlt AltDefault () (StgConApp !litDataConId (StgVarArg (binderId v2))) ]

export
ADDR_FROM_STRING : String
ADDR_FROM_STRING = "Idris.String.addrFromString"

export
addrFromStringBinderId : UniqueMapRef => Ref Counter Int => Core (BinderId (SingleValue LiftedRep))
addrFromStringBinderId = mkBinderIdStr ADDR_FROM_STRING

addrFromString : DataTypeMapRef => UniqueMapRef => Ref Counter Int => Core TopBinding
addrFromString = do
  arg <- mkSBinderLocalStr "Idris.String.addrFromString1"
  litBinder <- mkSBinderLocalStr "Idris.String.addrFromString2"
  addrBinder <- mkSBinderRepLocalStr (SingleValue AddrRep) "Idris.String.addrFromString3"
  valBinder <- mkSBinderLocalStr "Idris.String.addFromString4"
  byteArrayBinder <- mkSBinderRepLocalStr (SingleValue ByteArrayRep) "Idris.String.addrFromString5"
  pure
    $ StgTopLifted
    $ StgNonRec !(mkSBinderTopLevel ADDR_FROM_STRING)
    $ StgRhsClosure ReEntrant [mkSBinderSg arg]
    $ StgCase
        (AlgAlt !idrisStringTyConId)
        (StgApp
          (binderId (the (SBinder (SingleValue LiftedRep)) arg))
          []
          (SingleValue LiftedRep))
        !nonused
        [ -- Lit
          MkAlt (AltDataCon (mkDataConIdSg !litDataConId)) litBinder
                (StgCase
                  (AlgAlt !addrTyConId)
                  (StgApp
                    (binderId (the (SBinder (SingleValue LiftedRep)) litBinder))
                    []
                    (SingleValue LiftedRep))
                  !nonused
                  [ MkAlt (AltDataCon (mkDataConIdSg !addrDataConId)) addrBinder
                          (StgApp (binderId addrBinder) [] (SingleValue AddrRep))
                  ])
        ,
          MkAlt (AltDataCon (mkDataConIdSg !valDataConId2)) valBinder
                (StgCase
                  (AlgAlt !byteArrayTyConId)
                  (StgApp
                    (binderId (the (SBinder (SingleValue LiftedRep)) valBinder))
                    []
                    (SingleValue LiftedRep))
                  !nonused
                  [ MkAlt (AltDataCon (mkDataConIdSg !byteArrayDataConId)) byteArrayBinder
                          (StgOpApp ByteArrayContents (StgVarArg (binderId byteArrayBinder)))
                  ])
        ]

-- Creates a mutable byte array
newByteArray : DataTypeMapRef => UniqueMapRef => Ref Counter Int => Core TopBinding
newByteArray = do
  v1 <- mkSBinderLocalStr "Idris.String.newByteArray1"
  v2 <- mkSBinderLocalStr "Idris.String.newByteArray2"
  v3 <- mkSBinderRepLocalStr (SingleValue IntRep) "Idris.String.newByteArray3"
  v4 <- mkSBinderRepLocalStr (SingleValue UnliftedRep) "Idris.String.newByteArray4"
  da1 <- dataConIdRepForConstant IntRep IntType
  pure
    $ StgTopLifted
    $ StgNonRec !(mkSBinderTopLevel "Idris.String.newByteArray")
    $ StgRhsClosure ReEntrant [mkSBinderSg v1]
    $ StgCase (AlgAlt !(tyConIdForConstant IntType)) (StgApp (binderId v1) [] (SingleValue LiftedRep)) v2
      [ MkAlt (AltDataCon (mkDataConIdSg da1)) v3
      $ StgCase
          (PrimAlt MutableByteArrayRep) -- MutableByteArray has its own tag in GHC.
          (StgOpApp NewByteArray (StgVarArg (binderId v3)))
          v4
          [ MkAlt AltDefault () (StgConApp !mutableByteArrayDataConId (StgVarArg (binderId v4))) ]
      ]

copyAddrToByteArray : DataTypeMapRef => UniqueMapRef => Ref Counter Int => Core TopBinding
copyAddrToByteArray = do
  addr     <- mkSBinderLocalStr "Idris.String.copyAddrToByteArray1"
  marr     <- mkSBinderLocalStr "Idris.String.copyAddrToByteArray2"
  i        <- mkSBinderLocalStr "Idris.String.copyAddrToByteArray3"
  n        <- mkSBinderLocalStr "Idris.String.copyAddrToByteArray4"
  addrPrim <- mkSBinderRepLocalStr (SingleValue AddrRep) "Idris.String.copyAddrToByteArray5"
  marrPrim <- mkSBinderRepLocalStr (SingleValue UnliftedRep) "Idris.String.copyAddrToByteArray6"
  iPrim    <- mkSBinderRepLocalStr (SingleValue IntRep) "Idris.String.copyAddrToByteArray7"
  nPrim    <- mkSBinderRepLocalStr (SingleValue IntRep) "Idris.String.copyAddrToByteArray8"
  ad1 <- addrDataConId
  m1  <- mutableByteArrayDataConId
  ti1 <- dataConIdRepForConstant IntRep IntType
  pure
    $ topLevel !(mkSBinderTopLevel "Idris.String.copyAddrToByteArray")
               [ mkSBinderSg addr
               , mkSBinderSg marr
               , mkSBinderSg i
               , mkSBinderSg n
               ]
    $ unBox addr ad1 !addrTyConId !nonused addrPrim
    $ unBox marr  m1 !mutableByteArrayTyConId !nonused marrPrim
    $ unBox i    ti1 !(tyConIdForConstant IntType) !nonused iPrim
    $ unBox n    ti1 !(tyConIdForConstant IntType) !nonused nPrim
    $ StgCase
        (MultiValAlt 0) -- Unboxed tuple of arity 0
        (StgOpApp
          CopyAddrToByteArray
            [ StgVarArg $ binderId addrPrim
            , StgVarArg $ binderId marrPrim
            , StgVarArg $ binderId iPrim
            , StgVarArg $ binderId nPrim ])
        !(nonusedRep (SingleValue VoidRep))
        [ MkAlt AltDefault () (StgConApp !unitDataConId ()) ]

writeByteArray : DataTypeMapRef => UniqueMapRef => Ref Counter Int => Core TopBinding
writeByteArray = do
  marr     <- mkSBinderLocalStr "Idris.String.writeByteArray1"
  i        <- mkSBinderLocalStr "Idris.String.writeByteArray2"
  w        <- mkSBinderLocalStr "Idris.String.writeByteArray3"
  marrPrim <- mkSBinderRepLocalStr (SingleValue UnliftedRep) "Idris.String.writeByteArray4"
  iPrim    <- mkSBinderRepLocalStr (SingleValue IntRep)      "Idris.String.writeByteArray5"
  wPrim    <- mkSBinderRepLocalStr (SingleValue Word8Rep)    "Idris.String.writeByteArray6"
  m1  <- mutableByteArrayDataConId
  ti1 <- dataConIdRepForConstant IntRep IntType
  ti2 <- dataConIdRepForConstant Word8Rep CharType
  pure
    $ topLevel !(mkSBinderTopLevel "Idris.String.writeByteArray")
               [mkSBinderSg marr, mkSBinderSg i, mkSBinderSg w]
    $ unBox marr m1 !mutableByteArrayTyConId      !nonused marrPrim
    $ unBox i   ti1 !(tyConIdForConstant IntType) !nonused iPrim
    $ unBox w   ti2 !(tyConIdForConstant CharType) !nonused wPrim
    $ StgCase
        (MultiValAlt 0)
        (StgOpApp
          WriteWord8Array
          [ StgVarArg $ binderId marrPrim
          , StgVarArg $ binderId iPrim
          , StgVarArg $ binderId wPrim
          ])
        !(nonusedRep (SingleValue VoidRep))
        [ MkAlt AltDefault () (StgConApp !unitDataConId ()) ]

unsafeFreezeByteArray : DataTypeMapRef => UniqueMapRef => Ref Counter Int => Core TopBinding
unsafeFreezeByteArray = do
  marr      <- mkSBinderLocalStr "Idris.String.unsafeFreezeByteArray1"
  marrPrim  <- mkSBinderRepLocalStr (SingleValue UnliftedRep) "Idris.String.unsafeFreezeByteArray2"
  arrResult <- mkSBinderRepLocalStr (SingleValue UnliftedRep) "Idris.String.unsafeFreezeByteArray3"
  ma <- mutableByteArrayDataConId
  pure
    $ topLevel !(mkSBinderTopLevel "Idris.String.unsafeFreezeByteArray") [mkSBinderSg marr]
    $ unBox marr ma !mutableByteArrayTyConId !nonused marrPrim
    $ StgCase
        (PrimAlt ByteArrayRep)
        (StgOpApp UnsafeFreezeByteArray (StgVarArg (binderId marrPrim)))
        arrResult
        [ MkAlt AltDefault () (StgConApp !byteArrayDataConId (StgVarArg (binderId arrResult))) ]

export
stgTopBindings : DataTypeMapRef => UniqueMapRef => Ref Counter Int => Core (List TopBinding)
stgTopBindings = traverse id
  [
--  copyAddrToByteArray
--  , indexWord8Array -- TODO: Bring them back
--  , indexWord8OffAddr
--  , newByteArray
--  , sizeofByteArray
    stringFromAddr
  , addrFromString
--  , unsafeFreezeByteArray
--  , writeByteArray
  ]

copyToLitVal : (Name.Name, ANFDef)
copyToLitVal =
  ( UN "Idris.String.copyToLitVal"
  , MkAFun [0]
  $ ALet e 1 (APrimVal e (I 0))
  $ ALet e 2 (APrimVal e (I 1))
  $ ALet e 3 (APrimVal e (B8 0)) -- w
  $ ALet e 4 (AAppName e Nothing (UN "Idris.String.addrStrLength") (map ALocal [0,1])) -- s
  $ ALet e 5 (AOp      e Nothing (Add IntType) (map ALocal [4,2])) -- s + 1
  $ ALet e 6 (AAppName e Nothing (UN "Idris.String.newByteArray") [ALocal 5]) -- arr
  $ ALet e 7 (AAppName e Nothing (UN "Idris.String.copyAddrToByteArray") (map ALocal [0,6,1,4]))
  $ AAppName e Nothing (UN "Idris.String.writeByteArray") (map ALocal [6,4,3])
  )

indexWord8Str : (Name.Name, ANFDef)
indexWord8Str =
  ( UN "Idris.String.indexWord8Str"
  , MkAFun [0, 1]
  $ AConCase e (ALocal 0)
    [ MkAConAlt (UN "Idris.String.Lit") (Just 0) [2]
      $ AAppName e Nothing (UN "Idris.String.indexWord8OffAddr") [ALocal 2, ALocal 1]
    , MkAConAlt (UN "Idris.String.Val") (Just 1) [3]
      $ AAppName e Nothing (UN "Idris.String.indexWord8Array") [ALocal 3, ALocal 1]
    ] Nothing
  )

strCompareGo : (Name.Name, ANFDef)
strCompareGo =
  ( UN "Idris.String.strCompareGo"
  , MkAFun [0,1,2,3,4] -- str1, str2, length1, length2, i
  $ ALet e 5 (AOp e Nothing (EQ IntType) (ALocal <$> [2,4]))
  $ AConstCase e (ALocal 5)
    [ -- True
      MkAConstAlt (I 1) $ AOp e Nothing (Sub IntType) (ALocal <$> [2, 3])
    , -- False
      MkAConstAlt (I 0)
      $ ALet e 6 (AOp e Nothing (EQ IntType) (ALocal <$> [3, 4]))
      $ AConstCase e (ALocal 6)
        [ -- True
          MkAConstAlt (I 1) $ AOp e Nothing (Sub IntType) (ALocal <$> [2, 3])
        , -- False
          MkAConstAlt (I 0)
          $ ALet e 7 (AAppName e Nothing (UN "Idris.String.indexWord8Str") [ALocal 0, ALocal 4])
          $ ALet e 8 (AAppName e Nothing (UN "Idris.String.indexWord8Str") [ALocal 1, ALocal 4])
          $ ALet e 9 (AOp      e Nothing (EQ Bits8Type) (ALocal <$> [7,8]))
          $ AConstCase e (ALocal 9)
            [ MkAConstAlt (I 1)
              $ ALet e 10 (APrimVal e (I 1))
              $ ALet e 11 (AOp e Nothing (Add IntType) (ALocal <$> [4,10]))
              $ AAppName e Nothing (UN "Idris.String.strCompareGo") (ALocal <$> [0,1,2,3,11])
            , MkAConstAlt (I 0)
              $ ALet e 12 (AOp e Nothing (Cast Bits8Type IntType) [ALocal 7])
              $ ALet e 13 (AOp e Nothing (Cast Bits8Type IntType) [ALocal 8])
              $ AOp e Nothing (Sub IntType) (ALocal <$> [12, 13])
            ] Nothing
        ] Nothing
    ] Nothing
  )

strCompare : (Name.Name, ANFDef)
strCompare =
  ( UN "Idris.String.strCompare"
  , MkAFun [0,1] -- str1, str2
  $ ALet e 2 (AAppName e Nothing (UN "Idris.String.strLength") [ALocal 0])
  $ ALet e 3 (AAppName e Nothing (UN "Idris.String.strLength") [ALocal 1])
  $ ALet e 4 (APrimVal e (I 0))
  $ AAppName e Nothing (UN "Idris.String.strCompareGo") (ALocal <$> [0,1,2,3,4])
  )

strEq : (Name.Name, ANFDef)
strEq =
  ( UN "Idris.String.strEq"
  , MkAFun [0,1] -- str1, str2
  $ ALet e 3 (AAppName e Nothing (UN "Idris.String.strCompare") (ALocal <$> [0,1]))
  $ AConstCase e (ALocal 3)
    [ MkAConstAlt (I 0) $ APrimVal e (I 1)
    ] $ Just $ APrimVal e (I 0)
  )

addrStrLength : (Name.Name, ANFDef)
addrStrLength =
  ( UN "Idris.String.addrStrLength"
  , MkAFun [0,1]
  $ ALet e 2 (AAppName e Nothing (UN "Idris.String.indexWord8OffAddr") [ALocal 0, ALocal 1]) -- ALocal0 should represent (Addr Addr#)
  $ AConstCase e (ALocal 2)
    [ MkAConstAlt (I 0) $ AV e (ALocal 1) ]
    $ Just
    $ ALet e 3 (APrimVal e (I 1))
    $ ALet e 4 (AOp e Nothing (Add IntType) [ALocal 1, ALocal 3])
    $ AAppName e Nothing (UN "Idris.String.addrStrLength") [ALocal 0, ALocal 4]
  )

strLength : (Name.Name, ANFDef)
strLength =
  ( UN "Idris.String.strLength"
  , MkAFun [0]
  $ AConCase e (ALocal 0)
    [ MkAConAlt (UN "Idris.String.Lit") (Just 0) [1]
      $ ALet e 2 (APrimVal e (I 0))
      $ AAppName e Nothing (UN "Idris.String.addrStrLength") [ALocal 1, ALocal 2]

    , MkAConAlt (UN "Idris.String.Val") (Just 1) [3]
      $ ALet e 4 (APrimVal e (I 1))
      $ ALet e 5 (AAppName e Nothing (UN "Idris.String.sizeofByteArray") [ALocal 3])
      $ AOp e Nothing (Sub IntType) [ALocal 5, ALocal 4]

    ] Nothing
  )

export
topLevelANFDefs : List (Name.Name, ANFDef)
topLevelANFDefs = []
{- --TODO: Bring them back
  [ addrStrLength
  , copyToLitVal
  , indexWord8Str
  , strCompare
  , strCompareGo
  , strEq
  , strLength
  ]
-}
