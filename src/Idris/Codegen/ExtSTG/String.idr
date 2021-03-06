module Idris.Codegen.ExtSTG.String

import Core.Context
import Core.Core
import Core.TT
import Compiler.ANF
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

public export
litDataConId : UniqueMapRef => Ref Counter Int => Core DataConId
litDataConId = MkDataConId <$> uniqueForTerm STRING_TYPE_LIT_DATACON

public export
valDataConId : UniqueMapRef => Ref Counter Int => Core DataConId
valDataConId = MkDataConId <$> uniqueForTerm STRING_TYPE_VAL_DATACON

public export
idrisStringTyConId : UniqueMapRef => Ref Counter Int => Core TyConId
idrisStringTyConId = MkTyConId <$> uniqueForType STRING_TYPE_NAME

ADDR_TYPE_NAME : String
ADDR_TYPE_NAME = "Idris.String.Addr"

ADDR_DATACON_NAME : String
ADDR_DATACON_NAME = "Idris.String.Addr"

addrTyConId : UniqueMapRef => Ref Counter Int => Core TyConId
addrTyConId = MkTyConId <$> uniqueForType ADDR_TYPE_NAME

addrDataConId : UniqueMapRef => Ref Counter Int => Core DataConId
addrDataConId = MkDataConId <$> uniqueForTerm ADDR_DATACON_NAME

BYTEARRAY_TYPE_NAME : String
BYTEARRAY_TYPE_NAME = "Idris.String.ByteArray"

BYTEARRAY_DATACON_NAME : String
BYTEARRAY_DATACON_NAME = "Idris.String.ByteArray"

byteArrayTyConId : UniqueMapRef => Ref Counter Int => Core TyConId
byteArrayTyConId = MkTyConId <$> uniqueForType BYTEARRAY_TYPE_NAME

byteArrayDataConId : UniqueMapRef => Ref Counter Int => Core DataConId
byteArrayDataConId = MkDataConId <$> uniqueForTerm BYTEARRAY_DATACON_NAME

MBYTEARRAY_TYPE_NAME : String
MBYTEARRAY_TYPE_NAME = "Idris.String.MutableByteArray"

MBYTEARRAY_DATACON_NAME : String
MBYTEARRAY_DATACON_NAME = "Idris.String.MutableByteArray"

mutableByteArrayTyConId : UniqueMapRef => Ref Counter Int => Core TyConId
mutableByteArrayTyConId = MkTyConId <$> uniqueForType MBYTEARRAY_TYPE_NAME

mutableByteArrayDataConId : UniqueMapRef => Ref Counter Int => Core DataConId
mutableByteArrayDataConId = MkDataConId <$> uniqueForTerm MBYTEARRAY_DATACON_NAME

UNIT_TYPE_NAME : String
UNIT_TYPE_NAME = "Idris.String.Unit"

UNIT_DATACON_NAME : String
UNIT_DATACON_NAME = "Idris.String.Unit"

unitTyConId : UniqueMapRef => Ref Counter Int => Core TyConId
unitTyConId = MkTyConId <$> uniqueForType UNIT_TYPE_NAME

unitDataConId : UniqueMapRef => Ref Counter Int => Core DataConId
unitDataConId = MkDataConId <$> uniqueForTerm UNIT_DATACON_NAME

public export
defineStringTypes
  :  UniqueMapRef
  => Ref Counter Int
  => DataTypeMapRef
  => Ref ADTs ADTMap
  => Core ()
defineStringTypes = do
  define
    STRING_TYPE_NAME
    [ (STRING_TYPE_LIT_DATACON, AlgDataCon [LiftedRep]) -- Idris.String.Addr
    , (STRING_TYPE_VAL_DATACON, AlgDataCon [LiftedRep]) -- Idris.String.ByteArray
    ]
  define
    ADDR_TYPE_NAME
    [ (ADDR_DATACON_NAME, AlgDataCon [AddrRep]) ]
  define
    BYTEARRAY_TYPE_NAME
    [ (BYTEARRAY_DATACON_NAME, AlgDataCon [LiftedRep]) ]
  define
    MBYTEARRAY_TYPE_NAME
    [ (MBYTEARRAY_DATACON_NAME, AlgDataCon [LiftedRep]) ]
  define
    UNIT_TYPE_NAME
    [ (UNIT_DATACON_NAME, AlgDataCon []) ]
  where
    define : String -> List (String, DataConRep) -> Core ()
    define t ds = do
      st <- createSTyCon t ds
      _ <- traverse (registerInternalDataConToTyCon st . UN . fst) ds
      defineDataType (MkUnitId MAIN_UNIT) (MkModuleName MAIN_MODULE) st

unBox : SBinder -> DataConId -> TyConId -> SBinder -> SBinder -> PrimRep -> Expr -> Expr
unBox v1 d1 t1 cb v2 r e =
  StgCase (StgApp v1.Id [] (SingleValue r)) cb (AlgAlt t1)
  [ MkAlt (AltDataCon d1) [v2] e ]

topLevel : SBinder -> List SBinder -> Expr -> TopBinding
topLevel n as body = StgTopLifted $ StgNonRec n $ StgRhsClosure ReEntrant as $ body

nonused : UniqueMapRef => Ref Counter Int => Core SBinder
nonused = mkFreshSBinderStr LocalScope e "nonused"

indexWord8OffAddr : UniqueMapRef => Ref Counter Int => Core TopBinding
indexWord8OffAddr = do
  v1 <- mkSBinderLocalStr "Idris.String.indexWord8OffAddr1"
  v2 <- mkSBinderLocalStr "Idris.String.indexWord8OffAddr2"
  v3 <- mkSBinderLocalStr "Idris.String.indexWord8OffAddr3"
  v4 <- mkSBinderLocalStr "Idris.String.indexWord8OffAddr4"
  v5 <- mkSBinderLocalStr "Idris.String.indexWord8OffAddr4"
  v6 <- mkSBinderLocalStr "Idris.String.indexWord8OffAddr4"
  v7 <- mkSBinderLocalStr "Idris.String.indexWord8OffAddr4"
  pure
    $ topLevel !(mkSBinderTopLevel "Idris.String.indexWord8OffAddr") [v1,v2]
    $ unBox v1 !addrDataConId !addrTyConId v3 v4 LiftedRep
    $ unBox v2 !(dataConIdForConstant IntType) !(tyConIdForConstant IntType) v5 v6 LiftedRep
    $ StgCase (StgOpApp (StgPrimOp "GHC.Exts.indexWord8OffAddr#")
                        [StgVarArg v4.Id, StgVarArg v6.Id]
                        (SingleValue Word8Rep)
                        Nothing)
              v7 (PrimAlt Word8Rep)
      [ MkAlt AltDefault [] (StgConApp !(dataConIdForConstant Bits8Type) [StgVarArg v7.Id] []) ]

indexWord8Array : UniqueMapRef => Ref Counter Int => Core TopBinding
indexWord8Array = do
  arr     <- mkSBinderLocalStr "Idris.String.indexWord8Array1"
  i       <- mkSBinderLocalStr "Idris.String.indexWord8Array2"
  arrPrim <- mkSBinderLocalStr "Idris.String.indexWord8Array3"
  iPrim   <- mkSBinderLocalStr "Idris.String.indexWord8Array4"
  w       <- mkSBinderLocalStr "Idris.String.indexWord8Array5"
  pure
    $ topLevel !(mkSBinderTopLevel "Idris.String.indexWord8Array") [arr,i]
    $ unBox arr !byteArrayDataConId !byteArrayTyConId !nonused arrPrim AddrRep
    $ unBox i   !(dataConIdForConstant IntType) !(tyConIdForConstant IntType) !nonused iPrim IntRep
    $ StgCase (StgOpApp (StgPrimOp "GHC.Exts.indexWord8Array#")
                        [StgVarArg arrPrim.Id, StgVarArg iPrim.Id]
                        (SingleValue Word8Rep)
                        Nothing)
              w (PrimAlt Word8Rep)
      [ MkAlt AltDefault [] (StgConApp !(dataConIdForConstant Bits8Type) [StgVarArg w.Id] []) ]

sizeofByteArray : UniqueMapRef => Ref Counter Int => Core TopBinding
sizeofByteArray = do -- GHC.Exts.sizeofByteArray#
  v1 <- mkSBinderLocalStr "Idris.String.sizeofByteArray1"
  v2 <- mkSBinderLocalStr "Idris.String.sizeofByteArray2"
  v3 <- mkSBinderLocalStr "Idris.String.sizeofByteArray3"
  v4 <- mkSBinderLocalStr "Idris.String.sizeofByteArray4"
  pure
    $ StgTopLifted
    $ StgNonRec !(mkSBinderTopLevel "Idris.String.sizeofByteArray")
    $ StgRhsClosure ReEntrant [v1]
    $ StgCase (StgApp v1.Id [] (SingleValue LiftedRep)) v2 (AlgAlt !byteArrayTyConId)
      [ MkAlt (AltDataCon !byteArrayDataConId) [v3]
      $ StgCase (StgOpApp (StgPrimOp "GHC.Exts.sizeofByteArray#")
                          [StgVarArg v3.Id]
                          (SingleValue UnliftedRep)
                          Nothing)
                v4 (PrimAlt UnliftedRep) -- ByteArray#
        [ MkAlt AltDefault [] (StgConApp !(dataConIdForConstant IntType) [StgVarArg v4.Id] []) ]
      ]

-- Wrap an Addr# with (Idris.String.Val (Idris.String.Addr Addr#)) because String primitives are
-- implemented in ANF and compileANF compiles primitive types to Boxed types.
stringFromAddr : UniqueMapRef => Ref Counter Int => Core TopBinding
stringFromAddr = do
  v1 <- mkSBinderLocalStr "Idris.String.stringFromAddr1"
  v2 <- mkSBinderLocalStr "Idris.String.stringFromAddr2"
  pure
    $ StgTopLifted
    $ StgNonRec !(mkSBinderTopLevel "Idris.String.stringFromAddr")
    $ StgRhsClosure ReEntrant [v1]
    $ StgCase (StgConApp !addrDataConId [StgVarArg v1.Id] []) v2 (AlgAlt !addrTyConId)
      [ MkAlt AltDefault [] (StgConApp !litDataConId [StgVarArg v2.Id] []) ]

-- Creates a mutable byte array
newByteArray : UniqueMapRef => Ref Counter Int => Core TopBinding
newByteArray = do
  v1 <- mkSBinderLocalStr "Idris.String.newByteArray1"
  v2 <- mkSBinderLocalStr "Idris.String.newByteArray2"
  v3 <- mkSBinderLocalStr "Idris.String.newByteArray3"
  v4 <- mkSBinderLocalStr "Idris.String.newByteArray4"
  pure
    $ StgTopLifted
    $ StgNonRec !(mkSBinderTopLevel "Idris.String.newByteArray")
    $ StgRhsClosure ReEntrant [v1]
    $ StgCase (StgApp v1.Id [] (SingleValue LiftedRep)) v2 (AlgAlt !(tyConIdForConstant IntType))
      [ MkAlt (AltDataCon !(dataConIdForConstant IntType)) [v3]
      $ StgCase (StgOpApp (StgPrimOp "GHC.Exts.newByteArray#")
                          [StgVarArg v3.Id]
                          (SingleValue UnliftedRep)
                          Nothing)
                v4
                (PrimAlt LiftedRep) -- MutableByteArray has its own tag in GHC.
        [ MkAlt AltDefault [] (StgConApp !mutableByteArrayDataConId [StgVarArg v4.Id] []) ] --
      ]

copyAddrToByteArray : UniqueMapRef => Ref Counter Int => Core TopBinding
copyAddrToByteArray = do
  addr     <- mkSBinderLocalStr "Idris.String.copyAddrToByteArray1"
  marr     <- mkSBinderLocalStr "Idris.String.copyAddrToByteArray2"
  i        <- mkSBinderLocalStr "Idris.String.copyAddrToByteArray3"
  n        <- mkSBinderLocalStr "Idris.String.copyAddrToByteArray4"
  addrPrim <- mkSBinderLocalStr "Idris.String.copyAddrToByteArray5"
  marrPrim <- mkSBinderLocalStr "Idris.String.copyAddrToByteArray6"
  iPrim    <- mkSBinderLocalStr "Idris.String.copyAddrToByteArray7"
  nPrim    <- mkSBinderLocalStr "Idris.String.copyAddrToByteArray8"
  pure
    $ topLevel !(mkSBinderTopLevel "Idris.String.copyAddrToByteArray") [addr, marr, i, n]
    $ unBox addr !addrDataConId                  !addrTyConId                  !nonused addrPrim AddrRep
    $ unBox marr !mutableByteArrayDataConId      !mutableByteArrayTyConId      !nonused marrPrim LiftedRep
    $ unBox i    !(dataConIdForConstant IntType) !(tyConIdForConstant IntType) !nonused iPrim IntRep
    $ unBox n    !(dataConIdForConstant IntType) !(tyConIdForConstant IntType) !nonused nPrim IntRep
    $ StgCase (StgOpApp (StgPrimOp "GHC.Exts.copyAddrToByteArray#")
                        [StgVarArg addrPrim.Id, StgVarArg marrPrim.Id, StgVarArg iPrim.Id, StgVarArg nPrim.Id]
                        (UnboxedTuple [])
                        Nothing)
              !nonused
              (MultiValAlt 0) -- Unboxed tuple of arity 0
      [ MkAlt AltDefault [] (StgConApp !unitDataConId [] []) ]

writeByteArray : UniqueMapRef => Ref Counter Int => Core TopBinding
writeByteArray = do
  marr     <- mkSBinderLocalStr "Idris.String.writeByteArray1"
  i        <- mkSBinderLocalStr "Idris.String.writeByteArray2"
  w        <- mkSBinderLocalStr "Idris.String.writeByteArray3"
  marrPrim <- mkSBinderLocalStr "Idris.String.writeByteArray4"
  iPrim    <- mkSBinderLocalStr "Idris.String.writeByteArray5"
  wPrim    <- mkSBinderLocalStr "Idris.String.writeByteArray6"
  pure
    $ topLevel !(mkSBinderTopLevel "Idris.String.copyAddrToByteArray") [marr, i, w]
    $ unBox marr !mutableByteArrayDataConId      !mutableByteArrayTyConId      !nonused marrPrim LiftedRep
    $ unBox i    !(dataConIdForConstant IntType) !(tyConIdForConstant IntType) !nonused iPrim IntRep
    $ unBox w    !(dataConIdForConstant IntType) !(tyConIdForConstant IntType) !nonused wPrim Word8Rep
    $ StgCase (StgOpApp (StgPrimOp "GHC.Exts.writeByteArray#")
                        [StgVarArg marrPrim.Id, StgVarArg iPrim.Id, StgVarArg wPrim.Id]
                        (UnboxedTuple [])
                        Nothing)
              !nonused
              (MultiValAlt 0)
      [ MkAlt AltDefault [] (StgConApp !unitDataConId [] []) ]

unsafeFreezeByteArray : UniqueMapRef => Ref Counter Int => Core TopBinding
unsafeFreezeByteArray = do
  marr      <- mkSBinderLocalStr "Idris.String.unsafeFreezeByteArray1"
  marrPrim  <- mkSBinderLocalStr "Idris.String.unsafeFreezeByteArray2"
  arrResult <- mkSBinderLocalStr "Idris.String.unsafeFreezeByteArray3"
  pure
    $ topLevel !(mkSBinderTopLevel "Idris.String.unsafeFreezeByteArray") [marr]
    $ unBox marr !mutableByteArrayDataConId !mutableByteArrayTyConId !nonused marrPrim LiftedRep
    $ StgCase (StgOpApp (StgPrimOp "GHC.Exts.unsafeFreezeByteArray#")
                        [StgVarArg marrPrim.Id]
                        (SingleValue LiftedRep) -- ByteArray has its own internal tag
                        Nothing)
              arrResult
              (PrimAlt LiftedRep)
      [ MkAlt AltDefault [] (StgConApp !byteArrayDataConId [StgVarArg arrResult.Id] []) ]

copyToLitVal : (Name.Name, ANFDef)
copyToLitVal =
  ( UN "Idris.String.copyToLitVal"
  , MkAFun [0]
  $ ALet e 1 (APrimVal e (I 0))
  $ ALet e 2 (APrimVal e (I 1))
  $ ALet e 3 (APrimVal e (B8 0)) -- w
  $ ALet e 4 (AAppName e (UN "Idris.String.addrStrLength") (map ALocal [0,1])) -- s
  $ ALet e 5 (AOp      e (Add IntType) (map ALocal [4,2])) -- s + 1
  $ ALet e 6 (AAppName e (UN "Idris.String.newByteArray") [ALocal 5]) -- arr
  $ ALet e 7 (AAppName e (UN "Idris.String.copyAddrToByteArray") (map ALocal [0,6,1,4]))
  $ AAppName e (UN "Idris.String.writeArray") (map ALocal [6,4,3])
  )

indexWord8Str : (Name.Name, ANFDef)
indexWord8Str =
  ( UN "Idris.String.indexWord8Str"
  , MkAFun [0, 1]
  $ AConCase e (ALocal 0)
    [ MkAConAlt (UN "Idris.String.Lit") (Just 0) [2]
      $ AAppName e (UN "Idris.String.indexWord8OffAddr") [ALocal 2, ALocal 1]
    , MkAConAlt (UN "Idris.String.Val") (Just 1) [3]
      $ AAppName e (UN "Idris.String.indexWord8Arr") [ALocal 3, ALocal 1]
    ] Nothing
  )

strCompareGo : (Name.Name, ANFDef)
strCompareGo =
  ( UN "Idris.String.strCompareGo"
  , MkAFun [0,1,2,3,4] -- str1, str2, length1, length2, i
  $ ALet e 5 (AOp e (EQ IntType) (ALocal <$> [2,4]))
  $ AConstCase e (ALocal 5)
    [ -- True
      MkAConstAlt (I 1) $ AOp e (Sub IntType) (ALocal <$> [2, 3])
    , -- False
      MkAConstAlt (I 0)
      $ ALet e 6 (AOp e (EQ IntType) (ALocal <$> [3, 4]))
      $ AConstCase e (ALocal 6)
        [ -- True
          MkAConstAlt (I 1) $ AOp e (Sub IntType) (ALocal <$> [2, 3])
        , -- False
          MkAConstAlt (I 0)
          $ ALet e 7 (AAppName e (UN "Idris.String.indexWord8Str") [ALocal 0, ALocal 4])
          $ ALet e 8 (AAppName e (UN "Idris.String.indexWord8Str") [ALocal 1, ALocal 4])
          $ ALet e 9 (AOp e (EQ Bits8Type) (ALocal <$> [7,8]))
          $ AConstCase e (ALocal 9)
            [ MkAConstAlt (I 1)
              $ ALet e 10 (APrimVal e (I 1))
              $ ALet e 11 (AOp e (Add IntType) (ALocal <$> [4,10]))
              $ AAppName e (UN "Idris.String.strCompareGo") (ALocal <$> [0,1,2,3,11])
            , MkAConstAlt (I 0)
              $ ALet e 12 (AOp e (Cast Bits8Type IntType) [ALocal 7])
              $ ALet e 13 (AOp e (Cast Bits8Type IntType) [ALocal 8])
              $ AOp e (Sub IntType) (ALocal <$> [12, 13])
            ] Nothing
        ] Nothing
    ] Nothing
  )

strCompare : (Name.Name, ANFDef)
strCompare =
  ( UN "Idris.String.strCompare"
  , MkAFun [0,1] -- str1, str2
  $ ALet e 2 (AAppName e (UN "Idris.String.strLength") [ALocal 0])
  $ ALet e 3 (AAppName e (UN "Idris.String.strLength") [ALocal 1])
  $ ALet e 4 (APrimVal e (I 0))
  $ AAppName e (UN "Idris.String.strCompareGo") (ALocal <$> [0,1,2,3,4])
  )

strEq : (Name.Name, ANFDef)
strEq =
  ( UN "Idris.String.strEq"
  , MkAFun [0,1] -- str1, str2
  $ ALet e 3 (AAppName e (UN "Idris.String.strCompare") (ALocal <$> [0,1]))
  $ AConstCase e (ALocal 3)
    [ MkAConstAlt (I 0) $ APrimVal e (I 1)
    ] $ Just $ APrimVal e (I 0)
  )

-- copyLitToVal

public export
stgTopBindings : UniqueMapRef => Ref Counter Int => Core (List TopBinding)
stgTopBindings = traverse id
  [ indexWord8OffAddr
  , sizeofByteArray
  , stringFromAddr
  , copyAddrToByteArray
  , newByteArray
  , writeByteArray
  , unsafeFreezeByteArray
  ]

addrStrLength : (Name.Name, ANFDef)
addrStrLength =
  ( UN "Idris.String.addrStrLength"
  , MkAFun [0,1]
  $ ALet e 2 (AAppName e (UN "Idris.String.indexWord8OffAddr") [ALocal 0, ALocal 1]) -- ALocal0 should represent (Addr Addr#)
  $ AConstCase e (ALocal 2)
    [ MkAConstAlt (I 0) $ AV e (ALocal 1) ]
    $ Just
    $ ALet e 3 (APrimVal e (I 1))
    $ ALet e 4 (AOp e (Add IntType) [ALocal 1, ALocal 3])
    $ AAppName e (UN "Idris.String.addrStrLength") [ALocal 0, ALocal 4]
  )

strLength : (Name.Name, ANFDef)
strLength =
  ( UN "Idris.String.strLength"
  , MkAFun [0]
  $ AConCase e (ALocal 0)
    [ MkAConAlt (UN "Idris.String.Lit") (Just 0) [1]
      $ ALet e 2 (APrimVal e (I 0))
      $ AAppName e (UN "Idris.String.addrStrLength") [ALocal 1, ALocal 2]

    , MkAConAlt (UN "Idris.String.Val") (Just 1) [3]
      $ ALet e 4 (APrimVal e (I 1))
      $ ALet e 5 (AAppName e (UN "Idris.String.sizeofByteArray") [ALocal 3])
      $ AOp e (Sub IntType) [ALocal 5, ALocal 4]

    ] Nothing
  )

public export
topLevelANFDefs : List (Name.Name, ANFDef)
topLevelANFDefs =
  [ addrStrLength
  , copyToLitVal
  , indexWord8Str
  , strLength
  , strEq
  , strCompareGo
  , strCompare
  ]
