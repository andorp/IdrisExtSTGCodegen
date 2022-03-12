module Idris.Codegen.ExtSTG.Representation

import Idris.Codegen.ExtSTG.Prelude

public export
data PrimElemRep
  = Int8ElemRep
  | Int16ElemRep
  | Int32ElemRep
  | Int64ElemRep
  | Word8ElemRep
  | Word16ElemRep
  | Word32ElemRep
  | Word64ElemRep
  | FloatElemRep
  | DoubleElemRep

export
SemiDecEq PrimElemRep where
  semiDecEq Int8ElemRep   Int8ElemRep   = Just Refl
  semiDecEq Int16ElemRep  Int16ElemRep  = Just Refl
  semiDecEq Int32ElemRep  Int32ElemRep  = Just Refl
  semiDecEq Int64ElemRep  Int64ElemRep  = Just Refl
  semiDecEq Word8ElemRep  Word8ElemRep  = Just Refl
  semiDecEq Word16ElemRep Word16ElemRep = Just Refl
  semiDecEq Word32ElemRep Word32ElemRep = Just Refl
  semiDecEq Word64ElemRep Word64ElemRep = Just Refl
  semiDecEq FloatElemRep  FloatElemRep  = Just Refl
  semiDecEq DoubleElemRep DoubleElemRep = Just Refl
  semiDecEq _ _ = Nothing

Show PrimElemRep where
  show Int8ElemRep   = "Int8ElemRep"
  show Int16ElemRep  = "Int16ElemRep"
  show Int32ElemRep  = "Int32ElemRep"
  show Int64ElemRep  = "Int64ElemRep"
  show Word8ElemRep  = "Word8ElemRep"
  show Word16ElemRep = "Word16ElemRep"
  show Word32ElemRep = "Word32ElemRep"
  show Word64ElemRep = "Word64ElemRep"
  show FloatElemRep  = "FloatElemRep"
  show DoubleElemRep = "DoubleElemRep"

public export
data PrimRep
  = VoidRep
  | LiftedRep   -- Boxed, in thunk or WHNF
  | UnliftedRep -- Boxed, in WHNF
  | Int8Rep     -- Unboxed, Signed, 8 bits value
  | Int16Rep    -- Unboxed, Signed, 16 bits value
  | Int32Rep    -- Unboxed, Signed, 32 bits value
  | Int64Rep    -- Unboxed, Signed, 64 bits value (with 32 bits words only)
  | IntRep      -- Unboxed, Signed, word-sized value
  | Word8Rep    -- Unboxed, Unsigned, 8 bits value
  | Word16Rep   -- Unboxed, Unsigned, 16 bits value
  | Word32Rep   -- Unboxed, Unsigned, 32 bits value
  | Word64Rep   -- Unboxed, Unisgned, 64 bits value (with 32 bits words only)
  | WordRep     -- Unboxed, Unisgned, word-sized value
  | AddrRep     -- A pointer, but *not* a Haskell value. Use (Un)liftedRep
  | FloatRep
  | DoubleRep
  | VecRep Nat PrimElemRep -- A vector

-- MutableByteArray has its own tag in GHC.

public export
ByteArrayRep : PrimRep
ByteArrayRep = UnliftedRep

public export
MutableByteArrayRep : PrimRep
MutableByteArrayRep = UnliftedRep

public export
CharRep : PrimRep
CharRep = Word8Rep

export
SemiDecEq PrimRep where
  semiDecEq VoidRep     VoidRep     = Just Refl
  semiDecEq LiftedRep   LiftedRep   = Just Refl
  semiDecEq UnliftedRep UnliftedRep = Just Refl
  semiDecEq Int8Rep     Int8Rep     = Just Refl
  semiDecEq Int16Rep    Int16Rep    = Just Refl
  semiDecEq Int32Rep    Int32Rep    = Just Refl
  semiDecEq Int64Rep    Int64Rep    = Just Refl
  semiDecEq IntRep      IntRep      = Just Refl
  semiDecEq Word8Rep    Word8Rep    = Just Refl
  semiDecEq Word16Rep   Word16Rep   = Just Refl
  semiDecEq Word32Rep   Word32Rep   = Just Refl
  semiDecEq Word64Rep   Word64Rep   = Just Refl
  semiDecEq WordRep     WordRep     = Just Refl
  semiDecEq AddrRep     AddrRep     = Just Refl
  semiDecEq FloatRep    FloatRep    = Just Refl
  semiDecEq DoubleRep   DoubleRep   = Just Refl
  semiDecEq (VecRep n1 p1) (VecRep n2 p2) = do
    Refl <- semiDecEq p1 p2
    Refl <- semiDecEq n1 n2
    Just Refl
  semiDecEq _ _ = Nothing

export
Show PrimRep where
  showPrec d VoidRep      = "VoidRep"
  showPrec d LiftedRep    = "LiftedRep"
  showPrec d UnliftedRep  = "UnliftedRep"
  showPrec d Int8Rep      = "Int8Rep"
  showPrec d Int16Rep     = "Int16Rep"
  showPrec d Int32Rep     = "Int32Rep"
  showPrec d Int64Rep     = "Int64Rep"
  showPrec d IntRep       = "IntRep"
  showPrec d Word8Rep     = "Word8Rep"
  showPrec d Word16Rep    = "Word16Rep"
  showPrec d Word32Rep    = "Word32Rep"
  showPrec d Word64Rep    = "Word64Rep"
  showPrec d WordRep      = "WordRep"
  showPrec d AddrRep      = "AddrRep"
  showPrec d FloatRep     = "FloatRep"
  showPrec d DoubleRep    = "DoubleRep"
  showPrec d (VecRep n p) = showCon d "VecRep" $ showArg n ++ " " ++ showArg p

||| SingeValue LiftedRep = Simple Algebraic value
public export
data RepType
  = SingleValue    PrimRep
  | UnboxedTuple   (List PrimRep)
  | PolymorphicRep

export
Show RepType where
  showPrec d (SingleValue p)   = showCon d "SingleValue" $ showArg p
  showPrec d (UnboxedTuple ps) = showCon d "UnboxedTuple" $ showArg ps
  showPrec d PolymorphicRep    = "PolymorphicRep"

export
SemiDecEq RepType where
  semiDecEq (SingleValue p1) (SingleValue p2) = do
    Refl <- semiDecEq p1 p2
    Just Refl
  semiDecEq (UnboxedTuple p1) (UnboxedTuple p2) = do
    Refl <- semiDecEq p1 p2
    Just Refl
  semiDecEq PolymorphicRep PolymorphicRep = Just Refl
  semiDecEq _ _ = Nothing

