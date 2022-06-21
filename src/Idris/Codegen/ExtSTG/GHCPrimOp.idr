module Idris.Codegen.ExtSTG.GHCPrimOp

import Idris.Codegen.ExtSTG.Prelude
import Idris.Codegen.ExtSTG.Representation


public export
data PrimOp : (name: String) -> (args : List PrimRep) -> (ret : PrimRep) -> Type where
  PlusInt      : PrimOp "+#"            [IntRep   , IntRep   ] IntRep
  PlusInt8     : PrimOp "plusInt8#"     [Int8Rep  , Int8Rep  ] Int8Rep
  PlusInt16    : PrimOp "plusInt16#"    [Int16Rep , Int16Rep ] Int16Rep
  PlusWord8    : PrimOp "plusWord8#"    [Word8Rep , Word8Rep ] Word8Rep
  PlusWord16   : PrimOp "plusWord16#"   [Word16Rep, Word16Rep] Word16Rep
  PlusWord     : PrimOp "plusWord#"     [WordRep  , WordRep  ] WordRep
  PlusDouble   : PrimOp "+##"           [DoubleRep, DoubleRep] DoubleRep
  SubInt       : PrimOp "-#"            [IntRep   , IntRep   ] IntRep
  SubInt8      : PrimOp "subInt8#"      [Int8Rep  , Int8Rep  ] Int8Rep
  SubInt16     : PrimOp "subInt16#"     [Int16Rep , Int16Rep ] Int16Rep
  SubWord8     : PrimOp "subWord8#"     [Word8Rep , Word8Rep ] Word8Rep
  SubWord16    : PrimOp "subWord16#"    [Word16Rep, Word16Rep] Word16Rep
  SubWord      : PrimOp "subWord#"      [WordRep  , WordRep  ] WordRep
  SubDouble    : PrimOp "-##"           [DoubleRep, DoubleRep] DoubleRep
  TimesInt     : PrimOp "*#"            [IntRep   , IntRep   ] IntRep
  TimesInt8    : PrimOp "timesInt8#"    [Int8Rep  , Int8Rep  ] Int8Rep
  TimesInt16   : PrimOp "timesInt16#"   [Int16Rep , Int16Rep ] Int16Rep
  TimesWord8   : PrimOp "timesWord8#"   [Word8Rep , Word8Rep ] Word8Rep
  TimesWord16  : PrimOp "timesWord16#"  [Word16Rep, Word16Rep] Word16Rep
  TimesWord    : PrimOp "timesWord#"    [WordRep  , WordRep  ] WordRep
  TimesDouble  : PrimOp "timesDouble#"  [DoubleRep, DoubleRep] DoubleRep
  QuotInt      : PrimOp "quotInt#"      [IntRep   , IntRep   ] IntRep
  QuotInt8     : PrimOp "quotInt8#"     [Int8Rep  , Int8Rep  ] Int8Rep
  QuotInt16    : PrimOp "quotInt16#"    [Int16Rep , Int16Rep ] Int16Rep
  QuotWord8    : PrimOp "quotWord8#"    [Word8Rep , Word8Rep ] Word8Rep
  QuotWord16   : PrimOp "quotWord16#"   [Word16Rep, Word16Rep] Word16Rep
  QuotWord     : PrimOp "quotWord#"     [WordRep  , WordRep  ] WordRep
  QuotDouble   : PrimOp "/##"           [DoubleRep, DoubleRep] DoubleRep
  RemInt       : PrimOp "remInt#"       [IntRep   , IntRep   ] IntRep
  RemInt8      : PrimOp "remInt8#"      [Int8Rep  , Int8Rep  ] Int8Rep
  RemInt16     : PrimOp "remInt16#"     [Int16Rep , Int16Rep ] Int16Rep
  RemWord8     : PrimOp "remWord8#"     [Word8Rep , Word8Rep ] Word8Rep
  RemWord16    : PrimOp "remWord16#"    [Word16Rep, Word16Rep] Word16Rep
  RemWord      : PrimOp "remWord#"      [WordRep  , WordRep  ] WordRep
  AndInt       : PrimOp "andI#"         [IntRep   , IntRep   ] IntRep
  AndWord      : PrimOp "and#"          [WordRep  , WordRep  ] WordRep
  OrInt        : PrimOp "orI#"          [IntRep   , IntRep   ] IntRep
  OrWord       : PrimOp "or#"           [WordRep  , WordRep  ] WordRep
  XOrInt       : PrimOp "xorI#"         [IntRep   , IntRep   ] IntRep
  XOrWord      : PrimOp "xor#"          [WordRep  , WordRep  ] WordRep
  LTInt        : PrimOp "<#"            [IntRep   , IntRep   ] IntRep
  LTInt8       : PrimOp "ltInt8#"       [Int8Rep  , Int8Rep  ] IntRep
  LTInt16      : PrimOp "ltInt16#"      [Int16Rep , Int16Rep ] IntRep
  LTWord8      : PrimOp "ltWord8#"      [Word8Rep , Word8Rep ] IntRep
  LTWord16     : PrimOp "ltWord16#"     [Word16Rep, Word16Rep] IntRep
  LTWord       : PrimOp "ltWord#"       [WordRep  , WordRep  ] IntRep
  LTDouble     : PrimOp "<##"           [DoubleRep, DoubleRep] IntRep
  LTEInt       : PrimOp "<=#"           [IntRep   , IntRep   ] IntRep
  LTEInt8      : PrimOp "leInt8#"       [Int8Rep  , Int8Rep  ] IntRep
  LTEInt16     : PrimOp "leInt16#"      [Int16Rep , Int16Rep ] IntRep
  LTEWord8     : PrimOp "leWord8#"      [Word8Rep , Word8Rep ] IntRep
  LTEWord16    : PrimOp "leWord16#"     [Word16Rep, Word16Rep] IntRep
  LTEWord      : PrimOp "leWord#"       [WordRep  , WordRep  ] IntRep
  LTEDouble    : PrimOp "<=##"          [DoubleRep, DoubleRep] IntRep
  EQInt        : PrimOp "==#"           [IntRep   , IntRep   ] IntRep
  EQInt8       : PrimOp "eqInt8#"       [Int8Rep  , Int8Rep  ] IntRep
  EQInt16      : PrimOp "eqInt16#"      [Int16Rep , Int16Rep ] IntRep
  EQWord8      : PrimOp "eqWord8#"      [Word8Rep , Word8Rep ] IntRep
  EQWord16     : PrimOp "eqWord16#"     [Word16Rep, Word16Rep] IntRep
  EQWord       : PrimOp "eqWord"        [WordRep  , WordRep  ] IntRep
  EQDouble     : PrimOp "==##"          [DoubleRep, DoubleRep] IntRep
  GTEInt       : PrimOp ">=#"           [IntRep   , IntRep   ] IntRep
  GTEInt8      : PrimOp "geInt8#"       [Int8Rep  , Int8Rep  ] IntRep
  GTEInt16     : PrimOp "geInt16#"      [Int16Rep , Int16Rep ] IntRep
  GTEWord8     : PrimOp "geWord8#"      [Word8Rep , Word8Rep ] IntRep
  GTEWord16    : PrimOp "geWord16#"     [Word16Rep, Word16Rep] IntRep
  GTEWord      : PrimOp "geWord"        [WordRep  , WordRep  ] IntRep
  GTEDouble    : PrimOp ">=##"          [DoubleRep, DoubleRep] IntRep
  GTInt        : PrimOp ">#"            [IntRep   , IntRep   ] IntRep
  GTInt8       : PrimOp "gtInt8#"       [Int8Rep  , Int8Rep  ] IntRep
  GTInt16      : PrimOp "gtInt16#"      [Int16Rep , Int16Rep ] IntRep
  GTWord8      : PrimOp "gtWord8#"      [Word8Rep , Word8Rep ] IntRep
  GTWord16     : PrimOp "gtWord16#"     [Word16Rep, Word16Rep] IntRep
  GTWord       : PrimOp "gtWord"        [WordRep  , WordRep  ] IntRep
  GTDouble     : PrimOp ">##"           [DoubleRep, DoubleRep] IntRep
  GTChar       : PrimOp "gtChar#"       [CharRep  , CharRep  ] IntRep
  GEChar       : PrimOp "geChar#"       [CharRep  , CharRep  ] IntRep
  EQChar       : PrimOp "eqChar#"       [CharRep  , CharRep  ] IntRep
  NEChar       : PrimOp "neChar#"       [CharRep  , CharRep  ] IntRep
  LTChar       : PrimOp "ltChar#"       [CharRep  , CharRep  ] IntRep
  LEChar       : PrimOp "leChar#"       [CharRep  , CharRep  ] IntRep
  NegateInt    : PrimOp "negateInt#"    [IntRep   ] IntRep
  NegateInt8   : PrimOp "negateInt8#"   [Int8Rep  ] Int8Rep
  NegateInt16  : PrimOp "negateInt16#"  [Int16Rep ] Int16Rep
  NegateDouble : PrimOp "negateDouble#" [DoubleRep] DoubleRep
  ExpDouble    : PrimOp "expDouble"     [DoubleRep] DoubleRep
  LogDouble    : PrimOp "logDouble"     [DoubleRep] DoubleRep
  SinDouble    : PrimOp "sinDouble"     [DoubleRep] DoubleRep
  CosDouble    : PrimOp "cosDouble"     [DoubleRep] DoubleRep
  TanDouble    : PrimOp "tanDouble"     [DoubleRep] DoubleRep
  ASinDouble   : PrimOp "asinDouble"    [DoubleRep] DoubleRep
  ACosDouble   : PrimOp "acosDouble"    [DoubleRep] DoubleRep
  ATanDouble   : PrimOp "atanDouble"    [DoubleRep] DoubleRep
  SqrtDouble   : PrimOp "sqrtDouble"    [DoubleRep] DoubleRep
  IShiftLInt   : PrimOp "uncheckedIShiftL#"  [IntRep, IntRep] IntRep
  IShiftLRInt  : PrimOp "uncheckedIShiftRL#" [IntRep, IntRep] IntRep

  IndexWord8OffAddr : PrimOp "indexWord8OffAddr#" [AddrRep     , IntRep]  Word8Rep
  IndexWord8Array   : PrimOp "indexWord8Array#"   [ByteArrayRep, IntRep]  Word8Rep
  IndexCharOffAddr  : PrimOp "indexCharOffAddr#"  [AddrRep     , IntRep]  CharRep
  IndexCharArray    : PrimOp "indexCharArray#"    [ByteArrayRep, IntRep]  CharRep
  SizeOfByteArray   : PrimOp "sizeofByteArray#"   [ByteArrayRep]          IntRep
  ByteArrayContents : PrimOp "byteArrayContents#" [ByteArrayRep]          AddrRep

  PlusAddr : PrimOp "plusAddr#" [AddrRep, IntRep] AddrRep

  NarrowWord8  : PrimOp "narrowWord8#"  [WordRep  ] Word8Rep
  ExtendWord8  : PrimOp "extendWord8#"  [Word8Rep ] WordRep
  NarrowWord16 : PrimOp "narrowWord16#" [WordRep  ] Word16Rep
  ExtendWord16 : PrimOp "extendWord16#" [Word16Rep] WordRep
  NarrowWord32 : PrimOp "narrow32Word#" [WordRep  ] Word32Rep
  ExtendInt8   : PrimOp "extendInt8#"   [Int8Rep  ] IntRep
  NarrowInt8   : PrimOp "narrowInt8#"   [IntRep   ] Int8Rep
  ExtendInt16  : PrimOp "extendInt16#"  [Int16Rep ] IntRep
  NarrowInt16  : PrimOp "narrowInt16#"  [IntRep   ] Int16Rep
  NarrowInt32  : PrimOp "narrow32Int#"  [IntRep   ] Int32Rep

  -- State ignoring PrimOps:
  -- Int# -> State# s -> (# State# s, MutableByteArray# s #)
  NewByteArray : PrimOp "newByteArray#" [IntRep, VoidRep] MutableByteArrayRep
  -- Addr# -> MutableByteArray# s -> Int# -> Int# -> State# s -> State# s
  CopyAddrToByteArray : PrimOp "copyAddrToByteArray#" [AddrRep, MutableByteArrayRep, IntRep, IntRep, VoidRep] VoidRep
  -- ByteArray# -> Int# -> MutableByteArray# s -> Int# -> Int# -> State# s -> State# s
  CopyByteArray : PrimOp "copyByteArray#" [ByteArrayRep, IntRep, MutableByteArrayRep, IntRep, IntRep, VoidRep] VoidRep
  -- MutableByteArray# s -> State# s -> (# State# s, ByteArray# #)
  UnsafeFreezeByteArray : PrimOp "unsafeFreezeByteArray#" [MutableByteArrayRep,VoidRep] ByteArrayRep
  -- MutableByteArray# s -> Int# -> Word# -> State# s -> State# s
  WriteWord8Array : PrimOp "writeWord8Array#" [MutableByteArrayRep,IntRep,Word8Rep,VoidRep] VoidRep
  -- MutableByteArray# s -> Int# -> Char# -> State# s -> State# s
  WriteCharArray : PrimOp "writeCharArray#" [MutableByteArrayRep,IntRep,CharRep,VoidRep] VoidRep
  -- MutableByteArray# s -> Int# -> State# s -> (# State# s, Word# #)
  ReadWord8Array : PrimOp "readWord8Array#" [MutableByteArrayRep,IntRep,VoidRep] Word8Rep


namespace PrimOp

  export
  name : {n : String} -> PrimOp n args r -> String
  name {n} _ = n

  export
  args : {as : List PrimRep} -> PrimOp n as r -> List PrimRep
  args {as} _ = as
