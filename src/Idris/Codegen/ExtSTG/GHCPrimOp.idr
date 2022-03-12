module Idris.Codegen.ExtSTG.GHCPrimOp

import Idris.Codegen.ExtSTG.Prelude
import Idris.Codegen.ExtSTG.Representation


public export
data PrimOp : (name: String) -> (args : List PrimRep) -> (ret : PrimRep) -> Type where
  PlusInt      : PrimOp "+#"            [IntRep   , IntRep   ] IntRep
  PlusWord8    : PrimOp "plusWord8#"    [Word8Rep , Word8Rep ] Word8Rep
  PlusWord16   : PrimOp "plusWord16#"   [Word16Rep, Word16Rep] Word16Rep
  PlusWord     : PrimOp "plusWord3"     [WordRep  , WordRep  ] WordRep
  PlusDouble   : PrimOp "+##"           [DoubleRep, DoubleRep] DoubleRep
  SubInt       : PrimOp "-#"            [IntRep   , IntRep   ] IntRep
  SubWord8     : PrimOp "subWord8#"     [Word8Rep , Word8Rep ] Word8Rep
  SubWord16    : PrimOp "subWord16#"    [Word16Rep, Word16Rep] Word16Rep
  SubWord      : PrimOp "subWord#"      [WordRep  , WordRep  ] WordRep
  SubDouble    : PrimOp "-##"           [DoubleRep, DoubleRep] DoubleRep
  TimesInt     : PrimOp "*#"            [IntRep   , IntRep   ] IntRep
  TimesWord8   : PrimOp "timesWord8#"   [Word8Rep , Word8Rep ] Word8Rep
  TimesWord16  : PrimOp "timesWord16#"  [Word16Rep, Word16Rep] Word16Rep
  TimesWord    : PrimOp "timesWord#"    [WordRep  , WordRep  ] WordRep
  TimesDouble  : PrimOp "timesDouble#"  [DoubleRep, DoubleRep] DoubleRep
  QuotInt      : PrimOp "qoutInt#"      [IntRep   , IntRep   ] IntRep
  QuotWord8    : PrimOp "quotWord8#"    [Word8Rep , Word8Rep ] Word8Rep
  QuotWord16   : PrimOp "quotWord16#"   [Word16Rep, Word16Rep] Word16Rep
  QuotWord     : PrimOp "quotWord#"     [WordRep  , WordRep  ] WordRep
  QuotDouble   : PrimOp "/##"           [DoubleRep, DoubleRep] DoubleRep
  RemInt       : PrimOp "remInt#"       [IntRep   , IntRep   ] IntRep
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
  LTWord8      : PrimOp "ltWord8#"      [Word8Rep , Word8Rep ] IntRep
  LTWord16     : PrimOp "ltWord16#"     [Word16Rep, Word16Rep] IntRep
  LTWord       : PrimOp "ltWord#"       [WordRep  , WordRep  ] IntRep
  LTDouble     : PrimOp "<##"           [DoubleRep, DoubleRep] IntRep
  LTEInt       : PrimOp "<=#"           [IntRep   , IntRep   ] IntRep
  LTEWord8     : PrimOp "leWord8#"      [Word8Rep , Word8Rep ] IntRep
  LTEWord16    : PrimOp "leWord16#"     [Word16Rep, Word16Rep] IntRep
  LTEWord      : PrimOp "leWord#"       [WordRep  , WordRep  ] IntRep
  LTEDouble    : PrimOp "<=##"          [DoubleRep, DoubleRep] IntRep
  EQInt        : PrimOp "==#"           [IntRep   , IntRep   ] IntRep
  EQWord8      : PrimOp "eqWord8#"      [Word8Rep , Word8Rep ] IntRep
  EQWord16     : PrimOp "eqWord16#"     [Word16Rep, Word16Rep] IntRep
  EQWord       : PrimOp "eqWord"        [WordRep  , WordRep  ] IntRep
  EQDouble     : PrimOp "==##"          [DoubleRep, DoubleRep] IntRep
  GTEInt       : PrimOp ">=#"           [IntRep   , IntRep   ] IntRep
  GTEWord8     : PrimOp "geWord8#"      [Word8Rep , Word8Rep ] IntRep
  GTEWord16    : PrimOp "geWord16#"     [Word16Rep, Word16Rep] IntRep
  GTEWord      : PrimOp "geWord"        [WordRep  , WordRep  ] IntRep
  GTEDouble    : PrimOp ">=##"          [DoubleRep, DoubleRep] IntRep
  GTInt        : PrimOp ">#"            [IntRep   , IntRep   ] IntRep
  GTWord8      : PrimOp "gtWord8#"      [Word8Rep , Word8Rep ] IntRep
  GTWord16     : PrimOp "gtWord16#"     [Word16Rep, Word16Rep] IntRep
  GTWord       : PrimOp "gtWord"        [WordRep  , WordRep  ] IntRep
  GTDouble     : PrimOp ">##"           [DoubleRep, DoubleRep] IntRep
  NegateInt    : PrimOp "negateInt#"    [IntRep   ] IntRep
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

  IndexWord8OffAddr : PrimOp "indexWord8OffAddr#" [AddrRep, IntRep] Word8Rep
  IndexWord8Array   : PrimOp "indexWord8Array#" [ByteArrayRep, IntRep] Word8Rep
  IndexCharOffAddr  : PrimOp "indexCharOffAddr#" [AddrRep, IntRep] CharRep
  IndexCharArray    : PrimOp "indexCharArray#" [ByteArrayRep, IntRep] CharRep
  SizeOfByteArray   : PrimOp "sizeofByteArray#" [ByteArrayRep] IntRep
  ByteArrayContents : PrimOp "byteArrayContents#" [ByteArrayRep] AddrRep

  PlusAddr : PrimOp "plusAddr#" [AddrRep, IntRep] AddrRep

  NarrowWord8 : PrimOp "narrowWord8#" [WordRep] Word8Rep
  ExtendWord8 : PrimOp "extendWord8#" [Word8Rep] WordRep

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

namespace PrimOp

  export
  name : {n : String} -> PrimOp n args r -> String
  name {n} _ = n

  export
  args : {as : List PrimRep} -> PrimOp n as r -> List PrimRep
  args {as} _ = as
