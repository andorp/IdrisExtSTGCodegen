module Idris.Codegen.ExtSTG.GHCPrimOp

import Idris.Codegen.ExtSTG.Prelude
import Idris.Codegen.ExtSTG.Representation

%default total

public export
data PrimOp : (name: String) -> (args : List PrimRep) -> (ret : PrimRep) -> Type where
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

namespace PrimOp

  export
  name : {n : String} -> PrimOp n args r -> String
  name {n} _ = n
