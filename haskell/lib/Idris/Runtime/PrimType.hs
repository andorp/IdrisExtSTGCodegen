module Idris.Runtime.PrimType
  ( module Idris.Runtime.PrimType
  , module GHC.Word
  ) where

import GHC.Word (Word8, Word16, Word32, Word64)

type Bits8  = Word8
type Bits16 = Word16
type Bits32 = Word32
type Bits64 = Word64
