module Idris.Runtime.PrimType
  ( module Idris.Runtime.PrimType
  , module GHC.Int
  , module GHC.Word
  ) where

import GHC.Int (Int8, Int16, Int32, Int64)
import GHC.Word (Word8, Word16, Word32, Word64)

type Bits8  = Word8
type Bits16 = Word16
type Bits32 = Word32
type Bits64 = Word64
