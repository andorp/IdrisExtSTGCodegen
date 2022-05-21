module Idris.Runtime.Cast where

import Idris.Runtime.PrimType
import Idris.Runtime.String (Str, fromString)


intString :: Int -> IO Str
intString = fromString . show

bits8String :: Bits8 -> IO Str
bits8String = fromString . show

bits16String :: Bits16 -> IO Str
bits16String = fromString . show

bits32String :: Bits32 -> IO Str
bits32String = fromString . show

bits64String :: Bits64 -> IO Str
bits64String = fromString . show

doubleString :: Double -> IO Str
doubleString = fromString . show
