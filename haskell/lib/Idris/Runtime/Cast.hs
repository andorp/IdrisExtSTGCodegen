module Idris.Runtime.Cast where

import Data.Char (ord)
import Idris.Runtime.PrimType
import Idris.Runtime.String (Str, fromString)
import Idris.Runtime.Integer (BI(..), fromStr)


intString :: Int -> IO Str
intString = fromString . show

int8String :: Int8 -> IO Str
int8String = fromString . show

int16String :: Int16 -> IO Str
int16String = fromString . show

int32String :: Int32 -> IO Str
int32String = fromString . show

int64String :: Int64 -> IO Str
int64String = fromString . show

bits8String :: Bits8 -> IO Str
bits8String = fromString . show

bits16String :: Bits16 -> IO Str
bits16String = fromString . show

bits32String :: Bits32 -> IO Str
bits32String = fromString . show

bits64String :: Bits64 -> IO Str
bits64String = fromString . show

charString :: Char -> IO Str
charString = fromString . show

doubleString :: Double -> IO Str
doubleString = fromString . show

charInt :: Char -> Int
charInt = ord

stringInteger :: Str -> BI
stringInteger = fromStr

integerBits8 :: BI -> Bits8
integerBits8 (BI x) = fromInteger x

integerBits64 :: BI -> Bits64
integerBits64 (BI x) = fromInteger x
