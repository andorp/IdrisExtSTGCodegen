module Idris.Runtime.Cast where

import Data.Char (chr, ord)
import Idris.Runtime.PrimType
import Idris.Runtime.String as Str
import Idris.Runtime.Integer as BI (BI(..), fromStr, toStr)


intString :: Int -> IO Str
intString = fromString . show

intInteger :: Int -> BI
intInteger x = BI (fromIntegral x)

intChar :: Int -> Char
intChar = chr

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

charInteger :: Char -> BI
charInteger c = BI (toInteger (ord c))

stringInteger :: Str -> BI
stringInteger = fromStr -- TODO: Check if conversion is ok

stringInt :: Str -> Int
stringInt = read . Str.toString

integerInt :: BI -> Int
integerInt (BI a) = fromInteger a

integerBits8 :: BI -> Bits8
integerBits8 (BI x) = fromInteger x

integerBits16 :: BI -> Bits16
integerBits16 (BI x) = fromInteger x

integerBits32 :: BI -> Bits32
integerBits32 (BI x) = fromInteger x

integerBits64 :: BI -> Bits64
integerBits64 (BI x) = fromInteger x

integerString :: BI -> IO Str
integerString = BI.toStr

integerDouble :: BI -> Double
integerDouble (BI x) = fromInteger x
