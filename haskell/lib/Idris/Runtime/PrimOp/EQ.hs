module Idris.Runtime.PrimOp.EQ where

import Idris.Runtime.PrimType
import Idris.Runtime.Integer as BI
import Idris.Runtime.String as Str

int :: Int -> Int -> Int
int x y = boolean (x == y)

int8 :: Int8 -> Int8 -> Int
int8 x y = boolean (x == y)

int16 :: Int16 -> Int16 -> Int
int16 x y = boolean (x == y)

int32 :: Int32 -> Int32 -> Int
int32 x y = boolean (x == y)

int64 :: Int64 -> Int64 -> Int
int64 x y = boolean (x == y)

integer :: BI -> BI -> Int
integer = BI.eq

bits8 :: Bits8 -> Bits8 -> Int
bits8 x y = boolean (x == y)

bits16 :: Bits16 -> Bits16 -> Int
bits16 x y = boolean (x == y)

bits32 :: Bits32 -> Bits32 -> Int
bits32 x y = boolean (x == y)

bits64 :: Bits64 -> Bits64 -> Int
bits64 x y = boolean (x == y)

string :: Str -> Str -> IO Int
string x y = do
  r <- Str.strCompare x y
  pure $ boolean (r == 0)

char :: Char -> Char -> Int
char x y = boolean (x == y)

double :: Double -> Double -> Int
double x y = boolean (x == y)
