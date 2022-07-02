module Idris.Runtime.Cast.Integer where

import Idris.Runtime.PrimType
import Idris.Runtime.String as Str
import Idris.Runtime.Integer as BI

int :: BI -> Int
int (BI a) = fromInteger a

bits8 :: BI -> Bits8
bits8 (BI x) = fromInteger x

bits16 :: BI -> Bits16
bits16 (BI x) = fromInteger x

bits32 :: BI -> Bits32
bits32 (BI x) = fromInteger x

bits64 :: BI -> Bits64
bits64 (BI x) = fromInteger x

string :: BI -> IO Str
string = BI.toStr

double :: BI -> Double
double (BI x) = fromInteger x
