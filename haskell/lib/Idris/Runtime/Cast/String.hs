module Idris.Runtime.Cast.String where

import Idris.Runtime.PrimType
import Idris.Runtime.String as Str
import Idris.Runtime.Integer as BI

integer :: Str -> IO BI
integer = pure . fromStr

int :: Str -> IO Int
int = pure . read . Str.toString

double :: Str -> IO Double
double = pure . read . Str.toString
