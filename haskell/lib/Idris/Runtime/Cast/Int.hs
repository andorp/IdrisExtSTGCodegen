module Idris.Runtime.Cast.Int where

import Data.Char (chr)
import Idris.Runtime.PrimType
import Idris.Runtime.String as Str
import Idris.Runtime.Integer as BI

string :: Int -> IO Str
string = fromString . show

integer :: Int -> BI
integer x = BI (fromIntegral x)

char :: Int -> Char
char = chr
