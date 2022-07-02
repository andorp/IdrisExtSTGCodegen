module Idris.Runtime.Cast.Bits8 where

import Idris.Runtime.PrimType
import Idris.Runtime.String as Str
import Idris.Runtime.Integer as BI

string :: Bits8 -> IO Str
string = fromString . show
