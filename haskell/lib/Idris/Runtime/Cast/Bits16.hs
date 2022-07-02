module Idris.Runtime.Cast.Bits16 where

import Idris.Runtime.PrimType
import Idris.Runtime.String as Str
import Idris.Runtime.Integer as BI

string :: Bits16 -> IO Str
string = fromString . show
