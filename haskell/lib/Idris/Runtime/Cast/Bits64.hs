module Idris.Runtime.Cast.Bits64 where

import Idris.Runtime.PrimType
import Idris.Runtime.String as Str
import Idris.Runtime.Integer as BI

string :: Bits64 -> IO Str
string = fromString . show
