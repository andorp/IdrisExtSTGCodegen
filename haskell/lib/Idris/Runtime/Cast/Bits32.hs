module Idris.Runtime.Cast.Bits32 where

import Idris.Runtime.PrimType
import Idris.Runtime.String as Str
import Idris.Runtime.Integer as BI

string :: Bits32 -> IO Str
string = fromString . show
