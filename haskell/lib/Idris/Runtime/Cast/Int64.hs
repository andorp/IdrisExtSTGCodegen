module Idris.Runtime.Cast.Int64 where

import Idris.Runtime.PrimType
import Idris.Runtime.String as Str
import Idris.Runtime.Integer as BI

string :: Int64 -> IO Str
string = fromString . show
