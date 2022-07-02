module Idris.Runtime.Cast.Int32 where

import Idris.Runtime.PrimType
import Idris.Runtime.String as Str
import Idris.Runtime.Integer as BI

string :: Int32 -> IO Str
string = fromString . show
