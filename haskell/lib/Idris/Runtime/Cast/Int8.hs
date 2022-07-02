module Idris.Runtime.Cast.Int8 where

import Idris.Runtime.PrimType
import Idris.Runtime.String as Str
import Idris.Runtime.Integer as BI

string :: Int8 -> IO Str
string = fromString . show
