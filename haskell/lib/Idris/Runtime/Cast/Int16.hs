module Idris.Runtime.Cast.Int16 where

import Idris.Runtime.PrimType
import Idris.Runtime.String as Str
import Idris.Runtime.Integer as BI

string :: Int16 -> IO Str
string = fromString . show
