module Idris.Runtime.Cast.Double where

import Idris.Runtime.PrimType
import Idris.Runtime.String as Str
import Idris.Runtime.Integer as BI

string :: Double -> IO Str
string = fromString . show
