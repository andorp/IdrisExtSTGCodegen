module Idris.Runtime.Cast.Char where

import Data.Char (ord)
import Idris.Runtime.PrimType
import Idris.Runtime.String as Str
import Idris.Runtime.Integer as BI


int :: Char -> Int
int = ord

integer :: Char -> BI
integer c = BI (toInteger (ord c))

string :: Char -> IO Str
string = fromString . show
