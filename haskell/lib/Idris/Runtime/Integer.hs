module Idris.Runtime.Integer where

import Data.Char (ord)
import Idris.Runtime.PrimType (boolean)
import Idris.Runtime.String as Str

data BI = BI Integer

fromStr :: Str -> BI
fromStr = BI . read . Str.toString

toStr :: BI -> IO Str
toStr (BI i) = Str.fromString (show i)

-- Arithmetic

add :: BI -> BI -> BI
add (BI a) (BI b) = BI (a + b)

sub :: BI -> BI -> BI
sub (BI a) (BI b) = BI (a - b)

mul :: BI -> BI -> BI
mul (BI a) (BI b) = BI (a * b)

div :: BI -> BI -> BI
div (BI a) (BI b) = BI (Prelude.div a b)

mod :: BI -> BI -> BI
mod (BI a) (BI b) = BI (Prelude.mod a b)

neg :: BI -> BI
neg (BI a) = BI (negate a)

-- Compare

lt :: BI -> BI -> Int
lt (BI a) (BI b) = boolean (a < b)

lte :: BI -> BI -> Int
lte (BI a) (BI b) = boolean (a <= b)

eq :: BI -> BI -> Int
eq (BI a) (BI b) = boolean (a == b)

gte :: BI -> BI -> Int
gte (BI a) (BI b) = boolean (a >= b)

gt :: BI -> BI -> Int
gt (BI a) (BI b) = boolean (a > b)

-- Casts

castInt :: BI -> Int
castInt (BI a) = fromInteger a

fromChar :: Char -> BI
fromChar c = BI (toInteger (ord c))
