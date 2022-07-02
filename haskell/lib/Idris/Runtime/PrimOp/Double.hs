module Idris.Runtime.PrimOp.Double where

exp :: Double -> Double
exp = Prelude.exp

log :: Double -> Double
log = Prelude.log

pow :: Double -> Double -> Double
pow = (**)

sin :: Double -> Double
sin = Prelude.sin

cos :: Double -> Double
cos = Prelude.cos

tan :: Double -> Double
tan = Prelude.tan

asin :: Double -> Double
asin = Prelude.asin

acos :: Double -> Double
acos = Prelude.acos

atan :: Double -> Double
atan = Prelude.atan

sqrt :: Double -> Double
sqrt = Prelude.sqrt

floor :: Double -> Double
floor = fromInteger . Prelude.floor

ceiling :: Double -> Double
ceiling = fromInteger . Prelude.ceiling
