module Idris.FFI.Prelude.IO where

import Idris.Runtime.String as Str

putStr :: Str -> IO ()
putStr s = Prelude.putStr $ Str.toString s

getStr :: IO Str
getStr = Prelude.getLine >>= Str.fromString
