module Idris.FFI.System.File.ReadWrite
  ( prim__eof
  ) where

import GHC.IO.Handle

prim__eof :: Handle -> IO Int
prim__eof h = do
  b <- hIsEOF h
  pure $ case b of
    True  -> 1
    False -> 0
