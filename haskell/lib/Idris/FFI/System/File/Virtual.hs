module Idris.FFI.System.File.Virtual
  ( prim__stdin
  , prim__stdout
  ) where

import System.IO

prim__stdin :: Handle
prim__stdin = stdin

prim__stdout :: Handle
prim__stdout = stdout

