module Idris.FFI.System.File.Process
  ( prim__flush
  ) where

import GHC.IO.Handle
import System.IO.Error

-- Result int parameter will be ignored.
-- TODO: Investigate if this behaviour is ok, in the error case
prim__flush :: Handle -> IO Int
prim__flush h = catchIOError (hFlush h >> pure 0) (\_ -> pure 1)

