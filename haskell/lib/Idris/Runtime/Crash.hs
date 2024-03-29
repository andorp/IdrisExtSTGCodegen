module Idris.Runtime.Crash where

import Control.Exception
import Idris.Runtime.String (Str, toString)
import Idris.Runtime.Erased

missingDefault :: a
missingDefault = error "Missing default case for non-matched alternative."

crash :: Erased -> Str -> IO ()
crash _ = throwIO . AssertionFailed . toString

void :: Erased -> b -> a
void _ _ = error "Executed 'void'"
