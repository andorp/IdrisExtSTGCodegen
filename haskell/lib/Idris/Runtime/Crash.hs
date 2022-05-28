module Idris.Runtime.Crash where

missingDefault :: a
missingDefault = error "Missing default case for non-matched alternative."

