module Idris.FFI.System where

import Control.Concurrent
import Data.Time.Clock.POSIX

usleep :: Int -> IO ()
usleep = threadDelay

time :: IO Int
time = fmap round getPOSIXTime
