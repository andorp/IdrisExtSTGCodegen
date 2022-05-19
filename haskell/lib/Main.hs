module Main where

-- We need this just to import the module, which is needed by the GHC-WPC compiler for the time being.
import Idris.Runtime.String
import Idris.Runtime.World
import Idris.FFI.System.File.Process
import Idris.FFI.System.File.ReadWrite
import Idris.FFI.System.File.Virtual

main :: IO ()
main = putStrLn "Idris.Runtime"
