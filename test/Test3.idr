module Main

-- StrCons and StrIndex tests

import Data.String

partial
main : IO ()
main = putStrLn $ strCons (strIndex "Hello World!" 1) ""
