module Main

import Data.List

main : IO ()
main = putStr $ concat $ mapMaybe Just ["Hello", " STG!\n"]

