module Main

import Data.Strings

partial
main : IO ()
main = do
  name <- getLine
  putStrLn $ strTail $ "!Hello " ++ name
