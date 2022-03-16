module Main

import Data.String

isEmpty : String -> IO ()
isEmpty str = case strM str of
  StrNil => putStrLn "String is empty."
  StrCons h t => putStrLn "String is not empty."

main : IO ()
main = do
  let str1 = ""
  let str2 = "non-empty"
  isEmpty str1
  isEmpty str2
