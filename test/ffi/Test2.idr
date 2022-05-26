module Main

theList : List Int
theList = [1,2]

main : IO ()
main = do
  printLn (2 < 3)
  printLn theList
