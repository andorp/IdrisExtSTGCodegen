module Main -- where

main : IO ()
main = do
  let f (x :: xs) = ((x + 1) :: f xs)
  printLn $ f [1,2,3,4,5]

