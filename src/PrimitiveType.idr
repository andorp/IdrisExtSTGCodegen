module Main -- where

main : IO ()
main = do
  let x = 69.0
  case x of
    69.0 => print $ 2.0
    _    => print $ 4.0
