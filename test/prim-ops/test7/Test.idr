
main : IO ()
main = do
  x <- getLine
  putStrLn $ reverse x
