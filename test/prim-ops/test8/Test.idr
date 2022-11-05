module Main

data MyData = MkMyData

data MyStr = MkMyStr

matchOnType : (t : Type) -> t -> String
matchOnType MyData _ = "MyData"
matchOnType Nat        x  = show (x + 2)
matchOnType Double     x  = show (x + 1.1)
matchOnType (List Int) xs = show xs
matchOnType (List t)   _  = "List t"
matchOnType _          _  = "Not MyData"

main : IO ()
main = do
  putStrLn $ matchOnType MyStr  MkMyStr
  putStrLn $ matchOnType MyData MkMyData
  putStrLn $ matchOnType Nat    1
  putStrLn $ matchOnType Double 5.0
  putStrLn $ matchOnType (List Int) [1,2,3]
  putStrLn $ matchOnType (List Nat) []
  putStrLn $ matchOnType (Maybe ()) Nothing

