
data Test : Type where
  MkTest : Test

export
tpat1 : Type -> Int
tpat1 Int = 0
tpat1 String = 1
tpat1 (Maybe Test) = 2
tpat1 (Maybe String) = 3
tpat1 other = 4

export
tpat2 : (Type -> Type) -> Int
tpat2 _ = 1

main : IO ()
main = do
  printLn $ tpat1 (Maybe Test)
