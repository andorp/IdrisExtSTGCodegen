module Main

%foreign "stg:main_Idris.Test.FFITypes.cfListIntNil"
cfListIntNil : List Int

%foreign "stg:main_Idris.Test.FFITypes.cfListIntCons1"
cfListIntCons1 : List Int

%foreign "stg:main_Idris.Test.FFITypes.cfListIntCons2"
cfListIntCons2 : List Int


main : IO ()
main = do
  putStrLn $ show cfListIntNil
  putStrLn $ show cfListIntCons1
  putStrLn $ show cfListIntCons2
