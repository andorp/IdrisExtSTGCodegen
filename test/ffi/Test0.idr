module Main

-- %World is represented as VoidRep, for that reason the PrimIO is working. Check this and fix, separate if needed.

%foreign "stg:main_Idris.Test.FFITypes.cfInt"
cfInt42 : Int

%foreign "stg:main_Idris.Test.FFITypes.cfIntThunk"
cfIntThunk : Int

%foreign "stg:main_Idris.Test.FFITypes.cfIOInt"
cfIOInt42 : PrimIO Int

%foreign "stg:main_Idris.Test.FFITypes.cfIOIntThunk"
cfIOIntThunk : PrimIO Int

%foreign "stg:main_Idris.Test.FFITypes.printAndSucc"
printAndSucc : Int -> PrimIO Int

main : IO ()
main = do
  putStrLn $ show cfInt42
  putStrLn $ show cfIntThunk
  x <- primIO cfIOInt42
  putStrLn $ show x
  x <- primIO cfIOIntThunk
  putStrLn $ show x
  x <- primIO $ printAndSucc 42
  putStrLn $ show x
