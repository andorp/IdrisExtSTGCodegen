module Main

-- Haskell String

%foreign "stg:main_Idris.Test.FFITypes.cfString"
cfHsString : List Char

%foreign "stg:main_Idris.Test.FFITypes.cfStringThunk"
cfHsStringThunk : List Char

%foreign "stg:main_Idris.Test.FFITypes.cfIOString"
cfIOHsString : PrimIO (List Char)

%foreign "stg:main_Idris.Test.FFITypes.cfIOStringThunk"
cfIOHsStringThunk : PrimIO (List Char)

%foreign "stg:main_Idris.Test.FFITypes.cfStringString"
cfHsStringHsString : (List Char) -> (List Char)

%foreign "stg:main_Idris.Test.FFITypes.cfStringIOString"
cfHsStringIOHsString : (List Char) -> PrimIO (List Char)

-- Idris String

%foreign "stg:main_Idris.Test.FFITypes.cfStr"
cfString : String

%foreign "stg:main_Idris.Test.FFITypes.cfStrThunk"
cfStringThunk : String

%foreign "stg:main_Idris.Test.FFITypes.cfIOStr"
cfIOString : PrimIO String

%foreign "stg:main_Idris.Test.FFITypes.cfIOStrThunk"
cfIOStringThunk : PrimIO String

%foreign "stg:main_Idris.Test.FFITypes.cfStrStr"
cfStringString : String -> String

%foreign "stg:main_Idris.Test.FFITypes.cfStrIOStr"
cfStringIOString : String -> PrimIO String


main : IO ()
main = do
  putStrLn "Haskell String"
  putStrLn $ show cfHsString
  putStrLn $ pack cfHsString
  putStrLn $ pack cfHsStringThunk
  x <- primIO cfIOHsString
  putStrLn $ pack x
  x <- primIO cfIOHsStringThunk
  putStrLn $ pack x
  let x = cfHsStringHsString $ unpack "41"
  putStrLn $ pack x
  x <- primIO $ cfHsStringIOHsString $ unpack "41"
  putStrLn $ pack x

  putStrLn "Idris String"
  putStrLn cfString
  putStrLn cfStringThunk
  x <- primIO cfIOString
  putStrLn x
  x <- primIO cfIOStringThunk
  putStrLn x
  let x = cfStringString "41"
  putStrLn x
  x <- primIO $ cfStringIOString "41"
  putStrLn x
