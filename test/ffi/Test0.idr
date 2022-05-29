module Main

-- Int

%foreign "stg:main_Idris.Test.FFITypes.cfInt"
cfInt : Int

%foreign "stg:main_Idris.Test.FFITypes.cfIntThunk"
cfIntThunk : Int

%foreign "stg:main_Idris.Test.FFITypes.cfIOInt"
cfIOInt : PrimIO Int

%foreign "stg:main_Idris.Test.FFITypes.cfIOIntThunk"
cfIOIntThunk : PrimIO Int

%foreign "stg:main_Idris.Test.FFITypes.cfIntInt"
cfIntInt : Int -> Int

%foreign "stg:main_Idris.Test.FFITypes.cfIntIOInt"
cfIntIOInt : Int -> PrimIO Int

-- Integer

%foreign "stg:main_Idris.Test.FFITypes.cfInteger"
cfInteger : Integer

%foreign "stg:main_Idris.Test.FFITypes.cfIntegerThunk"
cfIntegerThunk : Integer

%foreign "stg:main_Idris.Test.FFITypes.cfIOInteger"
cfIOInteger : PrimIO Integer

%foreign "stg:main_Idris.Test.FFITypes.cfIOIntegerThunk"
cfIOIntegerThunk : PrimIO Integer

%foreign "stg:main_Idris.Test.FFITypes.cfIntegerInteger"
cfIntegerInteger : Integer -> Integer

%foreign "stg:main_Idris.Test.FFITypes.cfIntegerIOInteger"
cfIntegerIOInteger : Integer -> PrimIO Integer

-- Int8

%foreign "stg:main_Idris.Test.FFITypes.cfInt8"
cfInt8 : Int8

%foreign "stg:main_Idris.Test.FFITypes.cfInt8Thunk"
cfInt8Thunk : Int8

%foreign "stg:main_Idris.Test.FFITypes.cfIOInt8"
cfIOInt8 : PrimIO Int8

%foreign "stg:main_Idris.Test.FFITypes.cfIOInt8Thunk"
cfIOInt8Thunk : PrimIO Int8

%foreign "stg:main_Idris.Test.FFITypes.cfInt8Int8"
cfInt8Int8 : Int8 -> Int8

%foreign "stg:main_Idris.Test.FFITypes.cfInt8IOInt8"
cfInt8IOInt8 : Int8 -> PrimIO Int8

-- Int16

%foreign "stg:main_Idris.Test.FFITypes.cfInt16"
cfInt16 : Int16

%foreign "stg:main_Idris.Test.FFITypes.cfInt16Thunk"
cfInt16Thunk : Int16

%foreign "stg:main_Idris.Test.FFITypes.cfIOInt16"
cfIOInt16 : PrimIO Int16

%foreign "stg:main_Idris.Test.FFITypes.cfIOInt16Thunk"
cfIOInt16Thunk : PrimIO Int16

%foreign "stg:main_Idris.Test.FFITypes.cfInt16Int16"
cfInt16Int16 : Int16 -> Int16

%foreign "stg:main_Idris.Test.FFITypes.cfInt16IOInt16"
cfInt16IOInt16 : Int16 -> PrimIO Int16

-- Int32

%foreign "stg:main_Idris.Test.FFITypes.cfInt32"
cfInt32 : Int32

%foreign "stg:main_Idris.Test.FFITypes.cfInt32Thunk"
cfInt32Thunk : Int32

%foreign "stg:main_Idris.Test.FFITypes.cfIOInt32"
cfIOInt32 : PrimIO Int32

%foreign "stg:main_Idris.Test.FFITypes.cfIOInt32Thunk"
cfIOInt32Thunk : PrimIO Int32

%foreign "stg:main_Idris.Test.FFITypes.cfInt32Int32"
cfInt32Int32 : Int32 -> Int32

%foreign "stg:main_Idris.Test.FFITypes.cfInt32IOInt32"
cfInt32IOInt32 : Int32 -> PrimIO Int32

-- Int64

%foreign "stg:main_Idris.Test.FFITypes.cfInt64"
cfInt64 : Int64

%foreign "stg:main_Idris.Test.FFITypes.cfInt64Thunk"
cfInt64Thunk : Int64

%foreign "stg:main_Idris.Test.FFITypes.cfIOInt64"
cfIOInt64 : PrimIO Int64

%foreign "stg:main_Idris.Test.FFITypes.cfIOInt64Thunk"
cfIOInt64Thunk : PrimIO Int64

%foreign "stg:main_Idris.Test.FFITypes.cfInt64Int64"
cfInt64Int64 : Int64 -> Int64

%foreign "stg:main_Idris.Test.FFITypes.cfInt64IOInt64"
cfInt64IOInt64 : Int64 -> PrimIO Int64

-- Bits8

%foreign "stg:main_Idris.Test.FFITypes.cfBits8"
cfBits8 : Bits8

%foreign "stg:main_Idris.Test.FFITypes.cfBits8Thunk"
cfBits8Thunk : Bits8

%foreign "stg:main_Idris.Test.FFITypes.cfIOBits8"
cfIOBits8 : PrimIO Bits8

%foreign "stg:main_Idris.Test.FFITypes.cfIOBits8Thunk"
cfIOBits8Thunk : PrimIO Bits8

%foreign "stg:main_Idris.Test.FFITypes.cfBits8Bits8"
cfBits8Bits8 : Bits8 -> Bits8

%foreign "stg:main_Idris.Test.FFITypes.cfBits8IOBits8"
cfBits8IOBits8 : Bits8 -> PrimIO Bits8

-- Bits16

%foreign "stg:main_Idris.Test.FFITypes.cfBits16"
cfBits16 : Bits16

%foreign "stg:main_Idris.Test.FFITypes.cfBits16Thunk"
cfBits16Thunk : Bits16

%foreign "stg:main_Idris.Test.FFITypes.cfIOBits16"
cfIOBits16 : PrimIO Bits16

%foreign "stg:main_Idris.Test.FFITypes.cfIOBits16Thunk"
cfIOBits16Thunk : PrimIO Bits16

%foreign "stg:main_Idris.Test.FFITypes.cfBits16Bits16"
cfBits16Bits16 : Bits16 -> Bits16

%foreign "stg:main_Idris.Test.FFITypes.cfBits16IOBits16"
cfBits16IOBits16 : Bits16 -> PrimIO Bits16

-- Bits32

%foreign "stg:main_Idris.Test.FFITypes.cfBits32"
cfBits32 : Bits32

%foreign "stg:main_Idris.Test.FFITypes.cfBits32Thunk"
cfBits32Thunk : Bits32

%foreign "stg:main_Idris.Test.FFITypes.cfIOBits32"
cfIOBits32 : PrimIO Bits32

%foreign "stg:main_Idris.Test.FFITypes.cfIOBits32Thunk"
cfIOBits32Thunk : PrimIO Bits32

%foreign "stg:main_Idris.Test.FFITypes.cfBits32Bits32"
cfBits32Bits32 : Bits32 -> Bits32

%foreign "stg:main_Idris.Test.FFITypes.cfBits32IOBits32"
cfBits32IOBits32 : Bits32 -> PrimIO Bits32

-- Bits64

%foreign "stg:main_Idris.Test.FFITypes.cfBits64"
cfBits64 : Bits64

%foreign "stg:main_Idris.Test.FFITypes.cfBits64Thunk"
cfBits64Thunk : Bits64

%foreign "stg:main_Idris.Test.FFITypes.cfIOBits64"
cfIOBits64 : PrimIO Bits64

%foreign "stg:main_Idris.Test.FFITypes.cfIOBits64Thunk"
cfIOBits64Thunk : PrimIO Bits64

%foreign "stg:main_Idris.Test.FFITypes.cfBits64Bits64"
cfBits64Bits64 : Bits64 -> Bits64

%foreign "stg:main_Idris.Test.FFITypes.cfBits64IOBits64"
cfBits64IOBits64 : Bits64 -> PrimIO Bits64


-- Char

%foreign "stg:main_Idris.Test.FFITypes.cfChar"
cfChar : Char

%foreign "stg:main_Idris.Test.FFITypes.cfCharThunk"
cfCharThunk : Char

%foreign "stg:main_Idris.Test.FFITypes.cfIOChar"
cfIOChar : PrimIO Char

%foreign "stg:main_Idris.Test.FFITypes.cfIOCharThunk"
cfIOCharThunk : PrimIO Char

%foreign "stg:main_Idris.Test.FFITypes.cfCharChar"
cfCharChar : Char -> Char

%foreign "stg:main_Idris.Test.FFITypes.cfCharIOChar"
cfCharIOChar : Char -> PrimIO Char

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

-- Double

%foreign "stg:main_Idris.Test.FFITypes.cfDouble"
cfDouble : Double

%foreign "stg:main_Idris.Test.FFITypes.cfDoubleThunk"
cfDoubleThunk : Double

%foreign "stg:main_Idris.Test.FFITypes.cfIODouble"
cfIODouble : PrimIO Double

%foreign "stg:main_Idris.Test.FFITypes.cfIODoubleThunk"
cfIODoubleThunk : PrimIO Double

%foreign "stg:main_Idris.Test.FFITypes.cfDoubleDouble"
cfDoubleDouble : Double -> Double

%foreign "stg:main_Idris.Test.FFITypes.cfDoubleIODouble"
cfDoubleIODouble : Double -> PrimIO Double

main : IO ()
main = do
  putStrLn "Int"
  putStrLn $ show cfInt
  putStrLn $ show cfIntThunk
  x <- primIO cfIOInt
  putStrLn $ show x
  x <- primIO cfIOIntThunk
  putStrLn $ show x
  let x = cfIntInt 41
  putStrLn $ show x
  x <- primIO $ cfIntIOInt 41
  putStrLn $ show x

  putStrLn "Int8"
  putStrLn $ show cfInt8
  putStrLn $ show cfInt8Thunk
  x <- primIO cfIOInt8
  putStrLn $ show x
  x <- primIO cfIOInt8Thunk
  putStrLn $ show x
  let x = cfInt8Int8 41
  putStrLn $ show x
  x <- primIO $ cfInt8IOInt8 41
  putStrLn $ show x

  putStrLn "Int16"
  putStrLn $ show cfInt16
  putStrLn $ show cfInt16Thunk
  x <- primIO cfIOInt16
  putStrLn $ show x
  x <- primIO cfIOInt16Thunk
  putStrLn $ show x
  let x = cfInt16Int16 41
  putStrLn $ show x
  x <- primIO $ cfInt16IOInt16 41
  putStrLn $ show x

  putStrLn "Int32"
  putStrLn $ show cfInt32
  putStrLn $ show cfInt32Thunk
  x <- primIO cfIOInt32
  putStrLn $ show x
  x <- primIO cfIOInt32Thunk
  putStrLn $ show x
  let x = cfInt32Int32 41
  putStrLn $ show x
  x <- primIO $ cfInt32IOInt32 41
  putStrLn $ show x

  putStrLn "Int64"
  putStrLn $ show cfInt64
  putStrLn $ show cfInt64Thunk
  x <- primIO cfIOInt64
  putStrLn $ show x
  x <- primIO cfIOInt64Thunk
  putStrLn $ show x
  let x = cfInt64Int64 41
  putStrLn $ show x
  x <- primIO $ cfInt64IOInt64 41
  putStrLn $ show x

  putStrLn "Bits8"
  putStrLn $ show cfBits8
  putStrLn $ show cfBits8Thunk
  x <- primIO cfIOBits8
  putStrLn $ show x
  x <- primIO cfIOBits8Thunk
  putStrLn $ show x
  let x = cfBits8Bits8 41
  putStrLn $ show x
  x <- primIO $ cfBits8IOBits8 41
  putStrLn $ show x

  putStrLn "Bits16"
  putStrLn $ show cfBits16
  putStrLn $ show cfBits16Thunk
  x <- primIO cfIOBits16
  putStrLn $ show x
  x <- primIO cfIOBits16Thunk
  putStrLn $ show x
  let x = cfBits16Bits16 41
  putStrLn $ show x
  x <- primIO $ cfBits16IOBits16 41
  putStrLn $ show x

  putStrLn "Bits32"
  putStrLn $ show cfBits32
  putStrLn $ show cfBits32Thunk
  x <- primIO cfIOBits32
  putStrLn $ show x
  x <- primIO cfIOBits32Thunk
  putStrLn $ show x
  let x = cfBits32Bits32 41
  putStrLn $ show x
  x <- primIO $ cfBits32IOBits32 41
  putStrLn $ show x

  putStrLn "Bits64"
  putStrLn $ show cfBits64
  putStrLn $ show cfBits64Thunk
  x <- primIO cfIOBits64
  putStrLn $ show x
  x <- primIO cfIOBits64Thunk
  putStrLn $ show x
  let x = cfBits64Bits64 41
  putStrLn $ show x
  x <- primIO $ cfBits64IOBits64 41
  putStrLn $ show x

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

  putStrLn "Char"
  putStrLn $ show cfChar
  putStrLn $ show cfCharThunk
  x <- primIO cfIOChar
  putStrLn $ show x
  x <- primIO cfIOCharThunk
  putStrLn $ show x
  let x = cfCharChar 'A'
  putStrLn $ show x
  x <- primIO $ cfCharIOChar 'A'
  putStrLn $ show x

  putStrLn "Double"
  putStrLn $ show cfDouble
  putStrLn $ show cfDoubleThunk
  x <- primIO cfIODouble
  putStrLn $ show x
  x <- primIO cfIODoubleThunk
  putStrLn $ show x
  let x = cfDoubleDouble 41
  putStrLn $ show x
  x <- primIO $ cfDoubleIODouble 41
  putStrLn $ show x
