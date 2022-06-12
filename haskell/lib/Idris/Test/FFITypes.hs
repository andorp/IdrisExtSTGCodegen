{-# OPTIONS_GHC -O0 #-}
module Idris.Test.FFITypes where

import Data.Char
import System.IO.Unsafe (unsafePerformIO)

import Idris.Runtime.PrimType
import Idris.Runtime.String as Str

{-
Test cases for primitive types support for STG.

[ ] CFUnit : CFType
[x] CFInt : CFType
[x] CFInteger : CFType
[x] CFInt8 : CFType
[x] CFInt16 : CFType
[x] CFInt32 : CFType
[x] CFInt64 : CFType
[x] CFUnsigned8 : CFType
[x] CFUnsigned16 : CFType
[x] CFUnsigned32 : CFType
[x] CFUnsigned64 : CFType
[x] CFString : CFType -- support of fastPack and Idris List representation on Haskell side
[x] CFDouble : CFType
[x] CFChar : CFType -- Needs Integer cast
[ ] CFPtr : CFType -- Haskell type variable ?
[ ] CFGCPtr : CFType -- Haskell type variable ?
[ ] CFBuffer : CFType
[-] CFForeignObj : CFType -- Ability to manipulate scheme objects
[+] CFWorld : CFType
[ ] CFFun : CFType -> CFType -> CFType
[+] CFIORes : CFType -> CFType
[ ] CFStruct : String -> List (String, CFType) -> CFType
[ ] CFUser : Name -> List CFType -> CFType
-}



-- Int

cfInt :: Int
cfInt = 42

cfIntThunk :: Int
cfIntThunk = sum $ replicate 42 1

cfIOInt :: IO Int
cfIOInt = pure 42

cfIOIntThunk :: IO Int
cfIOIntThunk = pure $ sum $ replicate 42 1

cfIntInt :: Int -> Int
cfIntInt = succ

cfIntIOInt :: Int -> IO Int
cfIntIOInt x = do
  print x
  pure $ succ x

-- Bits8

cfBits8 :: Bits8
cfBits8 = 42

cfBits8Thunk :: Bits8
cfBits8Thunk = sum $ replicate 42 1

cfIOBits8 :: IO Bits8
cfIOBits8 = pure 42

cfIOBits8Thunk :: IO Bits8
cfIOBits8Thunk = pure $ sum $ replicate 42 1

cfBits8Bits8 :: Bits8 -> Bits8
cfBits8Bits8 = succ

cfBits8IOBits8 :: Bits8 -> IO Bits8
cfBits8IOBits8 x = do
  print x
  pure $ succ x

-- Bits16

cfBits16 :: Bits16
cfBits16 = 42

cfBits16Thunk :: Bits16
cfBits16Thunk = sum $ replicate 42 1

cfIOBits16 :: IO Bits16
cfIOBits16 = pure 42

cfIOBits16Thunk :: IO Bits16
cfIOBits16Thunk = pure $ sum $ replicate 42 1

cfBits16Bits16 :: Bits16 -> Bits16
cfBits16Bits16 = succ

cfBits16IOBits16 :: Bits16 -> IO Bits16
cfBits16IOBits16 x = do
  print x
  pure $ succ x

-- Bits32

cfBits32 :: Bits32
cfBits32 = 42

cfBits32Thunk :: Bits32
cfBits32Thunk = sum $ replicate 42 1

cfIOBits32 :: IO Bits32
cfIOBits32 = pure 42

cfIOBits32Thunk :: IO Bits32
cfIOBits32Thunk = pure $ sum $ replicate 42 1

cfBits32Bits32 :: Bits32 -> Bits32
cfBits32Bits32 = succ

cfBits32IOBits32 :: Bits32 -> IO Bits32
cfBits32IOBits32 x = do
  print x
  pure $ succ x

-- Bits64

cfBits64 :: Bits64
cfBits64 = 42

cfBits64Thunk :: Bits64
cfBits64Thunk = sum $ replicate 42 1

cfIOBits64 :: IO Bits64
cfIOBits64 = pure 42

cfIOBits64Thunk :: IO Bits64
cfIOBits64Thunk = pure $ sum $ replicate 42 1

cfBits64Bits64 :: Bits64 -> Bits64
cfBits64Bits64 = succ

cfBits64IOBits64 :: Bits64 -> IO Bits64
cfBits64IOBits64 x = do
  print x
  pure $ succ x

-- String

cfString :: String
cfString = "42"

cfStringThunk :: String
cfStringThunk = replicate 42 '1'

cfIOString :: IO String
cfIOString = pure "42"

cfIOStringThunk :: IO String
cfIOStringThunk = pure $ replicate 42 '1'

cfStringString :: String -> String
cfStringString x = x ++ ['1']

cfStringIOString :: String -> IO String
cfStringIOString x = do
  print x
  pure $ x ++ ['1']

-- IdrisString

cfStr :: Str
cfStr = Str.fromStringUnsafe "42"

cfStrThunk :: Str
cfStrThunk = Str.fromStringUnsafe $ replicate 42 '1'

cfIOStr :: IO Str
cfIOStr = Str.fromString "42"

cfIOStrThunk :: IO Str
cfIOStrThunk = Str.fromString $ replicate 42 '1'

cfStrStr :: Str -> Str
cfStrStr x = unsafePerformIO $ do
  y <- Str.fromString "1"
  Str.strAppend x y

cfStrIOStr :: Str -> IO Str
cfStrIOStr x = do
  y <- Str.fromString "1"
  Str.strAppend x y

-- Double

cfDouble :: Double
cfDouble = 42

cfDoubleThunk :: Double
cfDoubleThunk = sum $ replicate 42 1

cfIODouble :: IO Double
cfIODouble = pure 42

cfIODoubleThunk :: IO Double
cfIODoubleThunk = pure $ sum $ replicate 42 1

cfDoubleDouble :: Double -> Double
cfDoubleDouble x = x + 1

cfDoubleIODouble :: Double -> IO Double
cfDoubleIODouble x = do
  print x
  pure $ x + 1

-- Int8

cfInt8 :: Int8
cfInt8 = 42

cfInt8Thunk :: Int8
cfInt8Thunk = sum $ replicate 42 1

cfIOInt8 :: IO Int8
cfIOInt8 = pure 42

cfIOInt8Thunk :: IO Int8
cfIOInt8Thunk = pure $ sum $ replicate 42 1

cfInt8Int8 :: Int8 -> Int8
cfInt8Int8 = succ

cfInt8IOInt8 :: Int8 -> IO Int8
cfInt8IOInt8 x = do
  print x
  pure $ succ x

-- Int16

cfInt16 :: Int16
cfInt16 = 42

cfInt16Thunk :: Int16
cfInt16Thunk = sum $ replicate 42 1

cfIOInt16 :: IO Int16
cfIOInt16 = pure 42

cfIOInt16Thunk :: IO Int16
cfIOInt16Thunk = pure $ sum $ replicate 42 1

cfInt16Int16 :: Int16 -> Int16
cfInt16Int16 = succ

cfInt16IOInt16 :: Int16 -> IO Int16
cfInt16IOInt16 x = do
  print x
  pure $ succ x

-- Int32

cfInt32 :: Int32
cfInt32 = 42

cfInt32Thunk :: Int32
cfInt32Thunk = sum $ replicate 42 1

cfIOInt32 :: IO Int32
cfIOInt32 = pure 42

cfIOInt32Thunk :: IO Int32
cfIOInt32Thunk = pure $ sum $ replicate 42 1

cfInt32Int32 :: Int32 -> Int32
cfInt32Int32 = succ

cfInt32IOInt32 :: Int32 -> IO Int32
cfInt32IOInt32 x = do
  print x
  pure $ succ x

-- Int64

cfInt64 :: Int64
cfInt64 = 42

cfInt64Thunk :: Int64
cfInt64Thunk = sum $ replicate 42 1

cfIOInt64 :: IO Int64
cfIOInt64 = pure 42

cfIOInt64Thunk :: IO Int64
cfIOInt64Thunk = pure $ sum $ replicate 42 1

cfInt64Int64 :: Int64 -> Int64
cfInt64Int64 = succ

cfInt64IOInt64 :: Int64 -> IO Int64
cfInt64IOInt64 x = do
  print x
  pure $ succ x

-- Char

cfChar :: Char
cfChar = 'A'

cfCharThunk :: Char
cfCharThunk = chr $ sum $ replicate 65 1

cfIOChar :: IO Char
cfIOChar = pure 'A'

cfIOCharThunk :: IO Char
cfIOCharThunk = pure $ chr $ sum $ replicate 65 1

cfCharChar :: Char -> Char
cfCharChar = succ

cfCharIOChar :: Char -> IO Char
cfCharIOChar x = do
  print x
  pure $ succ x

-- List

cfListIntNil :: [Int]
cfListIntNil = []

cfListIntCons1 :: [Int]
cfListIntCons1 = [1]

cfListIntCons2 :: [Int]
cfListIntCons2 = [1,2]

cfListCons :: [Int] -> [Int]
cfListCons xs = 0 : xs
