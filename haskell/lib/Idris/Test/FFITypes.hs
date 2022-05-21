{-# OPTIONS_GHC -O0 #-}
module Idris.Test.FFITypes where

import Idris.Runtime.PrimType


{-
Test cases for primitive types support for STG.

[?] CFUnit : CFType
[x] CFInt : CFType
[ ] CFInteger : CFType
[ ] CFInt8 : CFType
[ ] CFInt16 : CFType
[ ] CFInt32 : CFType
[ ] CFInt64 : CFType
[x] CFUnsigned8 : CFType
[x] CFUnsigned16 : CFType
[x] CFUnsigned32 : CFType
[x] CFUnsigned64 : CFType
[ ] CFString : CFType
[x] CFDouble : CFType
[ ] CFChar : CFType
[ ] CFPtr : CFType -- Haskell type variable ?
[ ] CFGCPtr : CFType -- Haskell type variable ?
[ ] CFBuffer : CFType
[ ] CFForeignObj : CFType
[-] CFWorld : CFType
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

{- Error: INTERNAL ERROR: Unsupported type: [] (CFUser Prelude.Basics.List TODO:xs)
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
-}

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
