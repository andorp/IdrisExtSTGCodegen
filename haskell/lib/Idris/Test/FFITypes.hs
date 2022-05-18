{-# OPTIONS_GHC -O0 #-}
module Idris.Test.FFITypes where

{-
Test cases for primitive types support for STG.

[ ] CFUnit : CFType
[x] CFInt : CFType
[ ] CFInteger : CFType
[ ] CFInt8 : CFType
[ ] CFInt16 : CFType
[ ] CFInt32 : CFType
[ ] CFInt64 : CFType
[ ] CFUnsigned8 : CFType
[ ] CFUnsigned16 : CFType
[ ] CFUnsigned32 : CFType
[ ] CFUnsigned64 : CFType
[ ] CFString : CFType
[ ] CFDouble : CFType
[ ] CFChar : CFType
[ ] CFPtr : CFType
[ ] CFGCPtr : CFType
[ ] CFBuffer : CFType
[ ] CFForeignObj : CFType
[ ] CFWorld : CFType
[ ] CFFun : CFType -> CFType -> CFType
[ ] CFIORes : CFType -> CFType
[ ] CFStruct : String -> List (String, CFType) -> CFType
[ ] CFUser : Name -> List CFType -> CFType
-}

cfInt :: Int
cfInt = 42

cfIntThunk :: Int
cfIntThunk = sum $ replicate 42 1

cfIOInt :: IO Int
cfIOInt = pure 42

cfIOIntThunk :: IO Int
cfIOIntThunk = pure $ sum $ replicate 42 1

printAndSucc :: Int -> IO Int
printAndSucc x = do
  print x
  pure $ succ x
