{-# LANGUAGE MagicHash #-}
module IdrisString where
  --( Str(..)
  ----, strLength
  ----, strHead
  ----, strTail
  ----, strIndex
  ----, strCons
  ----, strAppend
  ----, strReverse
  ----, strSubstr
  --) where

import GHC.Exts
import Data.Primitive.ByteArray

test :: IO ()
test = do
  let s = "Hello World!"
  print $ length s
  str1 <- IdrisString.fromString s
  strLength str1 >>= print
  strHead str1 >>= print
  strTail str1 >>= toString >>= print
  strIndex str1 6 >>= print
  strCons '!' str1 >>= toString >>= print
  strSubstr str1 2 7 >>= toString >>= print
  str2 <- toString str1
  print $ length str2

  str3 <- IdrisString.fromString "Hello "
  str4 <- IdrisString.fromString "World!"
  strAppend str3 str4 >>= toString >>= print
  putStrLn str2

data Str
  = Lit Addr#
  | Val (MutableByteArray RealWorld)

fromString :: String -> IO Str
fromString str = do
  let arr = byteArrayFromList (str ++ [toEnum 0])
  let s1 = sizeofByteArray arr
  arr' <- newByteArray s1
  copyByteArray arr' 0 arr 0 s1
  pure (Val arr')

toString :: Str -> IO String
toString (Lit _addr) = undefined
toString (Val arr) = do
  s <- getSizeofMutableByteArray arr
  arr' <- freezeByteArray arr 0 (s - 4)
  pure $ foldrByteArray (:) [] arr'

strLength :: Str -> IO Int
strLength (Lit _addr) = undefined
strLength (Val arr) = do
  s <- getSizeofMutableByteArray arr
  pure ((s `div` 4) - 1)

strHead :: Str -> IO Char
strHead (Lit _addr) = undefined
strHead (Val arr) = readByteArray arr 0

strTail :: Str -> IO Str
strTail (Lit _addr) = undefined
strTail (Val arr) = do
  s <- getSizeofMutableByteArray arr
  arr' <- newByteArray (s - 4) -- Char stores Word# which is 4 bytes
  copyMutableByteArray arr' 0 arr 4 (s - 4)
  pure (Val arr')

strIndex :: Str -> Int -> IO Char
strIndex (Lit _addr) _i = undefined
strIndex (Val arr) i = readByteArray arr i

strCons :: Char -> Str -> IO Str
strCons _c (Lit _addr) = undefined
strCons c (Val arr) = do
  s <- getSizeofMutableByteArray arr
  arr' <- newByteArray (s + 4)
  writeByteArray arr' 0 c
  copyMutableByteArray arr' 4 arr 0 s
  pure (Val arr')

strAppend :: Str -> Str -> IO Str
strAppend (Lit _addr1) (Lit _addr2) = undefined
strAppend (Lit _addr1) (Val _arr2)  = undefined
strAppend (Val _arr1)  (Lit _addr2) = undefined
strAppend (Val arr1)   (Val arr2) = do
  s1 <- getSizeofMutableByteArray arr1
  s2 <- getSizeofMutableByteArray arr2
  arr' <- newByteArray (s1 + s2 - 4)
  copyMutableByteArray arr' 0 arr1 0 (s1 - 4)
  copyMutableByteArray arr' (s1 - 4) arr2 0 s2
  writeByteArray arr' ((s1 + s2) `div` 4) ((toEnum 0) :: Char)
  pure (Val arr')

-- TODO: Check if indexing is right
strSubstr :: Str -> Int -> Int -> IO Str
strSubstr (Lit _addr) _i _j = undefined
strSubstr (Val arr) i j = do
  let ns = ((j - i + 1) * 4)
  arr' <- newByteArray ns
  copyMutableByteArray arr' 0 arr (i * 4) ((j - i) * 4)
  writeByteArray arr' (ns `div` 4) ((toEnum 0) :: Char)
  pure (Val arr')

--strReverse :: Str -> IO Str
--strReverse = undefined

--strCompare :: Str -> Str -> IO Int
--strCompare = undefined
