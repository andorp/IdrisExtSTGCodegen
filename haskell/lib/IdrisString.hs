{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import GHC.Ptr
import GHC.Prim
import Foreign.Storable
import Data.Primitive.Types
import Data.Primitive.ByteArray
import Control.Monad.Primitive

test :: IO ()
test = do
  let s = "Hello World!"
  print $ length s
  str1' <- IdrisString.fromString s
  let str1 = strToLit str1'

  strLength str1 >>= print
  strHead str1 >>= print
  strTail str1 >>= toString >>= print
  strIndex str1 6 >>= print
  strCons '!' str1 >>= toString >>= print
  strSubstr str1 2 7 >>= toString >>= print
  strSubstr str1' 2 7 >>= toString >>= print
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

addrStrToString :: Addr# -> Int -> IO String
addrStrToString a n = do
  c :: Char <- peekElemOff (Ptr a) n
  if c == '\x0000'
      then pure []
      else fmap (c:) $ addrStrToString a (n + 1)

toString :: Str -> IO String
toString (Lit addr) = addrStrToString addr 0
toString (Val arr) = do
  s <- getSizeofMutableByteArray arr
  arr' <- freezeByteArray arr 0 (s - 4)
  pure $ foldrByteArray (:) [] arr'

strToLit :: Str -> Str
strToLit (Lit a)   = Lit a
strToLit (Val arr) = case mutableByteArrayContents arr of
  (Ptr addr) -> Lit addr

addrStrLength :: Addr# -> Int -> IO Int
addrStrLength a n = do
  c :: Char <- peekElemOff (Ptr a) n
  if c == '\x0000'
      then pure n
      else addrStrLength a (n + 1)

strLength :: Str -> IO Int
strLength (Lit addr) = addrStrLength addr 0
strLength (Val arr) = do
  s <- getSizeofMutableByteArray arr
  pure ((s `div` 4) - 1)

strHead :: Str -> IO Char
strHead (Lit addr) = peekElemOff (Ptr addr) 0
strHead (Val arr) = readByteArray arr 0

strTail :: Str -> IO Str
strTail (Lit addr) = pure $ (\(Ptr addr') -> Lit addr') $ plusPtr (Ptr addr) 4
strTail (Val arr) = do
  s <- getSizeofMutableByteArray arr
  arr' <- newByteArray (s - 4) -- Char stores Word# which is 4 bytes
  copyMutableByteArray arr' 0 arr 4 (s - 4)
  pure (Val arr')

strIndex :: Str -> Int -> IO Char
strIndex (Lit addr) i = peekElemOff (Ptr addr) i
strIndex (Val arr) i = readByteArray arr i

strCons :: Char -> Str -> IO Str
strCons c (Lit addr) = do
  n <- addrStrLength addr 0
  arr' <- newByteArray (4 * (n + 1))
  case (arr', 4, (4 * n)) of
    (MutableByteArray a, I# offset, I# lngth)
      -> primitive_ (copyAddrToByteArray# addr a offset lngth)
  writeByteArray arr' 0 c
  pure (Val arr')
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
strSubstr (Lit addr) i j = do
  let ns = ((j - i + 1) * 4)
  arr' <- newByteArray ns
  case (arr', plusPtr (Ptr addr) (i * 4), 0, ((j - i) * 4)) of
    (MutableByteArray a, Ptr addr', I# offset, I# lngth)
      -> primitive_ (copyAddrToByteArray# addr' a offset lngth)
  writeByteArray arr' (ns `div` 4) ((toEnum 0) :: Char)
  pure (Val arr')
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
