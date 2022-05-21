{-# LANGUAGE MagicHash, UnboxedTuples, ScopedTypeVariables #-}
module Idris.Runtime.String
  ( module Idris.Runtime.String
  ) where

import Idris.Runtime.PrimType

import Data.Char (chr, ord)
import Control.Monad.Primitive (primitive_)
import GHC.Exts
  ( RealWorld
  , Int(I#)
  , Char(C#)
  , Word(W#)
  , Int#
  , Addr#
  , ByteArray#
  , copyAddrToByteArray#
  , byteArrayContents#
  , sizeofByteArray#
  , indexCharArray#
  , copyByteArray#
  , plusAddr#
  , unsafeCoerce#
  , indexWord8OffAddr#
  , indexCharOffAddr#
  , writeCharArray#
  )
import Data.Primitive.ByteArray
  ( MutableByteArray(..)
  , ByteArray(..)
  , MutableByteArray#
  , unsafeFreezeByteArray
  , readByteArray
  , newByteArray
  , writeByteArray
  , foldrByteArray
  , copyByteArray
  , sizeofByteArray
  , byteArrayFromList
  , newPinnedByteArray
  )

unI :: Int -> Int#
unI (I# i) = i

unByteArray :: ByteArray -> ByteArray#
unByteArray (ByteArray arr) = arr

unMutableByteArray :: MutableByteArray RealWorld -> MutableByteArray# RealWorld
unMutableByteArray (MutableByteArray arr) = arr

data Str
  = Lit Addr#
  | Val ByteArray#

-- This is a limitation, which will be removed. TODO: Datacon registration that should come from FFI files.
mkStrFromAddr :: Addr# -> Str
mkStrFromAddr = Lit

mkChar :: Char -> Word8
mkChar = fromIntegral . ord

hsChar :: Word8 -> Char
hsChar = chr . fromIntegral

fromString :: String -> IO Str
fromString str = do
  let arr = byteArrayFromList $ map mkChar $ (str ++ [toEnum 0])
  let s1 = sizeofByteArray arr
  arr' <- newByteArray s1
  copyByteArray arr' 0 arr 0 s1
  arr2 <- unsafeFreezeByteArray arr'
  pure (Val (unByteArray arr2))

addrStrToString :: Addr# -> Int -> String
addrStrToString a (I# n) = case (W# (indexWord8OffAddr# a n)) of
  0 -> []
  _ -> (C# (indexCharOffAddr# a n)) : addrStrToString a (I# n + 1)


toString :: Str -> String
toString (Lit addr) = addrStrToString addr 0
toString (Val arr) = reverse $ drop 1 $ reverse $ map hsChar $ foldrByteArray (:) [] (ByteArray arr)

-- Helper function to simulate String literals in GHC
strToLit :: Str -> IO Str
strToLit (Lit a)   = pure $ Lit a
strToLit (Val arr) = do
  let s = sizeofByteArray# arr
  parr <- newPinnedByteArray (I# s)
  copyByteArray parr 0 (ByteArray arr) 0 (I# s)
  pure $ Lit (byteArrayContents# (unsafeCoerce# (unMutableByteArray parr)))

addrStrLength :: Addr# -> Int -> Int
addrStrLength a (I# n) = case (W# (indexWord8OffAddr# a n)) of
  0 -> (I# n)
  _ -> addrStrLength a (I# n + 1)

strLength :: Str -> Int
strLength (Lit addr) = addrStrLength addr 0
strLength (Val arr) = (I# (sizeofByteArray# arr)) - 1

strHead :: Str -> Char
strHead (Lit addr) = C# (indexCharOffAddr# addr (unI 0))
strHead (Val arr)  = C# (indexCharArray# arr (unI 0))

newString :: Int -> (MutableByteArray# RealWorld -> IO ()) -> IO Str
newString n f = do
  arr <- newByteArray (n + 1)
  f (unMutableByteArray arr)
  writeByteArray arr (n + 1) (0 :: Word8)
  arr2 <- unsafeFreezeByteArray arr
  pure (Val (unByteArray arr2))

strTail :: Str -> IO Str
strTail (Lit addr) = pure (Lit (plusAddr# addr (unI 1)))
strTail v@(Val arrSrc) = do
  let s = strLength v
  newString (s - 1) $ \arrDst -> do
    primitive_ (copyByteArray# arrSrc (unI 1) arrDst (unI 0) (unI (s - 1)))

strIndex :: Str -> Int -> Char
strIndex (Lit addr) (I# i) = C# (indexCharOffAddr# addr i)
strIndex (Val arr)  (I# i) = C# (indexCharArray# arr i)

strCons :: Char -> Str -> IO Str
strCons (C# c) s@(Lit addr) = do
  let sl = strLength s
  newString (sl + 1) $ \arrDst -> do
    primitive_ (writeCharArray# arrDst (unI 0) c)
    primitive_ (copyAddrToByteArray# addr arrDst (unI 1) (unI sl))
strCons (C# c) s@(Val arrSrc) = do
  let sl = strLength s
  newString (sl + 1) $ \arrDst -> do
    primitive_ (writeCharArray# arrDst (unI 0) c)
    primitive_ (copyByteArray# arrSrc (unI 0) arrDst (unI 1) (unI sl))

strAppend :: Str -> Str -> IO Str
strAppend v1@(Lit addr1) v2@(Lit addr2) = do
  let s1 = strLength v1
  let s2 = strLength v2
  newString (s1 + s2) $ \arrDst -> do
    primitive_ (copyAddrToByteArray# addr1 arrDst (unI 0) (unI s1))
    primitive_ (copyAddrToByteArray# addr2 arrDst (unI s1) (unI s2))
strAppend v1@(Lit addr1) v2@(Val arr2) = do
  let s1 = strLength v1
  let s2 = strLength v2
  newString (s1 + s2) $ \arrDst -> do
    primitive_ (copyAddrToByteArray# addr1         arrDst (unI 0)  (unI s1))
    primitive_ (copyByteArray#       arr2  (unI 0) arrDst (unI s1) (unI s2))
strAppend v1@(Val arr1) v2@(Lit addr2) = do
  let s1 = strLength v1
  let s2 = strLength v2
  newString (s1 + s2) $ \arrDst -> do
    primitive_ (copyByteArray#       arr1  (unI 0) arrDst (unI 0)  (unI s1))
    primitive_ (copyAddrToByteArray# addr2         arrDst (unI s1) (unI s2))
strAppend v1@(Val arrSrc1) v2@(Val arrSrc2) = do
  let s1 = strLength v1
  let s2 = strLength v2
  newString (s1 + s2) $ \arrDst -> do
    primitive_ (copyByteArray# arrSrc1 (unI 0) arrDst (unI 0)  (unI s1))
    primitive_ (copyByteArray# arrSrc2 (unI 0) arrDst (unI s1) (unI s2))

strSubstr :: Int -> Int -> Str -> IO Str
strSubstr i n (Lit addr) = do
  newString n $ \arrDst -> do
    primitive_ (copyAddrToByteArray# (plusAddr# addr (unI i)) arrDst (unI 0) (unI n))
strSubstr i n (Val arr) = do
  newString n $ \arrDst -> do
    primitive_ (copyByteArray# arr (unI i) arrDst (unI 0) (unI n))

arrReverse :: MutableByteArray RealWorld -> Int -> Int -> IO ()
arrReverse arr s e =
  if s <= e
    then do
      sv :: Word8 <- readByteArray arr s
      ev :: Word8 <- readByteArray arr e
      writeByteArray arr e sv
      writeByteArray arr s ev
      arrReverse arr (s + 1) (e - 1)
    else pure ()

copyLitToVal1 :: Addr# -> IO (MutableByteArray RealWorld)
copyLitToVal1 addr = do
  let s = addrStrLength addr 0
  arr <- newByteArray (s + 1)
  primitive_ (copyAddrToByteArray# addr (unMutableByteArray arr) (unI 0) (unI s))
  writeByteArray arr s (0 :: Word8)
  pure arr

copyLitToVal :: Addr# -> IO ByteArray
copyLitToVal addr = copyLitToVal1 addr >>= unsafeFreezeByteArray

strReverse :: Str -> IO Str
strReverse v@(Lit addr) = do
  let s = strLength v
  newString s $ \arrDst -> do
    primitive_ (copyAddrToByteArray# addr arrDst (unI 0) (unI s))
    arrReverse (MutableByteArray arrDst) 0 (s - 1)
strReverse v@(Val arr) = do
  let s = strLength v
  newString s $ \arrDst -> do
    primitive_ (copyByteArray# arr (unI 0) arrDst (unI 0) (unI s))
    arrReverse (MutableByteArray arrDst) 0 (s - 1)

arrCompare :: ByteArray# -> ByteArray# -> Int -> Int
arrCompare arr1 arr2 n =
  let w1 = ord (C# (indexCharArray# arr1 (unI n)))
      w2 = ord (C# (indexCharArray# arr2 (unI n)))
  in case w1 of
      0 -> case w2 of
            0 -> 0
            _ -> let s1 = I# (sizeofByteArray# arr1)
                     s2 = I# (sizeofByteArray# arr2)
                 in (s1 - s2)
      _ -> case w2 of
            0 -> let s1 = I# (sizeofByteArray# arr1)
                     s2 = I# (sizeofByteArray# arr2)
                 in (s1 - s2)
            _ -> case w1 == w2 of
                   True  -> arrCompare arr1 arr2 (n + 1)
                   False -> w1 - w2

strCompare :: Str -> Str -> IO Int
strCompare (Lit addr1) (Lit addr2) = do
  arr1 <- copyLitToVal addr1
  arr2 <- copyLitToVal addr2
  pure $ arrCompare (unByteArray arr1) (unByteArray arr2) 0
strCompare (Lit addr1) (Val arr2) = do
  arr1 <- copyLitToVal addr1
  pure $ arrCompare (unByteArray arr1) arr2 0
strCompare (Val arr1) (Lit addr2) = do
  arr2 <- copyLitToVal addr2
  pure $ arrCompare arr1 (unByteArray arr2) 0
strCompare (Val arr1) (Val arr2) = do
  pure $ arrCompare arr1 arr2 0

strEq :: Str -> Str -> IO Int
strEq s1 s2 = do
  r <- strCompare s1 s2
  pure $ case r of
    0 -> 1
    _ -> 0
