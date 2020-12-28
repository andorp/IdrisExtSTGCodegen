{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
module IdrisString
  ( Str(..)
  , strLength
  , strHead
  , strTail
  , strIndex
  , strCons
  , strAppend
  , strReverse
  , strSubstr
  , test
  ) where

import GHC.Ptr (Ptr(Ptr), plusPtr)
import Data.Word (Word8)
import Data.Char (chr, ord)
import Foreign.Storable (peekElemOff)
import Control.Monad.Primitive (primitive_)
import GHC.Exts
  ( RealWorld
  , Int(I#)
  , Addr#
  , copyAddrToByteArray#
  )
import Data.Primitive.ByteArray
  ( MutableByteArray(..)
  , getSizeofMutableByteArray
  , readByteArray
  , copyMutableByteArray
  , newByteArray
  , writeByteArray
  , mutableByteArrayContents
  , foldrByteArray
  , freezeByteArray
  , copyByteArray
  , sizeofByteArray
  , byteArrayFromList
  )
import Test.QuickCheck
import Test.QuickCheck.Monadic


data Str
  = Lit Addr#
  | Val (MutableByteArray RealWorld)

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
  pure (Val arr')

addrStrToString :: Addr# -> Int -> IO [Word8]
addrStrToString a n = do
  c :: Word8 <- peekElemOff (Ptr a) n
  if c == 0
      then pure []
      else fmap (c:) $ addrStrToString a (n + 1)

toString :: Str -> IO String
toString (Lit addr) = fmap (map hsChar) $ addrStrToString addr 0
toString (Val arr) = do
  s <- getSizeofMutableByteArray arr
  arr' <- freezeByteArray arr 0 (s - 1)
  pure $ map hsChar $ foldrByteArray (:) [] arr'

strToLit :: Str -> Str
strToLit (Lit a)   = Lit a
strToLit (Val arr) = case mutableByteArrayContents arr of
  (Ptr addr) -> Lit addr

addrStrLength :: Addr# -> Int -> IO Int
addrStrLength a n = do
  c :: Word8 <- peekElemOff (Ptr a) n
  if c == 0
    then pure n
    else addrStrLength a (n + 1)

strLength :: Str -> IO Int
strLength (Lit addr) = addrStrLength addr 0
strLength (Val arr) = do
  s <- getSizeofMutableByteArray arr
  pure (s - 1)

strHead :: Str -> IO Word8
strHead (Lit addr) = peekElemOff (Ptr addr) 0
strHead (Val arr) = readByteArray arr 0

strTail :: Str -> IO Str
strTail (Lit addr) = pure $ (\(Ptr addr') -> Lit addr') $ plusPtr (Ptr addr) 1
strTail (Val arr) = do
  s <- getSizeofMutableByteArray arr
  arr' <- newByteArray (s - 1)
  copyMutableByteArray arr' 0 arr 1 (s - 1)
  pure (Val arr')

strIndex :: Str -> Int -> IO Word8
strIndex (Lit addr) i = peekElemOff (Ptr addr) i
strIndex (Val arr) i = readByteArray arr i

strCons :: Word8 -> Str -> IO Str
strCons c (Lit addr) = do
  n <- addrStrLength addr 0
  arr' <- newByteArray (n + 2)
  case (arr', 1, n + 1) of
    (MutableByteArray a, I# offset, I# lngth)
      -> primitive_ (copyAddrToByteArray# addr a offset lngth)
  writeByteArray arr' 0 c
  pure (Val arr')
strCons c (Val arr) = do
  s <- getSizeofMutableByteArray arr
  arr' <- newByteArray (s + 1)
  writeByteArray arr' 0 c
  copyMutableByteArray arr' 1 arr 0 s
  pure (Val arr')

strAppend :: Str -> Str -> IO Str
strAppend (Lit addr1) (Lit addr2) = do
  s1 <- addrStrLength addr1 0
  s2 <- addrStrLength addr2 0
  arr <- newByteArray (s1 + s2 + 1)
  case (arr, 0, s1) of
    (MutableByteArray a, I# offset, I# lngth)
      -> primitive_ (copyAddrToByteArray# addr1 a offset lngth)
  case (arr, s1, s2) of
    (MutableByteArray a, I# offset, I# lngth)
      -> primitive_ (copyAddrToByteArray# addr2 a offset lngth)
  pure (Val arr)
strAppend (Lit addr1) (Val arr2) = do
  s1 <- addrStrLength addr1 0
  s2 <- getSizeofMutableByteArray arr2
  arr <- newByteArray (s1 + s2) -- Ending 0 is included
  case (arr, 0, s1) of
    (MutableByteArray a, I# offset, I# lngth)
      -> primitive_ (copyAddrToByteArray# addr1 a offset lngth)
  copyMutableByteArray arr s1 arr2 0 s2
  pure (Val arr)
strAppend (Val arr1) (Lit addr2) = do
  s1 <- getSizeofMutableByteArray arr1 -- Ending 0 is included
  s2 <- addrStrLength addr2 0
  arr <- newByteArray (s1 + s2) -- Ending 0 is included
  copyMutableByteArray arr 0 arr1 0 s1
  case (arr, s1 - 1, s2) of
    (MutableByteArray a, I# offset, I# lngth)
      -> primitive_ (copyAddrToByteArray# addr2 a offset lngth)
  pure (Val arr)
strAppend (Val arr1) (Val arr2) = do
  s1 <- getSizeofMutableByteArray arr1
  s2 <- getSizeofMutableByteArray arr2
  arr' <- newByteArray (s1 + s2 - 1)
  copyMutableByteArray arr' 0 arr1 0 (s1 - 1)
  copyMutableByteArray arr' (s1 - 1) arr2 0 s2
  writeByteArray arr' (s1 + s2) (0 :: Word8)
  pure (Val arr')

strSubstr :: Str -> Int -> Int -> IO Str
strSubstr (Lit addr) i j = do
  let ns = (j - i + 1)
  arr' <- newByteArray ns
  case (arr', plusPtr (Ptr addr) i, 0, (j - i)) of
    (MutableByteArray a, Ptr addr', I# offset, I# lngth)
      -> primitive_ (copyAddrToByteArray# addr' a offset lngth)
  writeByteArray arr' ns (0 :: Word8)
  pure (Val arr')
strSubstr (Val arr) i j = do
  let ns = (j - i + 1)
  arr' <- newByteArray ns
  copyMutableByteArray arr' 0 arr i (j - i)
  writeByteArray arr' ns (0 :: Word8)
  pure (Val arr')

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

copyLitToVal :: Addr# -> IO (MutableByteArray RealWorld)
copyLitToVal addr = do
  s <- addrStrLength addr 0
  arr <- newByteArray (s + 1)
  case (arr, 0, s) of
    (MutableByteArray a, I# offset, I# lngth)
      -> primitive_ (copyAddrToByteArray# addr a offset lngth)
  writeByteArray arr s (0 :: Word8)
  pure arr

strReverse :: Str -> IO Str
strReverse (Lit addr) = do
  arr <- copyLitToVal addr
  s <- getSizeofMutableByteArray arr
  arrReverse arr 0 (s - 2)
  pure (Val arr)
strReverse (Val arr) = do
  s <- getSizeofMutableByteArray arr
  arr' <- newByteArray s
  copyMutableByteArray arr' 0 arr 0 s
  arrReverse arr' 0 (s - 2)
  pure (Val arr')

arrCompare :: MutableByteArray RealWorld -> MutableByteArray RealWorld -> Int -> IO Int
arrCompare arr1 arr2 n = do
  w1 :: Word8 <- readByteArray arr1 n
  w2 :: Word8 <- readByteArray arr2 n
  case w1 of
    0 -> case w2 of
          0 -> pure 0    -- both finished
          _ -> do
            s1 <- getSizeofMutableByteArray arr1
            s2 <- getSizeofMutableByteArray arr2
            pure (s1 - s2) -- first finished earlier
    _ -> case w2 of
          0 -> do -- second finished earlier
            s1 <- getSizeofMutableByteArray arr1
            s2 <- getSizeofMutableByteArray arr2
            pure (s1 - s2)
          _ -> case w1 == w2 of
                True -> arrCompare arr1 arr2 (n + 1) -- next char
                False -> do --
                  let c1 = fromIntegral w1
                  let c2 = fromIntegral w2
                  pure (c1 - c2)

strCompare :: Str -> Str -> IO Int
strCompare (Lit addr1) (Lit addr2) = do
  arr1 <- copyLitToVal addr1
  arr2 <- copyLitToVal addr2
  arrCompare arr1 arr2 0
strCompare (Lit addr1) (Val arr2) = do
  arr1 <- copyLitToVal addr1
  arrCompare arr1 arr2 0
strCompare (Val arr1) (Lit addr2) = do
  arr2 <- copyLitToVal addr2
  arrCompare arr1 arr2 0
strCompare (Val arr1) (Val arr2) = arrCompare arr1 arr2 0

-- * Test

genString :: Gen String
genString = listOf $ elements ['a' .. 'z']

genString1 :: Gen String
genString1 = listOf1 $ elements ['a' .. 'z']

stringOfN :: Int -> Gen String
stringOfN n = vectorOf n $ elements ['a' .. 'z']

genFromString :: String -> PropertyM IO Str
genFromString xs = do
  xs1 <- run $ IdrisString.fromString xs
  lit <- pick $ arbitrary
  if lit
    then pure $ strToLit xs1
    else pure xs1

test :: IO ()
test = do

  putStrLn "toString . fromString"
  quickCheck $ monadicIO $ do
    xs <- pick genString
    ys <- genFromString xs
    zs <- run $ IdrisString.toString ys
    assert $ xs == zs

  putStrLn "strLength"
  quickCheck $ monadicIO $ do
    xs  <- pick genString
    xs1 <- genFromString xs
    n   <- run $ strLength xs1
    assert $ length xs == n

  putStrLn "strHead"
  quickCheck $ monadicIO $ do
    xs  <- pick genString1
    xs1 <- genFromString xs
    x   <- run $ strHead xs1
    assert $ head xs == hsChar x

  putStrLn "strTail"
  quickCheck $ monadicIO $ do
    xs  <- pick genString1
    xs1 <- genFromString xs
    ys  <- run $ strTail xs1
    ys' <- run $ IdrisString.toString ys
    assert $ tail xs == ys'

  putStrLn "strIndex"
  quickCheck $ monadicIO $ do
    xs  <- pick genString1
    xs1 <- genFromString xs
    let n = length xs
    i   <- pick $ elements [0..(n - 1)]
    ci  <- run $ strIndex xs1 i
    assert $ (xs !! i) == hsChar ci

  putStrLn "strCons"
  quickCheck $ monadicIO $ do
    xs  <- pick genString
    c   <- pick $ elements ['a' .. 'z']
    xs1 <- genFromString xs
    xs2 <- run $ strCons (mkChar c) xs1
    xs3 <- run $ IdrisString.toString xs2
    assert $ (c:xs) == xs3

  putStrLn "strAppend"
  quickCheck $ monadicIO $ do
    xs  <- pick genString
    ys  <- pick genString
    xs1 <- genFromString xs
    ys1 <- genFromString ys
    zs1 <- run $ strAppend xs1 ys1
    zs  <- run $ IdrisString.toString zs1
    assert $ xs ++ ys == zs

  putStrLn "strReverse"
  quickCheck $ monadicIO $ do
    xs  <- pick genString
    xs1 <- genFromString xs
    ys1 <- run $ strReverse xs1
    ys  <- run $ IdrisString.toString ys1
    assert $ reverse xs == ys

  putStrLn "strSubstr"
  quickCheck $ monadicIO $ do
    n <- pick $ fmap ((+2) . abs) arbitrary
    xs  <- pick $ stringOfN n
    xs1 <- genFromString xs
    i   <- pick $ elements [0..(n-2)]
    j   <- pick $ elements [i+1..(n-1)]
    ys1 <- run $ strSubstr xs1 i j
    ys  <- run $ IdrisString.toString ys1
    let zs = drop i $ take j xs
    assert $ ys == zs

  putStrLn "strCompare"
  quickCheck $ monadicIO $ do
    xs <- pick genString
    ys <- pick genString
    xs1 <- genFromString xs
    ys1 <- genFromString ys
    cmp <- run $ strCompare xs1 ys1
    let cmp1 = if cmp < 0 then LT else (if cmp == 0 then EQ else GT)
    let cmp0 = compare xs ys
    assert $ cmp1 == cmp0
