{-# LANGUAGE MagicHash, UnboxedTuples #-}
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


import GHC.Ptr (Ptr(Ptr))
import Data.Word
import Data.Char (chr, ord)
import Foreign.Storable (peekElemOff)
import Control.Monad.Primitive (primitive_)
import GHC.Exts
  ( RealWorld
  , Int(I#)
  , Char(C#)
  , Int#
  , Addr#
  , ByteArray#
  , copyAddrToByteArray#
  , byteArrayContents#
  , sizeofByteArray#
  , indexCharArray#
  , copyByteArray#
  , plusAddr#
  )
import Data.Primitive.ByteArray
  ( MutableByteArray(..)
  , ByteArray(..)
  , MutableByteArray#
  , unsafeFreezeByteArray
  , getSizeofMutableByteArray
  , readByteArray
  , newByteArray
  , writeByteArray
  , foldrByteArray
  , copyByteArray
  , sizeofByteArray
  , byteArrayFromList
  )
import Test.QuickCheck
import Test.QuickCheck.Monadic


unI :: Int -> Int#
unI (I# i) = i

unByteArray :: ByteArray -> ByteArray#
unByteArray (ByteArray arr) = arr

unMutableByteArray :: MutableByteArray RealWorld -> MutableByteArray# RealWorld
unMutableByteArray (MutableByteArray arr) = arr

data Str
  = Lit Addr#
  | Val ByteArray#

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

addrStrToString :: Addr# -> Int -> IO [Word8]
addrStrToString a n = do
  c :: Word8 <- peekElemOff (Ptr a) n
  if c == 0
      then pure []
      else fmap (c:) $ addrStrToString a (n + 1)

toString :: Str -> IO String
toString (Lit addr) = fmap (map hsChar) $ addrStrToString addr 0
toString (Val arr) = do
  pure $ reverse $ drop 1 $ reverse $ map hsChar $ foldrByteArray (:) [] (ByteArray arr)

strToLit :: Str -> Str
strToLit (Lit a)   = Lit a
strToLit (Val arr) = Lit (byteArrayContents# arr)

addrStrLength :: Addr# -> Int -> IO Int
addrStrLength a n = do
  c :: Word8 <- peekElemOff (Ptr a) n
  if c == 0
    then pure n
    else addrStrLength a (n + 1)

strLength :: Str -> IO Int
strLength (Lit addr) = addrStrLength addr 0
strLength (Val arr) = pure ((I# (sizeofByteArray# arr)) - 1)

strHead :: Str -> IO Word8
strHead (Lit addr) = peekElemOff (Ptr addr) 0
strHead (Val arr) = pure (fromIntegral (ord (C# (indexCharArray# arr (unI 0)))))

strTail :: Str -> IO Str
strTail (Lit addr) = pure (Lit (plusAddr# addr (unI 1)))
strTail (Val arr) = pure (Lit (plusAddr# (byteArrayContents# arr) (unI 1)))

strIndex :: Str -> Int -> IO Word8
strIndex (Lit addr) i = peekElemOff (Ptr addr) i
strIndex (Val arr) (I# i) = pure (fromIntegral (ord (C# (indexCharArray# arr i))))

strCons :: Word8 -> Str -> IO Str
strCons c (Lit addr) = do
  n <- addrStrLength addr 0
  arr' <- newByteArray (n + 2)
  primitive_ (copyAddrToByteArray# addr (unMutableByteArray arr') (unI 1) (unI (n + 1)))
  writeByteArray arr' 0 c
  arr2 <- unsafeFreezeByteArray arr'
  pure (Val (unByteArray arr2))
strCons c (Val arr) = do
  let s = sizeofByteArray# arr
  arr1 <- newByteArray ((I# s) + 1)
  writeByteArray arr1 0 c
  primitive_ (copyByteArray# arr (unI 0) (unMutableByteArray arr1) (unI 1) s)
  arr2 <- unsafeFreezeByteArray arr1
  pure (Val (unByteArray arr2))

strAppend :: Str -> Str -> IO Str
strAppend (Lit addr1) (Lit addr2) = do
  s1 <- addrStrLength addr1 0
  s2 <- addrStrLength addr2 0
  arr <- newByteArray (s1 + s2 + 1)
  primitive_ (copyAddrToByteArray# addr1 (unMutableByteArray arr) (unI 0) (unI s1))
  primitive_ (copyAddrToByteArray# addr2 (unMutableByteArray arr) (unI s1) (unI s2))
  arr2 <- unsafeFreezeByteArray arr
  pure (Val (unByteArray arr2))
strAppend (Lit addr1) (Val arr2) = do
  s1 <- addrStrLength addr1 0
  let s2 = sizeofByteArray# arr2
  arr <- newByteArray (s1 + (I# s2)) -- Ending 0 is included
  primitive_ (copyAddrToByteArray# addr1         (unMutableByteArray arr) (unI 0)  (unI s1))
  primitive_ (copyByteArray#       arr2  (unI 0) (unMutableByteArray arr) (unI s1) s2)
  arr3 <- unsafeFreezeByteArray arr
  pure (Val (unByteArray arr3))
strAppend (Val arr1) (Lit addr2) = do
  let s1 = sizeofByteArray# arr1 -- Ending 0 is included
  s2 <- addrStrLength addr2 0
  arr <- newByteArray ((I# s1) + s2) -- Ending 0 is included
  primitive_ (copyByteArray#       arr1  (unI 0) (unMutableByteArray arr) (unI 0)             s1)
  primitive_ (copyAddrToByteArray# addr2         (unMutableByteArray arr) (unI ((I# s1) - 1)) (unI s2))
  arr3 <- unsafeFreezeByteArray arr
  pure (Val (unByteArray arr3))
strAppend (Val arr1) (Val arr2) = do
  let s1 = sizeofByteArray# arr1
  let s2 = sizeofByteArray# arr2
  arr' <- newByteArray ((I# s1) + (I# s2) - 1)
  primitive_ (copyByteArray# arr1 (unI 0) (unMutableByteArray arr') (unI 0) (unI ((I# s1) - 1)))
  primitive_ (copyByteArray# arr2 (unI 0) (unMutableByteArray arr') (unI ((I# s1) - 1)) s2)
  writeByteArray arr' ((I# s1) + (I# s2)) (0 :: Word8)
  arr3 <- unsafeFreezeByteArray arr'
  pure (Val (unByteArray arr3))

strSubstr :: Str -> Int -> Int -> IO Str
strSubstr (Lit addr) i j = do
  let ns = (j - i + 1)
  arr' <- newByteArray ns
  primitive_ (copyAddrToByteArray# (plusAddr# addr (unI i)) (unMutableByteArray arr') (unI 0) (unI (j - i)))
  writeByteArray arr' ns (0 :: Word8)
  arr2 <- unsafeFreezeByteArray arr'
  pure (Val (unByteArray arr2))
strSubstr (Val arr) i j = do
  let ns = (j - i + 1)
  arr' <- newByteArray ns
  primitive_ (copyByteArray# arr (unI i) (unMutableByteArray arr') (unI 0) (unI (j - i)))
  writeByteArray arr' ns (0 :: Word8)
  arr3 <- unsafeFreezeByteArray arr'
  pure (Val (unByteArray arr3))


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
  s <- addrStrLength addr 0
  arr <- newByteArray (s + 1)
  primitive_ (copyAddrToByteArray# addr (unMutableByteArray arr) (unI 0) (unI s))
  writeByteArray arr s (0 :: Word8)
  pure arr

copyLitToVal2 :: Addr# -> IO ByteArray
copyLitToVal2 addr = copyLitToVal1 addr >>= unsafeFreezeByteArray

strReverse :: Str -> IO Str
strReverse (Lit addr) = do
  arr <- copyLitToVal1 addr
  s <- getSizeofMutableByteArray arr
  arrReverse arr 0 (s - 2)
  arr2 <- unsafeFreezeByteArray arr
  pure (Val (unByteArray arr2))
strReverse (Val arr) = do
  let s = sizeofByteArray# arr
  arr' <- newByteArray (I# s)
  primitive_ (copyByteArray# arr (unI 0) (unMutableByteArray arr') (unI 0) s)
  arrReverse arr' 0 ((I# s) - 2)
  arr3 <- unsafeFreezeByteArray arr'
  pure (Val (unByteArray arr3))

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
  arr1 <- copyLitToVal2 addr1
  arr2 <- copyLitToVal2 addr2
  pure $ arrCompare (unByteArray arr1) (unByteArray arr2) 0
strCompare (Lit addr1) (Val arr2) = do
  arr1 <- copyLitToVal2 addr1
  pure $ arrCompare (unByteArray arr1) arr2 0
strCompare (Val arr1) (Lit addr2) = do
  arr2 <- copyLitToVal2 addr2
  pure $ arrCompare arr1 (unByteArray arr2) 0
strCompare (Val arr1) (Val arr2) = do
  pure $ arrCompare arr1 arr2 0

-- * Test

genString :: Gen String
genString = listOf $ elements ['a' .. 'z']

genString1 :: Gen (NonEmptyList Char)
genString1 = fmap NonEmpty $ listOf1 $ elements ['a' .. 'z']

stringOfN :: Int -> Gen (NonEmptyList Char)
stringOfN n = fmap NonEmpty $ vectorOf n $ elements ['a' .. 'z']

genFromString :: String -> PropertyM IO Str
genFromString xs = do
  xs1 <- run $ IdrisString.fromString xs
  lit <- pick $ arbitrary
  if lit
    then pure $ strToLit xs1
    else pure xs1

bigCheck :: Testable prop => prop -> IO ()
bigCheck = quickCheckWith (stdArgs {maxSuccess = 100, maxSize = 10000})

test :: IO ()
test = do

  putStrLn "toString . fromString"
  bigCheck $ forAllShrink genString shrink $ \xs -> monadicIO $ do
    ys <- genFromString xs
    zs <- run $ IdrisString.toString ys
    assert $ xs == zs

  putStrLn "strLength"
  bigCheck $ forAllShrink genString shrink $ \xs -> monadicIO $ do
    xs1 <- genFromString xs
    n   <- run $ strLength xs1
    assert $ length xs == n

  putStrLn "strHead"
  bigCheck $ forAllShrink genString1 shrink $ \(NonEmpty xs) -> monadicIO $ do
    xs1 <- genFromString xs
    x   <- run $ strHead xs1
    assert $ head xs == hsChar x

  putStrLn "strTail"
  bigCheck $ forAllShrink genString1 shrink $ \(NonEmpty xs) -> monadicIO $ do
    xs1 <- genFromString xs
    ys  <- run $ strTail xs1
    ys' <- run $ IdrisString.toString ys
    assert $ tail xs == ys'

  putStrLn "strIndex"
  bigCheck $ forAllShrink genString1 shrink                 $ \(NonEmpty xs) ->
             forAll       (elements [0 .. (length xs - 1)]) $ \i ->
             monadicIO $ do
    xs1 <- genFromString xs
    ci  <- run $ strIndex xs1 i
    assert $ (xs !! i) == hsChar ci

  putStrLn "strCons"
  bigCheck $ forAllShrink genString shrink $ \xs -> monadicIO $ do
    c   <- pick $ elements ['a' .. 'z']
    xs1 <- genFromString xs
    xs2 <- run $ strCons (mkChar c) xs1
    xs3 <- run $ IdrisString.toString xs2
    assert $ (c:xs) == xs3

  putStrLn "strAppend"
  bigCheck $ forAllShrink genString shrink $ \xs ->
             forAllShrink genString shrink $ \ys ->
             monadicIO $ do
    xs1 <- genFromString xs
    ys1 <- genFromString ys
    zs1 <- run $ strAppend xs1 ys1
    zs  <- run $ IdrisString.toString zs1
    assert $ xs ++ ys == zs

  putStrLn "strReverse"
  bigCheck $ forAllShrink genString shrink $ \xs -> monadicIO $ do
    xs1 <- genFromString xs
    ys1 <- run $ strReverse xs1
    ys  <- run $ IdrisString.toString ys1
    assert $ reverse xs == ys

  putStrLn "strSubstr"
  bigCheck $ forAllShrink arbitrary shrink  $ \(Positive n') -> let n = n' + 2 in
             forAll       (stringOfN n)     $ \(NonEmpty xs) ->
             monadicIO $ do
    xs1 <- genFromString xs
    i   <- pick $ elements [0..(n-2)]
    j   <- pick $ elements [i+1..(n-1)]
    ys1 <- run $ strSubstr xs1 i j
    ys  <- run $ IdrisString.toString ys1
    let zs = drop i $ take j xs
    assert $ ys == zs

  putStrLn "strCompare"
  bigCheck $ forAllShrink genString shrink $ \xs ->
             forAllShrink genString shrink $ \ys ->
             monadicIO $ do
    xs1 <- genFromString xs
    ys1 <- genFromString ys
    cmp <- run $ strCompare xs1 ys1
    let cmp1 = if cmp < 0 then LT else (if cmp == 0 then EQ else GT)
    let cmp0 = compare xs ys
    assert $ cmp1 == cmp0
