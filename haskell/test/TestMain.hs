module Main where

import Idris.Runtime.String
import Test.QuickCheck
import Test.QuickCheck.Monadic

main :: IO ()
main = test

-- * Test

genString :: Gen String
genString = listOf $ elements ['a' .. 'z']

genString1 :: Gen (NonEmptyList Char)
genString1 = fmap NonEmpty $ listOf1 $ elements ['a' .. 'z']

stringOfN :: Int -> Gen (NonEmptyList Char)
stringOfN n = fmap NonEmpty $ vectorOf n $ elements ['a' .. 'z']

genFromString :: String -> PropertyM IO Str
genFromString xs = do
  xs1 <- run $ Idris.Runtime.String.fromString xs
  -- lit <- pick $ arbitrary
  if False -- lit
    then run $ strToLit xs1
    else pure xs1

bigCheck :: Testable prop => prop -> IO ()
bigCheck prop = do
  -- quickCheckWith (stdArgs {maxSuccess = 10000, maxSize = 1000000}) prop
  quickCheckWith (stdArgs {maxSuccess = 1000, maxSize = 1000000}) prop
  -- performMajorGC

test :: IO ()
test = do
  pure ()

  putStrLn "toString . fromString"
  bigCheck $ forAll genString $ \xs -> monadicIO $ do
    ys <- genFromString xs
    let zs = Idris.Runtime.String.toString ys
    assert $ xs == zs

  putStrLn "strLength"
  bigCheck $ forAll genString $ \xs -> monadicIO $ do
    xs1 <- genFromString xs
    let n = strLength xs1
    assert $ length xs == n

  putStrLn "strHead"
  bigCheck $ forAll genString1 $ \(NonEmpty xs) -> monadicIO $ do
    xs1 <- genFromString xs
    let x = strHead xs1
    assert $ head xs == x

  putStrLn "strTail"
  bigCheck $ forAll genString1 $ \(NonEmpty xs) -> monadicIO $ do
    xs1 <- genFromString xs
    ys  <- run $ strTail xs1
    let ys' = Idris.Runtime.String.toString ys
    assert $ tail xs == ys'

  putStrLn "strIndex"
  bigCheck $ forAll genString1                 $ \(NonEmpty xs) ->
             forAll       (elements [0 .. (length xs - 1)]) $ \i ->
             monadicIO $ do
    xs1 <- genFromString xs
    let ci = strIndex xs1 i
    assert $ (xs !! i) == ci

  putStrLn "strCons"
  bigCheck $ forAll genString $ \xs -> monadicIO $ do
    c   <- pick $ elements ['a' .. 'z']
    xs1 <- genFromString xs
    xs2 <- run $ strCons c xs1
    let xs3 = Idris.Runtime.String.toString xs2
    assert $ (c:xs) == xs3

  putStrLn "strAppend"
  bigCheck $ forAll genString $ \xs ->
             forAll genString $ \ys ->
             monadicIO $ do
    xs1 <- genFromString xs
    ys1 <- genFromString ys
    zs1 <- run $ strAppend xs1 ys1
    let zs = Idris.Runtime.String.toString zs1
    assert $ xs ++ ys == zs

  putStrLn "strReverse"
  bigCheck $ forAll genString $ \xs -> monadicIO $ do
    xs1 <- genFromString xs
    ys1 <- run $ strReverse xs1
    let ys = Idris.Runtime.String.toString ys1
    assert $ reverse xs == ys

{- TODO: Fix this later
  putStrLn "strSubstr"
  bigCheck $ forAll arbitrary     $ \(Positive n') -> let n = n' + 2 in
             forAll (stringOfN n) $ \(NonEmpty xs) ->
             monadicIO $ do
    xs1 <- genFromString xs
    i   <- pick $ elements [0..(n-1)]
    j   <- pick $ elements [0..(n-i-1)]
    ys1 <- run $ strSubstr i j xs1
    let ys = Idris.Runtime.String.toString ys1
    let zs = drop i $ take j xs
    assert $ ys == zs
-}

  putStrLn "strCompare"
  bigCheck $ forAll genString $ \xs ->
             forAll genString $ \ys ->
             monadicIO $ do
    xs1 <- genFromString xs
    ys1 <- genFromString ys
    cmp <- run $ strCompare xs1 ys1
    let cmp1 = if cmp < 0 then LT else (if cmp == 0 then EQ else GT)
    let cmp0 = compare xs ys
    assert $ cmp1 == cmp0
