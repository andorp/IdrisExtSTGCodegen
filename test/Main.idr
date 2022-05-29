module Main

import System
import System.Directory
import System.File

import Test.Golden

%default covering

primOpsTests : TestPool
primOpsTests = MkTestPool "PrimOps" [] Nothing
  [ "test0", "test1", "test2", "test3", "test4", "test5"
  ]

ffiTests : TestPool
ffiTests = MkTestPool "FFI" [] Nothing
  [ "test0", "test1", "test2", "test3", "test4", "test5"
  ]

testPaths : String -> TestPool -> TestPool
testPaths dir = { testCases $= map ((dir ++ "/") ++) }

main : IO ()
main = runner $
  [ testPaths "prim-ops" primOpsTests
  , testPaths "ffi"      ffiTests
  ]
