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

chezTests : TestPool
chezTests = MkTestPool "Chez" [] Nothing
  [ "chez009"
  ] 

idris2Tests : TestPool
idris2Tests = MkTestPool "Idris2" [] Nothing
  [ "basic045"
  , "basic054"
  , "basic055"
  , "basic056"
  , "basic068"
  , "builtin009"
  , "builtin011"
  , "builtin012"
  , "idiom001"
  , "perf002"
  , "perf008"
  , "record015"
  , "reflection014"
  , "reg008"
  -- , "total006" -- TODO: Support laziness and streams!
  ]

testPaths : String -> TestPool -> TestPool
testPaths dir = { testCases $= map ((dir ++ "/") ++) }

main : IO ()
main = runner $
  [ testPaths "idris2" idris2Tests
  ]
  -- [ testPaths "idris2" idris2Tests
  -- , testPaths "prim-ops" primOpsTests
  -- , testPaths "ffi"      ffiTests
  -- , testPaths "chez"     chezTests
  -- ]
