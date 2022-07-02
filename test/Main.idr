module Main

import System
import System.Directory
import System.File

import Test.Golden

%default covering

primOpsTests : TestPool
primOpsTests = MkTestPool "PrimOps" [] Nothing
  [ "test0"
  , "test1"
  , "test2"
  , "test3"
  , "test4"
  , "test5"
  , "test6"
  , "test7"
  ]

ffiTests : TestPool
ffiTests = MkTestPool "FFI" [] Nothing
  [ "test0"
  , "test1"
  , "test2"
  , "test3"
  , "test4"
  , "test5"
  ]

chezTests : TestPool
chezTests = MkTestPool "Chez" [] Nothing
  [ "chez009"
  ] 

idris2Tests : TestPool
idris2Tests = MkTestPool "Idris2" [] Nothing
  -- [ "basic055" ] -- WordRep vs Word64Rep errors
  [ "basic045"
  , "basic054"
  -- , "basic055" -- WordRep vs Word64Rep errors
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
  , "total006"
  ]

tyddTests : TestPool
tyddTests = MkTestPool "Type Driven Development" [] Nothing
  [ "chapter01"
  , "chapter02-average"
  , "chapter02-reverse"
  , "chapter04-data-store"
  , "chapter04-sum-inputs"
  , "chapter05-dep-pairs"
  , "chapter05-hello"
  , "chapter05-loops"
  , "chapter06"
  , "chapter09"
  , "chapter11-arith"
  , "chapter11-arith-cmd"
  , "chapter11-arith-cmd-do"
  , "chapter11-arith-total"
  , "chapter11-infio"
  , "chapter11-runio"
  , "chapter12-traverse"
  , "chapter12-arithstate"
  , "chapter13-stackio"
  , "chapter13-vending"
  , "chapter14"
  ]

allBackends : TestPool
allBackends = MkTestPool "All backends" [] Nothing
  [ "basic048"
  , "evaluator004"
  , "evaluator005"
  , "issue2362"
  , "perf006"
  ]

testPaths : String -> TestPool -> TestPool
testPaths dir = { testCases $= map ((dir ++ "/") ++) }

devTestCase : TestPool
devTestCase = MkTestPool "Under development" [] Nothing
  [ "test7"
  ]

main : IO ()
main = runner $
  -- [ testPaths "idris2" idris2Tests ]
  [ testPaths "prim-ops" primOpsTests
  , testPaths "ffi" ffiTests
  , testPaths "idris2" idris2Tests
  , testPaths "typedd-book" tyddTests
  ]
