module Idris.Codegen.ExtSTG.String

import Core.Context
import Core.Core
import Core.TT
import Idris.Codegen.ExtSTG.STG
import Idris.Codegen.ExtSTG.Core

{-
This module contains the STG implementation of the String handling primitives.

The high level Haskell codes are copied over as comments but the working implementation
can be found in the IdristString.hs file.
-}

import Idris.Codegen.ExtSTG.STG

-- data IdrisString
--   = IdrisStringLit Addr#
--   | IdrisStringDyn (MutableByteArray RealWorld)
--
-- but we replace MutableByteArray with ByteArray

export
litConId
  :  {auto _ : UniqueMapRef}
  -> {auto _ : Ref Counter Int}
  -> Core DataConId
litConId = MkDataConId <$> uniqueForTerm "IdrisStringLit"

export
valConId
  :  {auto _ : UniqueMapRef}
  -> {auto _ : Ref Counter Int}
  -> Core DataConId
valConId = MkDataConId <$> uniqueForTerm "IdrisStringDyn"

export
IdrisString
  :  {auto _ : UniqueMapRef}
  -> {auto _ : Ref Counter Int}
  -> Core STyCon
IdrisString = pure $
  MkSTyCon
    "IdrisString"
    (MkTyConId !(uniqueForType "IdrisString"))
    [ MkSDataCon
        "IdrisStringLit"
        !litConId
        (UnboxedTupleCon 1) -- ??? Unboxed because it will store Addr# which is unboxed?
        !(mkSBinderStr emptyFC "mkIdrisStringLit")
        (SsUnhelpfulSpan "IdrisStringLit")
    , MkSDataCon
        "IdrisStringDyn"
        !valConId
        (UnboxedTupleCon 1) -- ??? How to refer to mutable array? In primitive ops is: MutableByteArray# s
        !(mkSBinderStr emptyFC "mkIdrisStringDyn")
        (SsUnhelpfulSpan "IdrisStringDyn")
    ]
    (SsUnhelpfulSpan "IdrisString")

-- strToLit :: IdrisString -> IdrisString
-- strToLit (IdrisStringLit a) = IdrisStringLit a
-- strToLit (IdrisStringDyn a) = IdrisStringLit (byteArrayContents# a)

strToLit
  :  {auto _ : UniqueMapRef}
  -> {auto _ : Ref Counter Int}
  -> Core STopBinding
strToLit = pure
  $ StgTopLifted
  $ StgNonRec
    !(mkSBinderTopLevel "strToLit")
    $ StgRhsClosure
      Updatable -- ??? What are the update flags?
      [!(mkSBinderLocalStr "strToLit1")]
      $ StgCase
        (StgApp (MkBinderId !(uniqueForTerm "strToLit1")) [] (SingleValue LiftedRep))
        !(mkSBinderLocalStr "strToLit2")
        (PrimAlt UnliftedRep) -- ??? UnlifedRep for match on data constructors?
        [ MkAlt
            (AltDataCon !litConId)
            [!(mkSBinderLocalStr "strToLit3")]
            (StgConApp !litConId [StgVarArg (MkBinderId !(uniqueForTerm "strToLit3"))] [])
        , MkAlt
            (AltDataCon !valConId)
            [!(mkSBinderLocalStr "strToLit4")]
            $ StgCase
              (StgOpApp
                (StgPrimOp "byteArrayContents#")
                [StgVarArg (MkBinderId !(uniqueForTerm "strToLit4"))]
                (SingleValue AddrRep) -- ??? Is this right? This is the type of Addr# ?
                Nothing) -- ??? What is the result type name of Addr#
              !(mkSBinderLocalStr "strToLit5")
              (PrimAlt AddrRep)
              [ MkAlt
                  AltDefault []
                  (StgConApp !litConId [StgVarArg (MkBinderId !(uniqueForTerm "strToLit4"))] [])
              ]
        ]

stringModuleTypes
  :  {auto _ : UniqueMapRef}
  -> {auto _ : Ref Counter Int}
  -> Core (List STyCon)
stringModuleTypes = pure
  [ !IdrisString
  ]

export
stringModuleFunctions
  :  {auto _ : UniqueMapRef}
  -> {auto _ : Ref Counter Int}
  -> Core (List STopBinding)
stringModuleFunctions = pure
  [ !strToLit
  ]
