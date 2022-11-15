module Idris.Codegen.ExtSTG.DiscoverADTs

import Core.Context
import Core.Core
import Data.List
import Data.Maybe
import Data.SortedMap
import Data.String

import Idris.Codegen.ExtSTG.ADTAlias
import Idris.Codegen.ExtSTG.Context
import Idris.Codegen.ExtSTG.Core
import Idris.Codegen.ExtSTG.ExtName
import Idris.Codegen.ExtSTG.STG

%default total

{-
Discover the data and type constructors of the compiled program and register them in the ADTMap.

- Enumerate all the definitions from the context
- Act only on the DCon and TCon
- First register all the DCon: Create unique, DataCon info, and register it with ADTInfo
- Second register all the TCon: Create unique, DataCon info, and register it with ADTInfo
-}

checkArity : Name.Name -> Int -> Core Nat
checkArity n a = case a >= 0 of
  True  => pure $ cast a
  False => coreFail $ InternalError "\{show n} has invalid computed arity \{show a}"

covering
discoverDataCons : Ref Ctxt Defs => Ref STGCtxt STGContext => Name.Name -> Core SDataConSg
discoverDataCons n = do
  ctx <- gamma <$> get Ctxt
  mdef <- lookupCtxtExactI n ctx
  case mdef of
    Nothing => coreFail $ InternalError "discoverDataCons no data constructor is found for \{show n}"
    Just (_, def) => case definition def of
      DCon tag arity nta => do
        let realArityInt : Int = (cast arity) - cast (length (eraseArgs def))
        realArity <- checkArity n realArityInt
        let rep = AlgDataCon (replicate realArity LiftedRep)
        insertDTCon n rep (location def)
      other => coreFail $ InternalError "discoverDataCons not a data constructor for \{show n}"

covering
discoverTypes : Ref Ctxt Defs => Ref STGCtxt STGContext => Core ()
discoverTypes = do
  ctx <- gamma <$> get Ctxt
  traverse_
    (\n => do
        mdef <- lookupCtxtExactI n ctx
        case mdef of
          Nothing => pure ()
          Just (_, def) => case definition def of
            TCon tag arity parampos detpos flags mutwith datacons detaggable => do
              sdatacons <- traverse discoverDataCons datacons
              let realArityInt : Int = (cast arity) - cast (length (eraseArgs def))
              realArity <- checkArity n realArityInt
              unitRet $ insertTYCon n realArity sdatacons (location def)
            other => pure ())
    !(allNames ctx)

discoverPrimTypes : Ref Ctxt Defs => Ref STGCtxt STGContext => Core ()
discoverPrimTypes = do
  _ <- traverse registerPrimType
        [ CharType
        , IntType
        , IntegerType
        , Int8Type
        , Int16Type
        , Int32Type
        , Int64Type
        , Bits8Type
        , Bits16Type
        , Bits32Type
        , Bits64Type
        , DoubleType
        , WorldType
        ]
  pure ()

export
covering
discoverADTs : Ref Ctxt Defs => Ref STGCtxt STGContext => Core ()
discoverADTs = do
  discoverPrimTypes
  discoverTypes
  unitRet createTypeOfTypes

