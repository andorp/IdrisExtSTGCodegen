module Idris.Codegen.ExtSTG.ADTMap

import Data.IntMap
import Data.List
import Data.Strings
import Core.Context
import Core.Core
import Idris.Codegen.ExtSTG.STG
import Idris.Codegen.ExtSTG.Core


||| Names of the data constructors must be associated with their STyCons, this information
||| is needed for the code generator when we generate STG case expressions
namespace DataConstructorsToTypeDefinitions
  ||| Datatypes annotation for Ref
  export
  data ADTs : Type where

  ||| Associates the resolved names with the TyCon.
  export
  ADTMap : Type
  ADTMap = IntMap STyCon

  ||| Create the ADTs reference
  export
  mkADTs : Core (Ref ADTs ADTMap)
  mkADTs = newRef ADTs empty

  ||| Constructors annotation for Ref.
  data Cons : Type where

  ConsMap : Type
  ConsMap = IntMap Core.Name.Name

  mkConsMap : Core (Ref Cons ConsMap)
  mkConsMap = newRef Cons empty

  ||| Gets the Int identifies through its Resolved name.
  export
  resolvedNameId
    :  {auto _ : Ref Ctxt Defs}
    -> Core.Name.Name
    -> Core Int
  resolvedNameId n = do
    (Resolved r) <- toResolvedNames n
      | _ => coreFail $ InternalError $ "Name doesn't have resolved id: " ++ show n
    pure r

  ||| Register the consturctor name for the STyCon
  export
  registerDataConToTyCon
    :  {auto _ : Ref ADTs ADTMap}
    -> {auto _ : Ref Ctxt Defs}
    -> STyCon
    -> Core.Name.Name
    -> Core ()
  registerDataConToTyCon s n = do
    r <- resolvedNameId n
    m <- get ADTs
    case lookup r m of
      Just st => coreFail
               $ InternalError
               $ show !(toFullNames n) ++ " is already registered for " ++ STyCon.Name st
      Nothing => do
        put ADTs (insert r s m)

  ||| Lookup if there is an ADT defined for the given name either for type name or data con name.
  export
  lookupTyCon
    :  {auto _ : Ref ADTs ADTMap}
    -> {auto _ : Ref Ctxt Defs}
    -> Core.Name.Name
    -> Core (Maybe STyCon)
  lookupTyCon n = lookup <$> resolvedNameId n <*> get ADTs

||| Global definition mapping for types and data constructors.
namespace TConsAndDCons

  ||| Label for TAC reference
  data TAC : Type where

  ||| Types and data constructors
  ||| Maps Resolved identifiers to GlobalDefinitions
  record TyAndCnstrs where
    constructor MkTyAndCnstrs
    Types : IntMap GlobalDef
    Cntrs : IntMap GlobalDef

  mkTACRef : Core (Ref TAC TyAndCnstrs)
  mkTACRef = newRef TAC (MkTyAndCnstrs empty empty)

  addTypeOrCnst
    :  {auto _ : Ref TAC TyAndCnstrs}
    -> {auto _ : Ref Ctxt Defs}
    -> GlobalDef
    -> Core ()
  addTypeOrCnst g = case definition g of
    TCon _ _ _ _ _ _ _ _ => do
      r <- resolvedNameId (fullname g)
      MkTyAndCnstrs t c <- get TAC
      -- TODO: Check if resolved named already in
      let t1 = insert r g t
      put TAC (MkTyAndCnstrs t1 c)
    DCon _ _ _ => do
      r <- resolvedNameId (fullname g)
      MkTyAndCnstrs t c <- get TAC
      let c1 = insert r g c
      put TAC (MkTyAndCnstrs t c1)
    _ => pure ()

  ||| Learn the TCons and DCons from the Defs context.
  ||| This is a helper to define datatypes
  learnTConsAndCons
    :  {auto _ : Ref Ctxt Defs}
    -> {auto _ : Ref TAC TyAndCnstrs}
    -> Core ()
  learnTConsAndCons = do
    context <- gamma <$> get Ctxt
    traverse_
      (\n => do
        mdef <- lookupCtxtExactI n context
        case mdef of
          Nothing => pure ()
          Just (_, def) => addTypeOrCnst def)
      !(allNames context)

  ||| Create an SDataCon for STyCon when creating the type definitions for STG.
  createSTGDataCon
    :  {auto _ : UniqueMapRef}
    -> {auto _ : Ref Counter Int}
    -> GlobalDef
    -> Core SDataCon
  createSTGDataCon g = do
    let fullName = fullname g
    (DCon _ arity _) <- pure $ definition g
      | other => coreFail $ InternalError $ "createSTGDataCon found other than DCon: " ++ show other
    let arity' : Int = (cast arity) - cast (length (eraseArgs g))
    if arity' < 0
      then coreFail $ InternalError $ unlines
            [ "Negative arity after erased arguments:"
            , show fullName
            , "Full arity: " ++ show arity
            , "Erased arity: " ++ show arity'
            , "Erasable arguments: " ++ show (eraseArgs g)
            ]
      else pure $ MkSDataCon
                    (show fullName)
                    !(mkDataConId fullName)
                    (AlgDataCon (replicate (fromInteger (cast arity')) LiftedRep))
                    !(mkSBinder TermBinder GlobalScope emptyFC ("mk" ++ show fullName))
                    (mkSrcSpan (location g))

  ||| Create an STG data definition from a TCon definition
  createSTGTyCon
    :  {auto _ : UniqueMapRef}
    -> {auto _ : Ref Counter Int}
    -> List SDataCon
    -> GlobalDef
    -> Core STyCon
  createSTGTyCon stgDataCons g = do
    let fullName = fullname g
    (TCon _ _ _ _ _ _ _ _) <- pure $ definition g
      | other => coreFail $ InternalError $ "createSTGTyCon found other than TCon: " ++ show other
    pure $ MkSTyCon
      (show fullName)
      (MkTyConId !(uniqueForType (show fullName))) -- TODO: Where else do we use STG type names?
      stgDataCons
      (mkSrcSpan (location g))

  ||| Compiles the learned TCons and their DCons
  defineDataTypes
    :  {auto _ : UniqueMapRef}
    -> {auto _ : Ref Counter Int}
    -> {auto _ : Ref Ctxt Defs}
    -> {auto _ : DataTypeMapRef}
    -> {auto _ : Ref ADTs ADTMap}
    -> {auto _ : Ref TAC TyAndCnstrs}
    -> Core ()
  defineDataTypes = do
    MkTyAndCnstrs types constructors <- get TAC
    -- TODO: Write check if all the constructors are used.
    traverse_
      (\(r, g) =>
        case definition g of
          -- TODO: Add datatype with constructors and remove the constructors from the cnstrs list
          def@(TCon _ parampos detpos _ _ _ datacons _) => do
            -- Create DataCons, looking up resolved IDs
            resolveds <- traverse resolvedNameId datacons
            datacons <- traverse
                (\rd => case lookup rd constructors of
                  Nothing => coreFail
                           $ InternalError
                           $ "defineDatatypes: Data constructor is not found: "
                              ++ show !(toFullNames (Resolved rd))
                  Just dg => createSTGDataCon dg)
              resolveds
            -- Create TyCon and attach DataCons
            sTyCon <- createSTGTyCon datacons g
            traverse (registerDataConToTyCon sTyCon . Resolved) resolveds
            defineDataType (MkUnitId MAIN_UNIT) (MkModuleName MAIN_MODULE) sTyCon
            pure ()
          _ => pure ())
      (toList types)

  ||| Create STG datatypes from filtering out the TCon and DCon definitions from Defs
  export
  createDataTypes
    :  {auto _ : UniqueMapRef}
    -> {auto _ : Ref Counter Int}
    -> {auto _ : Ref Ctxt Defs}
    -> {auto _ : DataTypeMapRef}
    -> {auto _ : Ref ADTs ADTMap}
    -> Core ()
  createDataTypes = do
    tac <- mkTACRef
    learnTConsAndCons
    defineDataTypes
