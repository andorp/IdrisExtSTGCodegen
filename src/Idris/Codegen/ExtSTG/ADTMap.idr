module Idris.Codegen.ExtSTG.ADTMap

import Data.IntMap
import Data.StringMap
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

  ||| Associates the resolved names or internal names with the TyCon.
  export
  ADTMap : Type
  ADTMap = (IntMap STyCon, StringMap STyCon)

  ||| Create the ADTs reference
  export
  mkADTs : Core (Ref ADTs ADTMap)
  mkADTs = newRef ADTs (empty, empty)

  ||| Gets the Int identifies through its Resolved name.
  export
  resolvedNameId
    :  {auto _ : Ref Ctxt Defs}
    -> String
    -> Core.Name.Name
    -> Core Int
  resolvedNameId ctx n = do
    (Resolved r) <- toResolvedNames n
      | _ => coreFail $ InternalError $ show n ++ " doesn't have resolved id when " ++ ctx
    pure r

  resolvedNameIdOpt
    :  {auto _ : Ref Ctxt Defs}
    -> Core.Name.Name
    -> Core (Either Int String)
  resolvedNameIdOpt n =
    pure $ case !(toResolvedNames n) of
            (Resolved r) => Left r
            _            => Right $ show n

  ||| Register the consturctor name for the STyCon
  export
  registerDataConToTyCon
    :  {auto _ : Ref ADTs ADTMap}
    -> {auto _ : Ref Ctxt Defs}
    -> STyCon
    -> Core.Name.Name
    -> Core ()
  registerDataConToTyCon s n = do
    r <- resolvedNameId "registering type constructor" n
    (m,i) <- get ADTs
    let k = show n
    case (lookup r m <+> lookup k i) of
      Just st
        => coreFail $ InternalError
                    $ show !(toFullNames n) ++ " is already registered for " ++ STyCon.Name st
      Nothing => do
        put ADTs (insert r s m, i)

  export
  registerInternalDataConToTyCon
    : Ref ADTs ADTMap
    => STyCon -> Core.Name.Name
    -> Core ()
  registerInternalDataConToTyCon s n = do
    (m,i) <- get ADTs
    let k = show n
    case lookup k i of
      Just st => coreFail
               $ InternalError
               $ show n ++ " is already registered for " ++ STyCon.Name st
      Nothing => do
        put ADTs (m, insert k s i)

  ||| Lookup if there is an ADT defined for the given name either for type name or data con name.
  export
  lookupTyCon
    :  {auto _ : Ref ADTs ADTMap}
    -> {auto _ : Ref Ctxt Defs}
    -> Core.Name.Name
    -> Core (Maybe STyCon)
  lookupTyCon n = do
    (resMap, internalMap) <- get ADTs
    pure $ case !(resolvedNameIdOpt n) of
      Left i  => lookup i resMap
      Right m => lookup m internalMap

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
      r <- resolvedNameId "looking up TCon in definition" (fullname g)
      MkTyAndCnstrs t c <- get TAC
      -- TODO: Check if resolved named already in
      let t1 = insert r g t
      put TAC (MkTyAndCnstrs t1 c)
    DCon _ _ _ => do
      r <- resolvedNameId "looking up DCon in definition" (fullname g)
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

  createSTGDataConDesc : GlobalDef -> Core (STG.Name, DataConRep, SrcSpan)
  createSTGDataConDesc g = do
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
      else pure
            ( show fullName -- -- TODO: Idris.Name -> STG.Name
            , (AlgDataCon (replicate (fromInteger (cast arity')) LiftedRep))
            , mkSrcSpan (location g)
            )

  ||| Create an STG data definition from a TCon definition
  createSTGTyConDesc : GlobalDef -> Core (STG.Name, SrcSpan)
  createSTGTyConDesc g = do
    let fullName = fullname g
    (TCon _ _ _ _ _ _ _ _) <- pure $ definition g
      | other => coreFail $ InternalError $ "createSTGTyCon found other than TCon: " ++ show other
    pure (show fullName, (mkSrcSpan (location g)))

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
            resolveds <- traverse (resolvedNameId "when defining data types") datacons
            sTyCon <- createSTyCon (show (fullname g), mkSrcSpan (location g)) -- TODO, IdrisName -> STG.Name
                        !(traverse (\rd => case lookup rd constructors of
                                      Nothing => coreFail
                                               $ InternalError
                                               $ "defineDatatypes: Data constructor is not found: "
                                                  ++ show !(toFullNames (Resolved rd))
                                      Just dg => createSTGDataConDesc dg)
                                   resolveds)
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
