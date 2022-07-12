module Idris.Codegen.ExtSTG.ADTMap

import Libraries.Data.IntMap
import Libraries.Data.StringMap
import Data.List
import Data.String
import Core.Context
import Core.Core
import Idris.Codegen.ExtSTG.STG
import Idris.Codegen.ExtSTG.Core
import Idris.Codegen.ExtSTG.ExtName
import Idris.Codegen.ExtSTG.Context
import Idris.Codegen.ExtSTG.ADTAlias


||| Names of the data constructors must be associated with their STyCons, this information
||| is needed for the code generator when we generate STG case expressions
namespace DataConstructorsToTypeDefinitions

  ||| Gets the Int identifies through its Resolved name.
  export
  resolvedNameId : Ref Ctxt Defs => String -> Core.Name.Name -> Core Int
  resolvedNameId ctx n = do
    (Resolved r) <- toResolvedNames n
      | _ => coreFail $ InternalError $ show n ++ " doesn't have resolved id when " ++ ctx
    pure r

  resolvedNameIdOpt : Ref Ctxt Defs => Core.Name.Name -> Core (Either Int String)
  resolvedNameIdOpt n =
    pure $ case !(toResolvedNames n) of
            (Resolved r) => Left r
            _            => Right $ show n

  ||| Register the consturctor name for the STyCon
  export
  registerDataConToTyCon
    :  Ref STGCtxt STGContext
    => Ref Ctxt Defs
    => STyCon
    -> Core.Name.Name
    -> Core ()
  registerDataConToTyCon s n = do
    r <- resolvedNameId "registering type constructor" n
    Nothing <- lookupADTResolved r
      | Just st =>
          coreFail $ InternalError
                   $ show !(toFullNames n) ++ " is already registered for " ++ STyCon.Name st
    let k : String := show n
    Nothing <- lookupADTNamed k
      | Just st =>
          coreFail $ InternalError
                   $ show !(toFullNames n) ++ " is already registered for " ++ STyCon.Name st
    insertADTResolved r s

  export
  registerInternalDataConToTyCon : Ref STGCtxt STGContext => STyCon -> Core.Name.Name -> Core ()
  registerInternalDataConToTyCon s n = do
    let k : String := show n
    Nothing <- lookupADTNamed k
      | Just st =>
          coreFail $ InternalError
                   $ show n ++ " is already registered for " ++ STyCon.Name st
    -- modifySTGCtxt (record { adtNamed $= insert k s })
    insertADTNamed k s

  ||| Lookup if there is an ADT defined for the given name either for type name or data con name.
  export
  lookupTyCon
    :  Ref Ctxt Defs
    => Ref STGCtxt STGContext
    => Core.Name.Name
    -> Core (Maybe STyCon)
  lookupTyCon n =
    case !(resolvedNameIdOpt n) of
      Left i  => lookupADTResolved i
      Right m => lookupADTNamed m

export
insertTypeDataCon : Ref STGCtxt STGContext => Core.Name.Name -> (d : DataConRep) -> Core SDataConSg
insertTypeDataCon n d = do
  u <- uniqueForIdrisTypeDataCon n
  let name = binderStr n
  binder <- mkFreshSBinderStr GlobalScope EmptyFC name
  let sdatacon : SDataCon d := MkSDataCon name (MkDataConId u) binder (SsUnhelpfulSpan name)
  ctx <- get STGCtxt
  put STGCtxt ({typeDataConDefs $= insert (show u) (_ ** sdatacon)} ctx)
  pure (_ ** sdatacon)

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
    :  Ref TAC TyAndCnstrs
    => Ref Ctxt Defs
    => Ref STGCtxt STGContext
    => GlobalDef
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
    :  Ref Ctxt Defs
    => Ref TAC TyAndCnstrs
    => Ref STGCtxt STGContext
    => Core ()
  learnTConsAndCons = do
    context <- gamma <$> get Ctxt
    traverse_
      (\n => do
        mdef <- lookupCtxtExactI n context
        case mdef of
          Nothing => pure ()
          Just (_, def) => addTypeOrCnst def)
      !(allNames context)

  createSTGDataConDesc : GlobalDef -> Core (Core.Name.Name, DataConRep, SrcSpan)
  createSTGDataConDesc g = do
    let fullName = fullname g
    let (DCon _ arity _) = definition g
          | other => coreFail $ InternalError $ "createSTGDataCon found other than DCon: \{show other}"
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
            ( fullName
            , (AlgDataCon (replicate (fromInteger (cast arity')) LiftedRep))
            , mkSrcSpan (location g)
            )

  createSTGExtDataConDesc : GlobalDef -> Core (ExtName, DataConRep, SrcSpan)
  createSTGExtDataConDesc g = do
    let fullName = fullname g
    let (DCon _ originalArity _) = definition g
          | other => coreFail $ InternalError $ "createSTGDataCon found other than DCon: \{show other}"
    let arityAfterErasedArgs : Int = (cast originalArity) - cast (length (eraseArgs g))
    let False = arityAfterErasedArgs < 0
        | True => coreFail $ InternalError $ unlines
                    [ "Negative arity after erased arguments:"
                    , show fullName
                    , "Full arity: " ++ show originalArity
                    , "Erased arity: " ++ show arityAfterErasedArgs
                    , "Erasable arguments: " ++ show (eraseArgs g)
                    ]
    let Just (stgExtName, stgArity) = constructorExtName fullName
        | Nothing => coreFail $ InternalError "Data constructor should be defined as ExtName: \{show fullName}"
    let True = cast stgArity == arityAfterErasedArgs
        | False => coreFail $ InternalError "Different STG arity than expected. STG arity: \{show stgArity}, Idris arity \{show arityAfterErasedArgs}"
    pure
      ( stgExtName
      , (AlgDataCon (replicate stgArity LiftedRep))
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
    :  Ref Ctxt Defs
    => Ref TAC TyAndCnstrs
    => Ref STGCtxt STGContext
    => Core ()
  defineDataTypes = do
    MkTyAndCnstrs types constructors <- get TAC
    -- TODO: Write check if all the constructors are used.
    traverse_
      (\(r, g) =>
        case definition g of
          -- TODO: Add datatype with constructors and remove the constructors from the cnstrs list
          def@(TCon tag arity parampos detpos flags mutwith datacons detaggable) => do
            -- Create DataCons, looking up resolved IDs
            resolveds <- traverse (resolvedNameId "when defining data types") datacons
            case typeExtName (fullname g) of
              Just (stgExtName, stgArity) => do
                sTyCon <- createSTyConExt (stgExtName, SsUnhelpfulSpan "")
                            !(traverse (\rd => case lookup rd constructors of
                                          Nothing => coreFail $ InternalError
                                                      "defineDatatypes: Data constructor is not found: \{show !(toFullNames (Resolved rd))}"
                                          Just dg => createSTGExtDataConDesc dg)
                                       resolveds)
                traverse_ (registerDataConToTyCon sTyCon . Resolved) resolveds
                defineDataType (mkUnitId stgExtName) (mkModuleName stgExtName) sTyCon
              Nothing => do
                sTyCon <- createSTyCon (fullname g, mkSrcSpan (location g))
                            !(traverse (\rd => case lookup rd constructors of
                                          Nothing => coreFail
                                                  $ InternalError
                                                  $ "defineDatatypes: Data constructor is not found: "
                                                      ++ show !(toFullNames (Resolved rd))
                                          Just dg => createSTGDataConDesc dg)
                                      resolveds)
                traverse_ (registerDataConToTyCon sTyCon . Resolved) resolveds
                defineDataType (MkUnitId MAIN_UNIT) (MkModuleName MAIN_MODULE) sTyCon
          _ => pure ())
      (toList types)
    -- Compute and register type of types
    tyAlgDataCons
      <- map catMaybes $ traverse
          (\(r,g) =>
            case definition g of
              def@(TCon tag arity parampos detpos flags mutwith datacons detaggable) => do
                let fullName = fullname g
                let arity' : Int = (cast arity) - cast (length (eraseArgs g))
                let True = arity' >= 0
                   | False => coreFail $ InternalError "Wrong arity in type matching: \{show arity} \{show arity'}"
                let dataconrep = AlgDataCon (replicate (fromInteger (cast arity')) LiftedRep)
                sdatacon <- insertTypeDataCon fullName dataconrep
                pure $ Just sdatacon
              _ => pure Nothing)
          (toList types)
    u <- mkUnique 't'
    let stycon := MkSTyCon "Idris.Types" (MkTyConId u) tyAlgDataCons (SsUnhelpfulSpan "Idris.Types")
    defineDataType (MkUnitId MAIN_UNIT) (MkModuleName MAIN_MODULE) stycon
    ctx <- get STGCtxt
    put STGCtxt ({typeTyCon := Just stycon} ctx)

  ||| Create STG datatypes from filtering out the TCon and DCon definitions from Defs
  export
  createDataTypes
    :  Ref Ctxt Defs
    => Ref STGCtxt STGContext
    => Core ()
  createDataTypes = do
    tac <- mkTACRef
    learnTConsAndCons
    defineDataTypes
