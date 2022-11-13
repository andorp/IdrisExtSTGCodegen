module Idris.Codegen.ExtSTG.DiscoverADTs

import Core.Context
import Core.Core
import Data.List
import Data.Maybe
import Data.String
import Libraries.Data.IntMap
import Libraries.Data.StringMap

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

-- stgName : ExtName -> STG.Name
-- stgName (MkExtName _ _ n) = n

-- discoverDCon : Ref Ctxt Defs => Ref STGCtxt STGContext => Core ()
-- discoverDCon = do
--   ctx <- gamma <$> get Ctxt
--   traverse_
--     (\n => do
--         mdef <- lookupCtxtExactI n ctx
--         case mdef of
--           Nothing => pure ()
--           Just (_, def) => case definition def of
--             DCon tag arity nta => do
--               fn <- toFullNames n
--               let realArity : Int = (cast arity) - cast (length (eraseArgs def))
--               let False = realArity < 0
--                   | True => coreFail $ InternalError
--                       """
--                       Negative arity after erased arguments:
--                       \{show fn}
--                       Full arity: \{show arity}
--                       Erased arity: \{show realArity}
--                       Erasable arguments: \{show (eraseArgs def)}
--                       """
--               let span = mkSrcSpan (location def)
--               case !(constructorExtName fn) of
--                 IdrisName _ => do
--                   u <- mkUnique 'd'
--                   insertIdrisTermNamespace fn u
--                   let algDataCon = AlgDataCon (replicate (fromInteger (cast realArity)) LiftedRep)
--                   let dataConName = binderStr fn
--                   binder <- mkSBinder span Trm (IdrName fn)
--                   let sdatacon
--                         : SDataCon algDataCon
--                         := MkSDataCon dataConName (MkDataConId u) binder span
--                   insertSTGDataCon (algDataCon ** sdatacon)
--                 AliasedName _ stgExtName stgArity => do
--                   let True = cast stgArity == realArity
--                       | False => coreFail $ InternalError "\{show fn} has arity of \{show realArity} but its Haskell alias is given of arity \{show stgArity}"
--                   u <- mkUnique 'j'
--                   -- insertHaskellTermNamespace stgExtName u
--                   registerDataConAlias fn stgExtName u
--                   let algDataCon = AlgDataCon (replicate stgArity LiftedRep)
--                   let dataConName = stgName stgExtName
--                   binder <- mkSBinder span Trm (HsName stgExtName)
--                   let sdatacon
--                         : SDataCon algDataCon
--                         := MkSDataCon dataConName (MkDataConId u) binder span
--                   insertSTGDataCon (algDataCon ** sdatacon)
--             other => pure ())
--     !(allNames ctx)

-- coreNameOf : PrimType -> Name.Name
-- coreNameOf IntType      = UN (Basic "Int")
-- coreNameOf Int8Type     = UN (Basic "Int8")
-- coreNameOf Int16Type    = UN (Basic "Int16")
-- coreNameOf Int32Type    = UN (Basic "Int32")
-- coreNameOf Int64Type    = UN (Basic "Int64")
-- coreNameOf IntegerType  = UN (Basic "Integer")
-- coreNameOf Bits8Type    = UN (Basic "Bits8")
-- coreNameOf Bits16Type   = UN (Basic "Bits16")
-- coreNameOf Bits32Type   = UN (Basic "Bits32")
-- coreNameOf Bits64Type   = UN (Basic "Bits64")
-- coreNameOf StringType   = UN (Basic "String")
-- coreNameOf CharType     = UN (Basic "Char")
-- coreNameOf DoubleType   = UN (Basic "Double")
-- coreNameOf WorldType    = UN (Basic "%World")

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

-- discoverTCon : Ref Ctxt Defs => Ref STGCtxt STGContext => Core ()
-- discoverTCon = do
--   ctx <- gamma <$> get Ctxt
--   dataConOfTypeOfType <- traverse
--     (\n => do
--         mdef <- lookupCtxtExactI n ctx
--         case mdef of
--           Nothing => pure Nothing
--           Just (_, def) => case definition def of
--             TCon tag arity parampos detpos flags mutwith datacons detaggable => do
--               fn <- toFullNames n
--               ut <- mkUnique 't'
--               ud <- mkUnique 'i'
--               let span = mkSrcSpan (location def)
--               let realArity : Int = (cast arity) - cast (length (eraseArgs def))
--               case !(typeExtName fn) of
--                 Nothing => do
--                   insertIdrisTypeNamespace fn ut ud
--                   -- Create DataCon for TypeOfTypes
--                   sdatacons
--                     <- traverse
--                         (\dn => do
--                           dfn <- toFullNames dn
--                           registerIdrisDCtoTC dfn fn
--                           getIdrisDataCon dfn)
--                         datacons
--                   let tyconName = binderStr fn
--                   let sTyCon
--                         : STyCon
--                         := MkSTyCon tyconName (MkTyConId ut) sdatacons span
--                   defineDataType (MkUnitId MAIN_UNIT) (MkModuleName MAIN_MODULE) sTyCon
--                   pure (Just (span,fn,realArity,ud))
--                 Just stgExtName => do
--                   -- TODO: Figure out how to create TypeOfTypes constructor
--                   -- insertHaskellTypeNamespace stgExtName ut
--                   registerTypeConAlias fn stgExtName ut ud
--                   sdatacons
--                     <- traverse
--                         (\dn => do
--                           dfn <- toFullNames dn
--                           (AliasedName _ cExtName cSTGArity) <- constructorExtName dfn
--                             | IdrisName _ => coreFail $ InternalError "No aliased constructor is found for \{show dfn}"
--                           registerHaskellDCtoTC cExtName stgExtName
--                           getHaskellDataCon cExtName)
--                         datacons
--                   let tyconName = stgName stgExtName
--                   let sTyCon
--                         : STyCon
--                         := MkSTyCon tyconName (MkTyConId ut) sdatacons span
--                   defineDataType (mkUnitId stgExtName) (mkModuleName stgExtName) sTyCon
--                   pure (Just (span,fn,realArity,ud))
--             other => pure Nothing)
--     !(allNames ctx)

--   primTypeDataCons <- map (the (List SDataConSg)) $ traverse
--     (\pt => do
--       (tyExt, constExt, params) <- runtimeRepresentationOf pt
--       let algDataCon = AlgDataCon []
--       let span = SsUnhelpfulSpan "TODO"
--       let name = coreNameOf pt
--       let dataConName = binderStr name
--       ud <- mkUnique 'i'
--       binder <- mkSBinder span Trm (IdrName (UN (Basic dataConName)))
--       let sdatacon
--             : SDataCon algDataCon
--             := MkSDataCon dataConName (MkDataConId ud) binder span
--       Just ut <- lookupHaskellTypeNamespace tyExt
--         | Nothing => coreFail $ InternalError "Primitive type is not registered as Haskell type: \{show pt}"
--       insertPrimType pt name tyExt (ut, ud)
--       insertSTGDataCon (algDataCon ** sdatacon)
--       pure (algDataCon ** sdatacon))
--     [ CharType
--     , IntType
--     , IntegerType
--     , Int8Type
--     , Int16Type
--     , Int32Type
--     , Int64Type
--     , Bits8Type
--     , Bits16Type
--     , Bits32Type
--     , Bits64Type
--     , DoubleType
--     , WorldType
--     ]
--   dataConsTypeOfType <- map (the (List SDataConSg)) $ traverse
--     (\(span,tn,arity,unique) => do
--       let algDataCon = AlgDataCon (replicate (fromInteger (cast arity)) LiftedRep)
--       let dataConName = binderStr tn
--       binder <- mkSBinder span Trm (IdrName tn)
--       let sdatacon
--             : SDataCon algDataCon
--             := MkSDataCon dataConName (MkDataConId unique) binder span
--       pure (algDataCon ** sdatacon))
--     (catMaybes dataConOfTypeOfType)
--   stgCtx <- get STGCtxt
--   let sTyCon
--         : STyCon
--         := MkSTyCon
--             "TypeOfTypes"
--             (MkTyConId stgCtx.idrisTypeOfTypes)
--             (primTypeDataCons ++ dataConsTypeOfType)
--             (SsUnhelpfulSpan "TypeOfTypes")
--   defineDataType (MkUnitId MAIN_UNIT) (MkModuleName MAIN_MODULE) sTyCon

-- discoverTypeOfType : Ref Ctxt Defs => Ref STGCtxt STGContext => Core ()
-- discoverTypeOfType = 

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
  -- discoverDCon
  -- discoverTCon
  discoverPrimTypes
  discoverTypes
  unitRet createTypeOfTypes

