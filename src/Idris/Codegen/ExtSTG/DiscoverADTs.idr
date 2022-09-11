module Idris.Codegen.ExtSTG.DiscoverADTs

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

{-
Discover the data and type constructors of the compiled program and register them in the ADTMap.

- Enumerate all the definitions from the context
- Act only on the DCon and TCon
- First register all the DCon: Create unique, DataCon info, and register it with ADTInfo
- Second register all the TCon: Create unique, DataCon info, and register it with ADTInfo

-}

stgName : ExtName -> STG.Name
stgName (MkExtName _ _ n) = n

discoverDCon : Ref Ctxt Defs => Ref STGCtxt STGContext => Core ()
discoverDCon = do
  ctx <- gamma <$> get Ctxt
  traverse_
    (\n => do
        mdef <- lookupCtxtExactI n ctx
        case mdef of
          Nothing => pure ()
          Just (_, def) => case definition def of
            DCon tag arity nta => do
              fn <- toFullNames n
              let realArity : Int = (cast arity) - cast (length (eraseArgs def))
              let False = realArity < 0
                  | True => coreFail $ InternalError
                      """
                      Negative arity after erased arguments:
                      \{show fn}
                      Full arity: \{show arity}
                      Erased arity: \{show realArity}
                      Erasable arguments: \{show (eraseArgs def)}
                      """
              let span = mkSrcSpan (location def)
              case constructorExtName fn of
                Nothing => do
                  u <- mkUnique 'd'
                  insertIdrisTermNamespace fn u
                  let algDataCon = AlgDataCon (replicate (fromInteger (cast realArity)) LiftedRep)
                  let dataConName = binderStr fn
                  binder <- mkSBinder span Trm (IdrName fn)
                  let sdatacon
                        : SDataCon algDataCon
                        := MkSDataCon dataConName (MkDataConId u) binder span
                  insertSTGDataCon (algDataCon ** sdatacon)
                Just (stgExtName, stgArity) => do
                  let True = cast stgArity == realArity
                      | False => coreFail $ InternalError "\{show fn} has arity of \{show realArity} but its Haskell alias is given of arity \{show stgArity}"
                  u <- mkUnique 'j'
                  insertHaskellTermNamespace stgExtName u
                  let algDataCon = AlgDataCon (replicate stgArity LiftedRep)
                  let dataConName = stgName stgExtName
                  binder <- mkSBinder span Trm (HsName stgExtName)
                  let sdatacon
                        : SDataCon algDataCon
                        := MkSDataCon dataConName (MkDataConId u) binder span
                  insertSTGDataCon (algDataCon ** sdatacon)
            other => pure ())
    !(allNames ctx)

discoverTCon : Ref Ctxt Defs => Ref STGCtxt STGContext => Core ()
discoverTCon = do
  ctx <- gamma <$> get Ctxt
  traverse_
    (\n => do
        mdef <- lookupCtxtExactI n ctx
        case mdef of
          Nothing => pure ()
          Just (_, def) => case definition def of
            TCon tag arity parampos detpos flags mutwith datacons detaggable => do
              fn <- toFullNames n
              ut <- mkUnique 't'
              ud <- mkUnique 'i'
              let span = mkSrcSpan (location def)
              case typeExtName fn of
                Nothing => do
                  insertIdrisTypeDataCon fn ut ud
                  sdatacons
                    <- traverse
                        (\dn => do
                          dfn <- toFullNames dn
                          registerIdrisDCtoTC dfn fn
                          getIdrisDataCon dfn)
                        datacons
                  let tyconName = binderStr fn
                  let sTyCon
                        : STyCon
                        := MkSTyCon tyconName (MkTyConId ut) sdatacons span
                  defineDataType (MkUnitId MAIN_UNIT) (MkModuleName MAIN_MODULE) sTyCon
                Just (stgExtName, stgArity) => do
                  insertHaskellTypeNamespace stgExtName ut
                  sdatacons
                    <- traverse
                        (\dn => do
                          dfn <- toFullNames dn
                          let Just (cExtName, cSTGArity) = constructorExtName dfn
                              | Nothing => coreFail $ InternalError "TODO #1"
                          registerHaskellDCtoTC cExtName stgExtName
                          getHaskellDataCon cExtName)
                        datacons
                  let tyconName = stgName stgExtName
                  let sTyCon
                        : STyCon
                        := MkSTyCon tyconName (MkTyConId ut) sdatacons span
                  defineDataType (mkUnitId stgExtName) (mkModuleName stgExtName) sTyCon
            other => pure ())
    !(allNames ctx)

export
discoverADTs : Ref Ctxt Defs => Ref STGCtxt STGContext => Core ()
discoverADTs = do
  discoverDCon
  discoverTCon
