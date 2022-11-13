module Idris.Codegen.ExtSTG.Context

import Compiler.ANF -- (AVar)
import Core.Context
import Core.Context.Context
import Core.Core
import Core.Options 
import Data.List1
import Data.SortedMap
import Data.String -- (isPreffixOf)
import Data.String.Extra -- (drop)
import Libraries.Data.IntMap
import Libraries.Data.StringMap

import Idris.Codegen.ExtSTG.ADTAlias
import Idris.Codegen.ExtSTG.ADTs
import Idris.Codegen.ExtSTG.Configuration
import Idris.Codegen.ExtSTG.ExtName
import Idris.Codegen.ExtSTG.ForeignFile
import Idris.Codegen.ExtSTG.STG

import public Idris.Codegen.ExtSTG.Binders

%default total

export
binderStr : Core.Name.Name -> String
binderStr (NS ns n@(UN (Field _))) = show ns ++ ".(" ++ binderStr n ++ ")"
binderStr (NS ns n) = show ns ++ "." ++ binderStr n
binderStr (UN x) = show x
binderStr (MN x y) = "{" ++ x ++ ":" ++ show y ++ "}"
binderStr (PV n d) = "{P:" ++ binderStr n ++ ":" ++ show d ++ "}"
binderStr (DN str n) = str ++ "*" ++ binderStr n
binderStr (Nested (outer, idx) inner) = show outer ++ ":" ++ show idx ++ ":" ++ binderStr inner
binderStr (CaseBlock outer i) = "case:block:in:" ++ outer ++ "*" ++ show i
binderStr (WithBlock outer i) = "with:block:in:" ++ outer ++ "*" ++ show i
binderStr (Resolved x) = "$resolved" ++ show x


-- public export
-- DataTypeMap : Type
-- DataTypeMap = StringMap {-UnitId-} (StringMap {-ModuleName-} (List STyCon))

public export
StringTableMap : Type
StringTableMap = StringMap TopBinding

export
data STGCtxt : Type where

public export
record STGContext where
  constructor MkSTGContext
  configuration         : Configuration
  counter               : Int
  -- dataTypes             : DataTypeMap
  stringTable           : StringTableMap
--  adts                  : ADTs
  adts2                 : ADTs2
  binders               : Binders
  -- mainUnique            : Unique
  -- mainArgUnique         : Unique
  -- idrisTypeOfTypes      : Unique
  -- idrisTypesSTyCon      : Maybe STyCon
  -- extNameBinders        : SortedMap ExtName SBinderSg
  ffiFiles              : FFIFiles
  adtAliasFiles         : ADTAliasFiles

export
logLine : Ref STGCtxt STGContext => Configuration.LogLevel -> Lazy String -> Core ()
logLine levelOfMsg msg = do
  logLvl <- map (\c => c.configuration.logLevel) $ get STGCtxt
  when (logLvl <= levelOfMsg) $ coreLift $ putStrLn msg

export
modifySTGCtxt : (Ref STGCtxt STGContext) => (STGContext -> STGContext) -> Core ()
modifySTGCtxt f = do
  logLine Debug "Context modification"
  ctx <- get STGCtxt
  put STGCtxt (f ctx)

-- lookupFFIBinder : STGContext -> ExtName -> Maybe SBinderSg
-- lookupFFIBinder ctx e = lookup e ctx.extNameBinders

-- insertFFIBinder : STGContext -> ExtName -> SBinderSg -> Either String STGContext
-- insertFFIBinder ctx e b = do
--   let u = getBinderIdUnique (binderId (snd b))
--   Right $ { extNameBinders $= insert e b } ctx

-- extNameBinderList : STGContext -> List (ExtName, SBinderSg)
-- extNameBinderList ctx = toList ctx.extNameBinders

-- lookupIdrisTypesSTyCon : STGContext -> Maybe STyCon
-- lookupIdrisTypesSTyCon = idrisTypesSTyCon

-- export
-- getMainUnique : Ref STGCtxt STGContext => Core Unique
-- getMainUnique = do
--   ctx <- get STGCtxt
--   pure ctx.mainUnique

-- export
-- getMainArgUnique : Ref STGCtxt STGContext => Core Unique
-- getMainArgUnique = do
--   ctx <- get STGCtxt
--   pure ctx.mainArgUnique

-- export
incCounter : Ref STGCtxt STGContext => Core Int
incCounter = do
  ctx <- get STGCtxt
  put STGCtxt ({counter $= (+1)} ctx)
  pure ctx.counter

export
mkUnique
  :  (Ref STGCtxt STGContext)
  => Char
  -> Core Unique
mkUnique c = do
  x <- incCounter
  let u = MkUnique c x
  logLine Debug $ "mkUnique: \{show u}"
  pure u

export
covering
nameToPath : Ref Ctxt Defs => Core.Name.Name -> Core (Maybe (List String, String))
nameToPath n = do
  (NS ns (UN n)) <- toFullNames n
    | other => pure Nothing
  let mdl : List String = toList $ split (=='.') $ show ns
  pure (Just (mdl, (displayUserName n)))

public export
data ResolvedName
  = IdrisName   Core.Name.Name
  | AliasedName Core.Name.Name ExtName Arity

Show ResolvedName where
  showPrec p = \case
    IdrisName n       => showCon p "IdrisName" (showArg n)
    AliasedName n e a => showCon p "AliasedName" $ showArg n ++ showArg e ++ showArg a

export
covering
typeExtName
  :  Ref Ctxt Defs
  => Ref STGCtxt STGContext
  => Core.Name.Name -> Core (Maybe ExtName)
typeExtName n = do
  logLine Debug "typeExtName: \{show n}"
  ctx <- get STGCtxt
  let dir = ctx.configuration.foreignDirectory
  Just (mdl,nm) <- nameToPath n
    | Nothing => pure Nothing
  (result, aaf) <- typeName ctx.adtAliasFiles dir mdl nm
  modifySTGCtxt ({ adtAliasFiles := aaf})
  pure result

export
covering
constructorExtName
  :  Ref Ctxt Defs
  => Ref STGCtxt STGContext
  => Core.Name.Name -> Core ResolvedName
constructorExtName n = do
  logLine Debug "constructorExtName: \{show n}"
  ctx <- get STGCtxt
  let dir = ctx.configuration.foreignDirectory
  Just (mdl,nm) <- nameToPath n
    | Nothing => pure $ IdrisName n
  (result, aaf) <- constructorName ctx.adtAliasFiles dir mdl nm
  modifySTGCtxt ({ adtAliasFiles := aaf})
  pure $ case result of
    Nothing    => IdrisName n
    Just (e,a) => AliasedName n e a

-- export
-- insertSTGDataCon : Ref STGCtxt STGContext => SDataConSg -> Core ()
-- insertSTGDataCon s = do
--   ctx <- get STGCtxt
--   let Right adts' = registerSDataCon ctx.adts s
--       | Left err => coreFail $ InternalError "insertSTGDataCon: \{err}"
--   put STGCtxt ({ adts := adts' } ctx)

-- export
-- lookupIdrisTypeDataCon : Ref STGCtxt STGContext => Core.Name.Name -> Core (Maybe Unique)
-- lookupIdrisTypeDataCon n = do
--   ctx <- get STGCtxt
--   case lookupIdrisTyName ctx.adts n of
--     Right (Just (IdrisTyCon _ (MkIdrisTyUnique _ u))) => pure $ Just u
--     Right (Just (IdrisTyCon _ (MkPrimTyUnique _ u))) => pure $ Just u
--     Right (Just (IdrisBuiltInType _ _ (_, u))) => pure $ Just u -- TODO: We would need IdrisBuiltInType
--     Right (Just (ADTAliasType _ _ (_, u))) => pure $ Just u
--     Right Nothing => pure Nothing
--     Right other => coreFail $ InternalError "lookupIdrisTypeDataCon: found other constructor than IdrisTyCon. \{show other}"
--     Left err => coreFail $ InternalError "lookupIdrisTypeDataCon: \{err}"

-- ||| Looks up the associated tycon with the given name of a data constructor.
-- export
-- lookupIdrisAssociatedTyCon
--   : Ref STGCtxt STGContext => Core.Name.Name -> Core STyCon
-- lookupIdrisAssociatedTyCon n = do
--   ctx <- get STGCtxt
--   let Right (Just adtinfo) = lookupIdrisDtName ctx.adts n
--       | Right Nothing => coreFail $ InternalError "lookupIdrisAssociatedTyCon: No ADTInfo is found for \{show n}"
--       | Left err => coreFail $ InternalError "lookupIdrisAssociatedTyCon: \{err}"
--   let Right ud = idrisUnique adtinfo
--       | Left err => coreFail $ InternalError "lookupIdrisAssociatedTyCon: \{err}"
--   let Right ut = lookupDataConUniqueToTyCon ctx.adts ud
--       | Left err => coreFail $ InternalError "lookupIdrisAssociatedTyCon: \{err}"
--   let Just stycon = lookupSTGTyCon ctx.adts ut
--       | Nothing => coreFail $ InternalError "lookupIdrisAssociatedTyCon: No associated STyCon is found for \{show n}"
--   pure stycon      

-- ||| Looks up the associated tycon with the given name of a data constructor.
-- export
-- lookupHaskellAssociatedTyCon
--   : Ref STGCtxt STGContext => ExtName -> Core STyCon
-- lookupHaskellAssociatedTyCon e = do
--   ctx <- get STGCtxt
--   let Right (Just adtinfo) = lookupHaskellDtName ctx.adts e
--       | Right Nothing => coreFail $ InternalError "lookupHaskellAssociatedTyCon: No ADTInfo is found for \{show e}"
--       | Left err => coreFail $ InternalError "lookupHaskellAssociatedTyCon: \{err}"
--   let Right ud = haskellUnique adtinfo
--       | Left err => coreFail $ InternalError "lookupHaskellAssociatedTyCon: \{err}"
--   let Right ut = lookupDataConUniqueToTyCon ctx.adts ud
--       | Left err => coreFail $ InternalError "lookupHaskellAssociatedTyCon: \{err}"
--   let Just stycon = lookupSTGTyCon ctx.adts ut
--       | Nothing => coreFail $ InternalError "lookupHaskellAssociatedTyCon: No associated STyCon is found for \{show e}"
--   pure stycon      

-- export
-- lookupAssociatedTyCon
--   :  Ref Ctxt Defs
--   => Ref STGCtxt STGContext
--   => Core.Name.Name -> Core STyCon
-- lookupAssociatedTyCon n =
--   case !(constructorExtName n) of
--     Nothing     => lookupIdrisAssociatedTyCon n
--     Just (e, _) => lookupHaskellAssociatedTyCon e

-- export
-- lookupIdrisTypeDataConDef : Ref STGCtxt STGContext => Unique -> Core SDataConSg
-- lookupIdrisTypeDataConDef u = do
--   ctx <- get STGCtxt
--   let Just d = lookupIdrisSTGDataCon ctx.adts u
--       | Nothing => coreFail $ InternalError "lookupIdrisTypeDataConDef: No STG datacon for \{show u}"
--   pure d

-- export
-- insertIdrisTypeNamespace : Ref STGCtxt STGContext => Core.Name.Name -> Unique -> Unique -> Core ()
-- insertIdrisTypeNamespace n ut ud = do
--   ctx <- get STGCtxt
--   let Right adts' = insertIdrisTyName ctx.adts n (MkIdrisTyUnique ut ud)
--       | Left err => coreFail $ InternalError "insertIdrisTypeNamespace \{err}"
--   put STGCtxt ({adts := adts'} ctx)

-- -- TODO: Unify with lookupIdrisTypeDataCon, returning IdrisTyUnique
-- export
-- lookupIdrisTypeNamespace : Ref STGCtxt STGContext => Core.Name.Name -> Core (Maybe IdrisTyUnique)
-- lookupIdrisTypeNamespace n = do
--   ctx <- get STGCtxt
--   case lookupIdrisTyName ctx.adts n of
--     Right (Just (IdrisTyCon _ u)) => pure $ Just u
--     Right Nothing => pure Nothing
--     Right other => coreFail $ InternalError "lookupIdrisTypeNamespace: found other constructor than IdrisTyCon. \{show other}"
--     Left err => coreFail $ InternalError "lookupIdrisTypeNamespace: \{err}"


-- export
-- lookupHaskellTypeNamespace : Ref STGCtxt STGContext => ExtName -> Core (Maybe Unique)
-- lookupHaskellTypeNamespace e = do
--   ctx <- get STGCtxt
--   case lookupHaskellTyName ctx.adts e of
--     Right (Just (HaskellTyCon _ u)) => pure $ Just u
--     Right (Just (IdrisBuiltInType _ _ (u,_))) => pure $ Just u
--     Right Nothing => pure Nothing
--     Right other => coreFail $ InternalError "lookupHaskellTypeNamespace: found other constructor than HaskellTyCon. \{show other}"
--     Left err => coreFail $ InternalError "lookupHaskellTypeNamespace: \{err}"

-- export
-- insertHaskellTypeNamespace : Ref STGCtxt STGContext => ExtName -> Unique -> Core ()
-- insertHaskellTypeNamespace e u = do
--   ctx <- get STGCtxt
--   let Right adts' = insertHaskellTyName ctx.adts e u
--       | Left err => coreFail $ InternalError "insertHaskellTypeNamespace \{err}"
--   put STGCtxt ({adts := adts'} ctx)

-- export
-- lookupIdrisTermNamespace : Ref STGCtxt STGContext => Core.Name.Name -> Core (Maybe Unique)
-- lookupIdrisTermNamespace n = do
--   ctx <- get STGCtxt
--   case lookupIdrisDtName ctx.adts n of
--     Right (Just (IdrisDtCon _ u)) => pure $ Just u
--     Right Nothing => pure Nothing
--     Right other => coreFail $ InternalError "lookupIdrisTermNamespace: found other constructor than IdrisDtCon. \{show other}"
--     Left err => coreFail $ InternalError "lookupIdrisTermNamespace: \{err}"

-- export
-- insertIdrisTermNamespace : Ref STGCtxt STGContext => Core.Name.Name -> Unique -> Core ()
-- insertIdrisTermNamespace n u = do
--   logLine Debug "Insert name \{show n} \{show u}"
--   ctx <- get STGCtxt
--   let Right adts' = insertIdrisDtName ctx.adts n u
--       | Left err => coreFail $ InternalError "insertIdrisTermNamespace \{err}"
--   put STGCtxt ({adts := adts'} ctx)

-- export
-- lookupHaskellTermNamespace : Ref STGCtxt STGContext => ExtName -> Core (Maybe Unique)
-- lookupHaskellTermNamespace e = do
--   ctx <- get STGCtxt
--   case lookupHaskellDtName ctx.adts e of
--     Right (Just (HaskellDtCon _ u)) => pure $ Just u
--     Right (Just (ADTAliasData _ _ u)) => pure $ Just u
--     Right Nothing => pure Nothing
--     Right other => coreFail $ InternalError "lookupHaskellTermNamespace: found other constructor than HaskellDtCon. \{show other}"
--     Left err => coreFail $ InternalError "lookupHaskellTermNamespace: \{err}"

-- export
-- insertHaskellTermNamespace : Ref STGCtxt STGContext => ExtName -> Unique -> Core ()
-- insertHaskellTermNamespace e u = do
--   ctx <- get STGCtxt
--   let Right adts' = insertHaskellDtName ctx.adts e u
--       | Left err => coreFail $ InternalError "insertHaskellTermNamespace \{err}"
--   put STGCtxt ({adts := adts'} ctx)

-- export
-- addDataType : Ref STGCtxt STGContext => UnitId -> ModuleName -> STyCon -> Core ()
-- addDataType (MkUnitId u) (MkModuleName m) s = do
--   logLine Debug $ "addDataType: \{show u} \{show m} \{show s}"
--   ctx <- get STGCtxt
--   let Right adts1 = registerSTyCon ctx.adts s
--       | Left err => coreFail $ InternalError "addDataType: \{err}"
--   put STGCtxt $ 
--     { dataTypes  $= merge (singleton u (singleton m [s]))
--     , adts := adts1
--     } ctx

-- export
-- getUniqueDataCon : Ref STGCtxt STGContext => Unique -> Core SDataConSg
-- getUniqueDataCon u = do
--   ctx <- get STGCtxt
--   let Just d = lookupSTGDataCon ctx.adts u
--       | Nothing => do
--           coreFail $ InternalError "getUniqueDataCon: datacon is not found for \{show u}"
--   pure d

-- export
-- getIdrisDataCon : Ref STGCtxt STGContext => Core.Name.Name -> Core SDataConSg
-- getIdrisDataCon n = do
--   ctx <- get STGCtxt
--   let Right (Just adtInfo) = lookupIdrisDtName ctx.adts n
--       | Right Nothing => coreFail $ InternalError "getIdrisDataCon: \{show n} is not found."
--       | Left err => coreFail $ InternalError "getIdrisDataCon: \{err}"
--   let Right u = idrisUnique adtInfo
--       | Left err => coreFail $ InternalError "getIdrisDataCon: \{err}"
--   let Just d = lookupSTGDataCon ctx.adts u
--       | Nothing => coreFail $ InternalError "getIdrisDataCon: DataCon is not found for \{show n}"
--   pure d

-- export
-- registerIdrisDCtoTC : Ref STGCtxt STGContext => Core.Name.Name -> Core.Name.Name -> Core ()
-- registerIdrisDCtoTC d t = do
--   logLine Debug "Register \{show d} under \{show t}"
--   ctx <- get STGCtxt
--   let Right adts' = registerIdrisDataConToTyCon ctx.adts d t
--       | Left err => coreFail $ InternalError "registerIdrisDCtoTC: \{err}"
--   put STGCtxt ({ adts := adts'} ctx)

-- export
-- getHaskellDataCon : Ref STGCtxt STGContext => ExtName -> Core SDataConSg
-- getHaskellDataCon n = do
--   ctx <- get STGCtxt
--   let Right (Just adtInfo) = lookupHaskellDtName ctx.adts n
--       | Right Nothing => coreFail $ InternalError "getHaskellDataCon: \{show n} is not found."
--       | Left err => coreFail $ InternalError "getHaskellDataCon: \{err}"
--   let Right u = haskellUnique adtInfo
--       | Left err => coreFail $ InternalError "getHaskellDataCon: \{err}"
--   let Just d = lookupSTGDataCon ctx.adts u
--       | Nothing => coreFail $ InternalError "getHaskellDataCon: DataCon is not found for \{show n}"
--   pure d

-- export
-- registerHaskellDCtoTC : Ref STGCtxt STGContext => ExtName -> ExtName -> Core ()
-- registerHaskellDCtoTC d t = do
--   logLine Debug "Register \{show d} under \{show t}"
--   ctx <- get STGCtxt
--   let Right adts' = registerHaskellDataConToTyCon ctx.adts d t
--       | Left err => coreFail $ InternalError "registerHaskellDCtoTC: \{err}"
--   put STGCtxt ({ adts := adts'} ctx)

-- export
-- insertPrimType : Ref STGCtxt STGContext => PrimType -> Core.Name.Name -> ExtName -> (Unique, Unique) -> Core ()
-- insertPrimType pt n e (ut,ud) = do
--   ctx <- get STGCtxt
--   let Right adts' = insertIdrisPrimType ctx.adts pt n e (ut,ud)
--       | Left err => coreFail $ InternalError "insertPrimType: \{err}"
--   put STGCtxt ({ adts := adts'} ctx)

-- export
-- registerTypeConAlias : Ref STGCtxt STGContext => Core.Name.Name -> ExtName -> Unique -> Unique -> Core ()
-- registerTypeConAlias n e ut ud = do
--   ctx <- get STGCtxt
--   let Right adts' = insertTypeConAlias ctx.adts n e ut ud
--       | Left err => coreFail $ InternalError "registerTypeConAlias \{err}"
--   put STGCtxt ({ adts := adts'} ctx)

-- export
-- registerDataConAlias : Ref STGCtxt STGContext => Core.Name.Name -> ExtName -> Unique -> Core ()
-- registerDataConAlias n e u = do
--   ctx <- get STGCtxt
--   let Right adts' = insertDataConAlias ctx.adts n e u
--       | Left err => coreFail $ InternalError "registerDataConAlias \{err}"
--   put STGCtxt ({ adts := adts'} ctx)

-- export
-- getTyConId : Ref STGCtxt STGContext => Unique -> Core (Maybe STyCon)
-- getTyConId u = do
--   ctx <- get STGCtxt
--   pure $ lookupSTGTyCon ctx.adts u

-- export
-- getDataTypes : Ref STGCtxt STGContext => Core DataTypeMap
-- getDataTypes = do
--   ctx <- get STGCtxt
--   pure ctx.dataTypes

export
lookupStringTable : Ref STGCtxt STGContext => String -> Core (Maybe TopBinding)
lookupStringTable s = do
  logLine Debug "lookupStringTable: \{s}"
  ctx <- get STGCtxt
  pure (lookup s ctx.stringTable)

export
insertStringTable : Ref STGCtxt STGContext => String -> TopBinding -> Core ()
insertStringTable s t = do
  logLine Debug "insertStringTable: \{s}"
  ctx <- get STGCtxt
  put STGCtxt ({ stringTable $= insert s t } ctx)

export
getStringTable : Ref STGCtxt STGContext => Core StringTableMap
getStringTable = do
  logLine Debug "getStringTable"
  ctx <- get STGCtxt
  pure ctx.stringTable

-- export
-- lookupExtBinds : Ref STGCtxt STGContext => ExtName -> Core (Maybe SBinderSg)
-- lookupExtBinds x = do
--   ctx <- get STGCtxt
--   pure $ lookupFFIBinder ctx x

-- export
-- insertExtBinds : Ref STGCtxt STGContext => ExtName -> SBinderSg -> Core ()
-- insertExtBinds e b = do
--   ctx <- get STGCtxt
--   let Right ctx' = insertFFIBinder ctx e b
--       | Left err => coreFail $ InternalError "insertExtBinds \{err}"
--   put STGCtxt ctx'

export
getExtBinds : Ref STGCtxt STGContext => Core (List (ExtName, SBinderSg))
getExtBinds = do
  logLine Debug "getExtBinds"
  ctx <- get STGCtxt
  pure $ getExtBinders ctx.binders

-- export
-- getIdrisTypesTyCon : Ref STGCtxt STGContext => Core TyConId
-- getIdrisTypesTyCon = do
--   ctx <- get STGCtxt
--   pure $ MkTyConId ctx.idrisTypeOfTypes

record Directives where
  constructor MkDirectives
  debugInfo  : Bool
  foreignDir : Maybe String

learnDirectives : Ref Ctxt Defs => Core Directives
learnDirectives = do
  ds <- getDirectives (Other "stg")
  pure $ MkDirectives
    { debugInfo  = elem "debug-info" ds
    , foreignDir = head' $ mapMaybe getForeignDir ds
    }
  where
    getForeignDir : String -> Maybe String
    getForeignDir str = if isPrefixOf "foreign-dir=" str then (Just (drop 12 str)) else Nothing

export
getConfiguration : Ref STGCtxt STGContext => Core Configuration
getConfiguration = map (.configuration) (get STGCtxt)

export
mkSTGContext
  :  Ref Ctxt Defs
  => Core (Ref STGCtxt STGContext)
mkSTGContext = do
  -- let mainUnique      = MkUnique 'm' 0
  -- let mainArgUnique   = MkUnique 'm' 1
  -- let typeOfTypes     = MkUnique 'z' 2
  ds <- learnDirectives
  newRef STGCtxt (MkSTGContext
    { configuration = MkConfiguration
        { foreignDirectory
            = fromMaybe "./.foreign" $ foreignDir ds
        , logLevel
            = if debugInfo ds then Debug else Message
        }
    , counter              = 0
    -- , dataTypes            = empty
    , stringTable          = empty
    -- , adts                 = createADTs typeOfTypes
    , adts2                = createADTs2
    , binders              = createBinders
    -- , mainUnique           = mainUnique
    -- , mainArgUnique        = mainArgUnique
    -- , idrisTypeOfTypes     = typeOfTypes
    -- , idrisTypesSTyCon     = Nothing
    -- , extNameBinders       = empty
    , ffiFiles             = empty
    , adtAliasFiles        = empty
    })

stgNameOf : ResolvedName -> STG.Name
stgNameOf (IdrisName n)       = binderStr n
stgNameOf (AliasedName n e a) = stgName e

||| Create a new Binder for the name, aimed to be used in DataCon
createWorkerBinder : Ref STGCtxt STGContext => ResolvedName -> DataConIdSg -> SrcSpan -> Core LiftedRepBinder
createWorkerBinder n i s = do
  pure $ MkSBinder
    { binderName    = stgNameOf n
    , binderId      = MkBinderId !(mkUnique 'w')
    , binderTypeSig = "Worker"
    , binderScope   = case n of
        IdrisName   _     => GlobalScope
        AliasedName _ _ _ => HaskellExported
    , binderDetails = DataConWorkId i
    , binderInfo    = "TODO: Worker"
    , binderDefLoc  = s
    }

createWorkerBinderExt : Ref STGCtxt STGContext => ExtName -> DataConIdSg -> SrcSpan -> Core LiftedRepBinder
createWorkerBinderExt e d s = pure
  $ MkSBinder
    { binderName    = stgName e
    , binderId      = MkBinderId !(mkUnique 'o')
    , binderTypeSig = "Worker"
    , binderScope   = HaskellExported
    , binderDetails = DataConWorkId d
    , binderInfo    = "TODO: Worker"
    , binderDefLoc  = s
    }

createSDataCon : Ref STGCtxt STGContext => ResolvedName -> DataConRep -> SrcSpan -> Core SDataConSg
createSDataCon n r s = do  
  dataConId <- MkDataConId <$> (mkUnique 'd')
  pure
    (MkDPair r
      (MkSDataCon
        { name    = stgNameOf n
        , ident   = dataConId
        , worker  = !(createWorkerBinder n (_ ** dataConId) s)
        , defLoc  = s
        }))

export
createExtSDataCon : Ref STGCtxt STGContext => ExtName -> DataConRep -> SrcSpan -> Core SDataConSg
createExtSDataCon e r s = do
  let dataConId = MkDataConId !(mkUnique 'e')
  pure
    (MkDPair r
      (MkSDataCon
        { name    = stgName e
        , ident   = dataConId
        , worker  = !(createWorkerBinderExt e (_ ** dataConId) s)
        , defLoc  = s
        }))

-- TODO: Remove duplication
mkSrcSpan : FC -> SrcSpan
mkSrcSpan (MkFC file (sl,sc) (el,ec))
  = SsRealSrcSpan (MkRealSrcSpan (show file) (sl + 1) (sc + 1) (el + 1) (ec + 1)) Nothing
mkSrcSpan (MkVirtualFC file (sl,sc) (el,ec))
  = SsRealSrcSpan (MkRealSrcSpan (show file) (sl + 1) (sc + 1) (el + 1) (ec + 1)) Nothing
mkSrcSpan EmptyFC
  = SsUnhelpfulSpan "<no location>"

export
covering
insertDTCon
  :  Ref Ctxt Defs => Ref STGCtxt STGContext
  => Core.Name.Name -> DataConRep -> FC
  -> Core SDataConSg
insertDTCon n0 r fc = do
  ctx <- get STGCtxt
  fn <- toFullNames n0
  aliasName <- constructorExtName fn
  checkArity aliasName (arity r)
  dataCon <- createSDataCon aliasName r (mkSrcSpan fc)
  case aliasName of
    IdrisName n => do
      logLine Debug "insertDTCon: \{show n} \{show (identSg dataCon)}"
      let Right adts2 = insertIdrisDt fn dataCon ctx.adts2
          | Left err => coreFail $ InternalError "insertDTCon: \{err}"
      modifySTGCtxt ({ adts2 := adts2 })
    AliasedName n en a => do
      logLine Debug "insertDTCon: \{show (n,en)} \{show (identSg dataCon)}"
      let Right adts2 = insertAliasDt fn en dataCon ctx.adts2
          | Left err => coreFail $ InternalError "insertDTCon: \{err}"
      modifySTGCtxt ({ adts2 := adts2 })
  pure dataCon
  where
    arity : DataConRep -> Maybe Nat
    arity (AlgDataCon xs)     = Just (length xs)
    arity (UnboxedTupleCon k) = Nothing

    checkArity : ResolvedName -> Maybe Nat -> Core ()
    checkArity (IdrisName _)   _ = pure () -- No need to check the arity
    checkArity (AliasedName _ _ a) (Just e) =
      when (a /= e) $ coreFail $ InternalError "insertDTCon: Alias defined arity \{show a} does not match \{show e}"
    checkArity (AliasedName _ _ _) Nothing =
      coreFail $ InternalError "insertDTCon: Shouldn't happen, got aliased name with UnboxedTuple."

export
covering
lookupDTCon : Ref Ctxt Defs => Ref STGCtxt STGContext => Core.Name.Name -> Core (SDataConSg, STyCon)
lookupDTCon n0 = do
  logLine Debug "lookupDTCon: \{show n0}"
  ctx <- get STGCtxt
  fn <- toFullNames n0
  datacon <- case !(constructorExtName fn) of
    IdrisName n => do
      let Just datacon = lookupIdrisDt n ctx.adts2
          | Nothing => coreFail $ InternalError "lookupDTCon: No registered datacon found for \{show fn}"
      pure datacon
    AliasedName n en a => do
      let Just (extName, datacon) = lookupAliasDt n ctx.adts2
          | Nothing => coreFail $ InternalError "lookupDTCon: No registered datacon found for \{show fn}"
      pure datacon

  let Just stycon = lookupSTypeOfDataCon datacon ctx.adts2
      | Nothing => coreFail $ InternalError "lookupDTCon: No STyCon is found for \{show fn}"
  pure (datacon, stycon)

export
createSTyCon
  :  Ref STGCtxt STGContext
  => Either Core.Name.Name ExtName -> List SDataConSg -> SrcSpan
  -> Core STyCon
createSTyCon n ds s = do
  logLine Debug "createSTyCon: \{show n}"
  pure
    $ MkSTyCon
      { Name = case n of
          Left i => binderStr i
          Right e => stgName e
      , Id = MkTyConId !(mkUnique 't')
      , DataCons = ds
      , DefLoc = s
      }

coreNameOf : PrimType -> Name.Name
coreNameOf IntType      = UN (Basic "Int")
coreNameOf Int8Type     = UN (Basic "Int8")
coreNameOf Int16Type    = UN (Basic "Int16")
coreNameOf Int32Type    = UN (Basic "Int32")
coreNameOf Int64Type    = UN (Basic "Int64")
coreNameOf IntegerType  = UN (Basic "Integer")
coreNameOf Bits8Type    = UN (Basic "Bits8")
coreNameOf Bits16Type   = UN (Basic "Bits16")
coreNameOf Bits32Type   = UN (Basic "Bits32")
coreNameOf Bits64Type   = UN (Basic "Bits64")
coreNameOf StringType   = UN (Basic "String")
coreNameOf CharType     = UN (Basic "Char")
coreNameOf DoubleType   = UN (Basic "Double")
coreNameOf WorldType    = UN (Basic "%World")

primTypeOfName : Name.Name -> Maybe PrimType
primTypeOfName (UN (Basic "Int"))     = Just IntType
primTypeOfName (UN (Basic "Int8"))    = Just Int8Type
primTypeOfName (UN (Basic "Int16"))   = Just Int16Type
primTypeOfName (UN (Basic "Int32"))   = Just Int32Type
primTypeOfName (UN (Basic "Int64"))   = Just Int64Type
primTypeOfName (UN (Basic "Integer")) = Just IntegerType
primTypeOfName (UN (Basic "Bits8"))   = Just Bits8Type
primTypeOfName (UN (Basic "Bits16"))  = Just Bits16Type
primTypeOfName (UN (Basic "Bits32"))  = Just Bits32Type
primTypeOfName (UN (Basic "Bits64"))  = Just Bits64Type
primTypeOfName (UN (Basic "String"))  = Just StringType
primTypeOfName (UN (Basic "Char"))    = Just CharType
primTypeOfName (UN (Basic "Double"))  = Just DoubleType
primTypeOfName (UN (Basic "%World"))  = Just WorldType
primTypeOfName _ = Nothing

0
primTypeNameMapping : (p : PrimType) -> primTypeOfName (coreNameOf p) === Just p
primTypeNameMapping IntType     = Refl
primTypeNameMapping Int8Type    = Refl
primTypeNameMapping Int16Type   = Refl
primTypeNameMapping Int32Type   = Refl
primTypeNameMapping Int64Type   = Refl
primTypeNameMapping IntegerType = Refl
primTypeNameMapping Bits8Type   = Refl
primTypeNameMapping Bits16Type  = Refl
primTypeNameMapping Bits32Type  = Refl
primTypeNameMapping Bits64Type  = Refl
primTypeNameMapping StringType  = Refl
primTypeNameMapping CharType    = Refl
primTypeNameMapping DoubleType  = Refl
primTypeNameMapping WorldType   = Refl


export
covering
insertTYCon
  :  Ref Ctxt Defs
  => Ref STGCtxt STGContext
  => Core.Name.Name -> Nat -> List SDataConSg -> FC
  -> Core (STyCon, SDataConSg)
insertTYCon n0 a ds fc = do
  ctx <- get STGCtxt
  fn <- toFullNames n0
  let span = mkSrcSpan fc
  let drep = AlgDataCon (replicate a LiftedRep)
  typeConDCon <- createSDataCon (IdrisName fn) drep span
  stycon <- case !(typeExtName fn) of
    Nothing => do
      stycon <- createSTyCon (Left fn) ds span
      logLine Debug "insertTYCon: \{show fn} \{show (Id stycon, map identSg (DataCons stycon))}"
      let Right adts2 = insertIdrisTy fn stycon typeConDCon ctx.adts2
          | Left err => coreFail $ InternalError "insertTYCon: \{err}"
      modifySTGCtxt ({ adts2 := adts2 })
      pure stycon
    Just ex => do
      stycon <- createSTyCon (Right ex) ds span
      logLine Debug "insertTYCon: \{show ex} \{show (Id stycon,map identSg (DataCons stycon))}"
      let Right adts2 = insertAliasTy fn ex stycon typeConDCon ctx.adts2
          | Left err => coreFail $ InternalError "insertTYCon: \{err}"
      modifySTGCtxt ({ adts2 := adts2 })
      pure stycon
  pure (stycon,typeConDCon)

export
covering
lookupTYCon
  :  Ref Ctxt Defs
  => Ref STGCtxt STGContext
  => Core.Name.Name
  -> Core (STyCon, TypeOfTypeDataCon)
lookupTYCon n0 = do
  logLine Debug "lookupTYCon: \{show n0}"
  ctx <- get STGCtxt
  fn <- toFullNames n0
  case primTypeOfName fn of
    Just pt => do
      let Just d = lookupPrimType pt ctx.adts2
          | Nothing => coreFail $ InternalError "lookupTYCon: No STyCon found for \{show pt}"
      pure (d.typeConSTG, d.dataConOfType)
    Nothing => case !(typeExtName fn) of
      Nothing => do
        let Just td = lookupIdrisTy fn ctx.adts2
            | Nothing => coreFail $ InternalError "lookupTYCon: No STyCon found for \{show fn}"
        pure td
      Just _ => do
        let Just (_, td) = lookupAliasTy fn ctx.adts2
            | Nothing => coreFail $ InternalError "lookupTYCon: No STyCon found for \{show fn}"
        pure td

export
createTypeOfTypes : Ref STGCtxt STGContext => Core (UnitId, ModuleName, STyCon)
createTypeOfTypes = do
  ctx <- get STGCtxt
  let primTypeToTDataCons : List TypeOfTypeDataCon = map (dataConOfType . snd) $ SortedMap.toList $ ctx.adts2.getPrimTypeMap
  let idrisTyConDataCons : List TypeOfTypeDataCon = map snd $ toList $ ctx.adts2.getIdrisTyMap
  let aliasTyConDataCons : List TypeOfTypeDataCon = map (snd . snd) $ toList $ ctx.adts2.getAliasTyMap
  let datacons = primTypeToTDataCons ++ idrisTyConDataCons ++ aliasTyConDataCons
  let stycon : STyCon := MkSTyCon
        { Name = "::.Type.Of.Type"
        , Id = MkTyConId !(mkUnique 'w')
        , DataCons = datacons
        , DefLoc = SsUnhelpfulSpan "TypeOfTypes"
        }
  let Right adts2 = insertTypeOfTypes stycon ctx.adts2
      | Left err => coreFail $ InternalError "createTypeOfTypes: \{err}"
  modifySTGCtxt ({ adts2 := adts2 })
  pure (MkUnitId MAIN_UNIT, MkModuleName MAIN_MODULE, stycon)

export
getTypeOfTypes : Ref STGCtxt STGContext => Core STyCon
getTypeOfTypes = do
  ctx <- get STGCtxt
  let Just stycon = ctx.adts2.getTypeOfTypes
      | Nothing => coreFail $ InternalError "getTypeOfTypes: Type Of Type is not initialized."
  pure stycon

export
runtimeRepresentationOf : PrimType -> Core (ExtName, ExtName, List PrimRep)
runtimeRepresentationOf IntType = pure
  ( MkExtName "ghc-prim" ["GHC", "Types"] "Int"
  , MkExtName "ghc-prim" ["GHC", "Types"] "I#", [IntRep])
runtimeRepresentationOf IntegerType = pure
  ( MkExtName "main" ["Idris", "Runtime", "Integer"] "BI"
  , MkExtName "main" ["Idris", "Runtime", "Integer"] "BI", [LiftedRep])
runtimeRepresentationOf Int8Type = pure
  ( MkExtName "base" ["GHC", "Int"] "Int8"
  , MkExtName "base" ["GHC", "Int"] "I8#", [Int8Rep])
runtimeRepresentationOf Int16Type = pure
  ( MkExtName "base" ["GHC", "Int"] "Int16"
  , MkExtName "base" ["GHC", "Int"] "I16#", [Int16Rep])
runtimeRepresentationOf Int32Type = pure
  ( MkExtName "base" ["GHC", "Int"] "Int32"
  , MkExtName "base" ["GHC", "Int"] "I32#", [Int32Rep])
runtimeRepresentationOf Int64Type = pure
  ( MkExtName "base" ["GHC", "Int"] "Int64"
  , MkExtName "base" ["GHC", "Int"] "I64#", [Int64Rep])
runtimeRepresentationOf Bits8Type = pure
  ( MkExtName "base" ["GHC", "Word"] "Word8"
  , MkExtName "base" ["GHC", "Word"] "W8#", [Word8Rep])
runtimeRepresentationOf Bits16Type = pure
  ( MkExtName "base" ["GHC", "Word"] "Word16"
  , MkExtName "base" ["GHC", "Word"] "W16#", [Word16Rep])
runtimeRepresentationOf Bits32Type = pure
  ( MkExtName "base" ["GHC", "Word"] "Word32"
  , MkExtName "base" ["GHC", "Word"] "W32#", [Word32Rep])
runtimeRepresentationOf Bits64Type = pure
  ( MkExtName "base" ["GHC", "Word"] "Word64"
  , MkExtName "base" ["GHC", "Word"] "W64#", [Word64Rep])
runtimeRepresentationOf CharType = pure
  ( MkExtName "ghc-prim" ["GHC", "Types"] "Char"
  , MkExtName "ghc-prim" ["GHC", "Types"] "C#", [CharRep])
runtimeRepresentationOf DoubleType = pure
  ( MkExtName "ghc-prim" ["GHC", "Types"] "Double"
  , MkExtName "ghc-prim" ["GHC", "Types"] "D#", [DoubleRep])
runtimeRepresentationOf WorldType = pure
  ( MkExtName "main" ["Idris", "Runtime", "World"] "World"
  , MkExtName "main" ["Idris", "Runtime", "World"] "World", [])
runtimeRepresentationOf other
  = coreFail $ UserError $ "No type and data constructor for " ++ show other

export
registerPrimType : Ref Ctxt Defs => Ref STGCtxt STGContext => PrimType -> Core ()
registerPrimType pt = do
  ctx <- get STGCtxt
  (tyExt, constExt, params) <- runtimeRepresentationOf pt
  let iName = coreNameOf pt
  datacon <- createSDataCon
              (AliasedName iName constExt (length params))
              (AlgDataCon params)
              (SsUnhelpfulSpan (show pt))
  stycon <- createSTyCon
              (Right tyExt)
              [datacon]
              (SsUnhelpfulSpan (show pt ++ "Ty"))
  dataconToT <- createSDataCon
                  (IdrisName iName)
                  (AlgDataCon [])
                  (SsUnhelpfulSpan (show pt ++ "ToT"))
  logLine Debug "registerPrimType: \{show pt} \{show (identSg datacon)}"
  let Right adts2 = insertPrimTypeADTs2 pt constExt datacon tyExt stycon dataconToT ctx.adts2
      | Left err => coreFail $ InternalError "discoverPrimType \{show pt} \{err}"
  modifySTGCtxt ({ adts2 := adts2 })

export
lookupPrimType : Ref STGCtxt STGContext => PrimType -> Core PrimTypeADTDesc
lookupPrimType p = do
  logLine Debug "lookupPrimType: \{show p}"
  ctx <- get STGCtxt
  let Just info = lookupPrimType p ctx.adts2
      | Nothing => coreFail $ InternalError "lookupPrimType: \{show p} is not registered."
  pure info

export
insertExtNameDTCon : Ref STGCtxt STGContext => ExtName -> SDataConSg -> Core ()
insertExtNameDTCon e d = do
  logLine Debug "insertExtNameDTCon: \{show e} \{show (identSg d)}"
  ctx <- get STGCtxt
  let Right adts2 = insertExtDataCon e d ctx.adts2
      | Left err => coreFail $ InternalError "insertExtNameDTCon: \{err}"
  put STGCtxt ({ adts2 := adts2 } ctx)

export
lookupExtNameDTCon : Ref STGCtxt STGContext => ExtName -> Core SDataConSg
lookupExtNameDTCon e = do
  logLine Debug "lookupExtNameDTCon: \{show e}"
  ctx <- get STGCtxt
  let Just d = lookupExtDataCon e ctx.adts2
      | Nothing => coreFail $ InternalError "lookupExtNameDTCon: \{show e} is not registered."
  pure d

export
insertExtNameTyCon : Ref STGCtxt STGContext => ExtName -> STyCon -> Core ()
insertExtNameTyCon e s = do
  logLine Debug "insertExtNameTyCon: \{show e} \{show (Id s, map identSg (DataCons s))}"
  ctx <- get STGCtxt
  let Right adts2 = insertExtTyCon e s ctx.adts2
      | Left err => coreFail $ InternalError "insertExtNameTyCon: \{err}"
  put STGCtxt ({ adts2 := adts2 } ctx)

export
lookupExtNameTyCon : Ref STGCtxt STGContext => ExtName -> Core STyCon
lookupExtNameTyCon e = do
  logLine Debug "lookupExtNameTyCon: \{show e}"
  ctx <- get STGCtxt
  let Just s = lookupExtTyCon e ctx.adts2
      | Nothing => coreFail $ InternalError "lookupExtNameTyCon: \{show e} is not registered."
  pure s

export
getDefinedDataTypes : Ref STGCtxt STGContext => Core DefinedDataTypes
getDefinedDataTypes = do
  ctx <- get STGCtxt
  pure $ definedDataTypes ctx.adts2

-- export
registerFunctionBinder : Ref STGCtxt STGContext => Name.Name -> FunctionBinder -> Core ()
registerFunctionBinder name sbinder = do
  logLine Debug "registerFunctionBinder: \{show name} \{show (binderId sbinder)}"
  ctx <- get STGCtxt
  let Right binders' = insertFunction name sbinder ctx.binders
      | Left err => coreFail $ InternalError "mkFunctionBinder: \{err}"
  put STGCtxt $ { binders := binders' } ctx

export
lookupFunctionBinder : Ref STGCtxt STGContext => Name.Name -> Core FunctionBinder
lookupFunctionBinder n = do
  logLine Debug "lookupFunctionBinder: \{show n}"
  ctx <- get STGCtxt
  let Just b = lookupFunction n ctx.binders
      | Nothing => coreFail $ InternalError "lookupFunctionBinder: \{show n} is not found."
  pure b

-- export
registerLocalVarBinder : Ref STGCtxt STGContext => Name.Name -> Int -> LocalVarBinder -> Core ()
registerLocalVarBinder n v b = do
  logLine Debug "registerLocalVarBinder: \{show (n,v)} \{show (binderId b)}"
  ctx <- get STGCtxt
  let Right binders' = insertLocalVar n v b ctx.binders
      | Left err => coreFail $ InternalError "registerLocalVarBinder: \{err}"
  put STGCtxt $ { binders := binders' } ctx

-- export
registerFFIBinder : Ref STGCtxt STGContext => ExtName -> FFIBinder -> Core ()
registerFFIBinder e b = do
  logLine Debug "registerFFIBinder: \{show e} \{show (binderId b)}"
  ctx <- get STGCtxt
  let Right binders' = insertFFIBinder e b ctx.binders
      | Left err => coreFail $ InternalError "registerFFIBinder: \{err}"
  put STGCtxt $ { binders := binders' } ctx

export
lookupFFIBinder : Ref STGCtxt STGContext => ExtName -> Core (Maybe FFIBinder)
lookupFFIBinder e = do
  logLine Debug "lookupFFIBinder: \{show e}"
  ctx <- get STGCtxt
  pure $ lookupFFIBinder e ctx.binders

export
mkFunctionBinder
  :  Ref STGCtxt STGContext
  => FC -> Name.Name
  -> Core FunctionBinder
mkFunctionBinder fc name = do
  let sbinder = MkSBinder
        { binderName    = binderStr name
        , binderId      = MkBinderId !(mkUnique 'f')
        , binderTypeSig = "mkFunctionBinder: binderTypeSig" -- TODO
        , binderScope   = GlobalScope
        , binderDetails = VanillaId
        , binderInfo    = "mkFunctionBinder: binderInfo" -- TODO
        , binderDefLoc  = mkSrcSpan fc
        }
  logLine Debug "mkFunctionBinder: \{show name} \{show sbinder.binderId}"
  registerFunctionBinder name sbinder
  pure sbinder

-- export
mkFFIBinder
  :  Ref STGCtxt STGContext
  => FC -> ExtName
  -> Core FFIBinder
mkFFIBinder fc name = do
  pure $
    MkSBinder
        { binderName    = stgName name
        , binderId      = MkBinderId !(mkUnique 'h')
        , binderTypeSig = "mkFFIBinder: binderTypeSig" -- TODO
        , binderScope   = HaskellExported
        , binderDetails = VanillaId
        , binderInfo    = "mkFFIBinder: binderInfo" -- TODO
        , binderDefLoc  = mkSrcSpan fc
        }

||| Ask for a BinderId for the given name, if there is, if not create a one Binder and
||| register in the ExtBindMap
export
extNameLR
  :  Ref STGCtxt STGContext
  => ExtName
  -> Core FFIBinder
extNameLR e = do
  logLine Debug "extNameLR: \{show e}"
  case !(lookupFFIBinder e) of
    Nothing => do
      b <- mkFFIBinder emptyFC e
      registerFFIBinder e b
      pure b
    Just b => pure b

export
lookupLocalVarBinder : Ref STGCtxt STGContext => Name.Name -> AVar -> Core LocalVarBinder
lookupLocalVarBinder n ANull = do
  logLine Debug "lookupLocalVarBinder: \{show n} \{show ANull}"
  extNameLR erasedExtName
lookupLocalVarBinder n (ALocal i) = do
  logLine Debug "lookupLocalVarBinder: \{show n} \{show (ALocal i)}"
  ctx <- get STGCtxt
  let Just b = lookupLocalVar n i ctx.binders
      | Nothing => coreFail $ InternalError "lookupLocalVarBinder: \{show (n,i)} is not defined."
  pure b

export
createLocalVarBinder
  :  Ref STGCtxt STGContext
  => FC -> Name.Name -> AVar
  -> Core LocalVarBinder
createLocalVarBinder fc name ANull = do
  logLine Debug "createLocalVarBinder: \{show name} \{show ANull}"
  extNameLR erasedExtName
createLocalVarBinder fc name (ALocal x) = do
  logLine Debug "createLocalVarBinder: \{show name} \{show (ALocal x)}"
  let sbinder = MkSBinder
        { binderName    = binderStr name
        , binderId      = MkBinderId !(mkUnique 'l')
        , binderTypeSig = "createLocalVarBinder: binderTypeSig" -- TODO
        , binderScope   = LocalScope
        , binderDetails = VanillaId
        , binderInfo    = "createLocalVarBinder: binderTypeSig" -- TODO
        , binderDefLoc  = mkSrcSpan fc
        }
  registerLocalVarBinder name x sbinder
  pure sbinder

export
mkFreshSBinderStr -- TODO: Remove Str suffix
  :  Ref STGCtxt STGContext
  => {rep : RepType} -> Scope -> FC -> String
  -> Core (SBinder rep)
mkFreshSBinderStr scope fc binderName = do
  logLine Debug "Creating Fresh Binder for \{binderName}"
  unique@(MkUnique _ c) <- mkUnique 'l'
  binderId <- MkBinderId <$> mkUnique 'l'
  let typeSig = "mkSBinder: typeSig"
  let details = VanillaId
  let info    = "mkSBinder: IdInfo"
  let defLoc  = mkSrcSpan fc
  pure $ MkSBinder
    { binderName    = (binderName ++ ":" ++ show c)
    , binderId      = binderId
    , binderTypeSig = typeSig
    , binderScope   = scope
    , binderDetails = details
    , binderInfo    = info
    , binderDefLoc  = defLoc
    }

export
dropLocalVars : Ref STGCtxt STGContext => Core ()
dropLocalVars = do
  logLine Debug "Dropping local variables."
  modifySTGCtxt $ { binders $= dropLocalVars }

export
realWorldHashBinder : Ref STGCtxt STGContext => Core (SBinder (SingleValue VoidRep))
realWorldHashBinder = do
  logLine Debug "Access to RealWorld#"
  ctx <- get STGCtxt
  pure $ getRealWorldHash ctx.binders

-- TODO: Use STG definitions
export
defineSoloDataType : Ref STGCtxt STGContext => Core ()
defineSoloDataType = do
  -- d <- createSTyConExt (soloExtName, SsUnhelpfulSpan "") [(soloExtName, UnboxedTupleCon 1, SsUnhelpfulSpan "")]
  -- defineDataType (mkUnitId soloExtName) (mkModuleName soloExtName) d
  datacon <- createExtSDataCon soloExtName (UnboxedTupleCon 1) (SsUnhelpfulSpan "Solo")
  stycon <- createSTyCon (Right soloExtName) [datacon] (SsUnhelpfulSpan "Solo")
  insertExtNameDTCon soloExtName datacon
  insertExtNameTyCon soloExtName stycon
