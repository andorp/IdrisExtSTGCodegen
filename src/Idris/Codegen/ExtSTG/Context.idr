module Idris.Codegen.ExtSTG.Context

import Core.Core
import Core.Context
import Core.Options 
import Libraries.Data.StringMap
import Libraries.Data.IntMap
import Idris.Codegen.ExtSTG.ExtName
import Idris.Codegen.ExtSTG.STG
import Idris.Codegen.ExtSTG.Configuration
import Idris.Codegen.ExtSTG.ADTAlias
import Idris.Codegen.ExtSTG.ADTs
import Core.Context.Context
import Data.String -- (isPreffixOf)
import Data.String.Extra -- (drop)
import Data.SortedMap

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


public export
DataTypeMap : Type
DataTypeMap = StringMap {-UnitId-} (StringMap {-ModuleName-} (List STyCon))

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
  adtResolved           : IntMap    STyCon
  adtNamed              : SortedMap Core.Name.Name STyCon
  dataTypes             : DataTypeMap
  stringTable           : StringTableMap
  adts                  : ADTs
  mainUnique            : Unique
  mainArgUnique         : Unique
  idrisTypesSTyCon      : Maybe STyCon
  extNameBinders        : SortedMap ExtName SBinderSg

lookupExtNameBinder : STGContext -> ExtName -> Maybe SBinderSg
lookupExtNameBinder ctx e = lookup e ctx.extNameBinders

insertExtNameBinder : STGContext -> ExtName -> SBinderSg -> Either String STGContext
insertExtNameBinder ctx e b = do
  let u = getBinderIdUnique (binderId (snd b))
  -- TODO
  -- case lookup u ctx.uniqueToADTInfo of
  --   -- TODO: Check if registered as ADT.
  --   Just adtInfo => do
  --     let Right e' = extNameOfADT adtInfo
  --         | Left err => Left err
  --     let True = (e' == e)
  --         | False => Left "Found a different registered name. \{show u}, found \{show e'}, registering \{show e}"
  --     Right ()          
  --   Nothing => Right ()              
  Right $ { extNameBinders $= insert e b } ctx

extNameBinderList : STGContext -> List (ExtName, SBinderSg)
extNameBinderList ctx = toList ctx.extNameBinders

lookupIdrisTypesSTyCon : STGContext -> Maybe STyCon
lookupIdrisTypesSTyCon = idrisTypesSTyCon

export
logLine : Ref STGCtxt STGContext => Configuration.LogLevel -> Lazy String -> Core ()
logLine levelOfMsg msg = do
  logLvl <- map (\c => c.configuration.logLevel) $ get STGCtxt
  when (logLvl <= levelOfMsg) $ coreLift $ putStrLn msg

export
getMainUnique : Ref STGCtxt STGContext => Core Unique
getMainUnique = do
  ctx <- get STGCtxt
  pure ctx.mainUnique

export
getMainArgUnique : Ref STGCtxt STGContext => Core Unique
getMainArgUnique = do
  ctx <- get STGCtxt
  pure ctx.mainArgUnique

export
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
  pure u

export
insertSTGDataCon : Ref STGCtxt STGContext => SDataConSg -> Core ()
insertSTGDataCon s = do
  ctx <- get STGCtxt
  let Right adts' = registerSDataCon ctx.adts s
      | Left err => coreFail $ InternalError "insertSTGDataCon: \{err}"
  put STGCtxt ({ adts := adts' } ctx)

export
lookupIdrisTypeDataCon : Ref STGCtxt STGContext => Core.Name.Name -> Core (Maybe Unique)
lookupIdrisTypeDataCon n = do
  ctx <- get STGCtxt
  case lookupIdrisTyName ctx.adts n of
    Right (Just (IdrisTyCon _ (MkIdrisTyUnique _ u))) => pure $ Just u
    Right Nothing => pure Nothing
    Right other => coreFail $ InternalError "lookupIdrisTypeDataCon: found other constructor than IdrisTyCon."
    Left err => coreFail $ InternalError "lookupIdrisTypeDataCon: \{err}"

||| Looks up the associated tycon with the given name of a data constructor.
export
lookupIdrisAssociatedTyCon
  : Ref STGCtxt STGContext => Core.Name.Name -> Core STyCon
lookupIdrisAssociatedTyCon n = do
  ctx <- get STGCtxt
  let Right (Just adtinfo) = lookupIdrisDtName ctx.adts n
      | Right Nothing => coreFail $ InternalError "lookupIdrisAssociatedTyCon: No ADTInfo is found for \{show n}"
      | Left err => coreFail $ InternalError "lookupIdrisAssociatedTyCon: \{err}"
  let Right ud = idrisUnique adtinfo
      | Left err => coreFail $ InternalError "lookupIdrisAssociatedTyCon: \{err}"
  let Right ut = lookupDataConUniqueToTyCon ctx.adts ud
      | Left err => coreFail $ InternalError "lookupIdrisAssociatedTyCon: \{err}"
  let Just stycon = lookupSTGTyCon ctx.adts ut
      | Nothing => coreFail $ InternalError "lookupIdrisAssociatedTyCon: No associated STyCon is found for \{show n}"
  pure stycon      

||| Looks up the associated tycon with the given name of a data constructor.
export
lookupHaskellAssociatedTyCon
  : Ref STGCtxt STGContext => ExtName -> Core STyCon
lookupHaskellAssociatedTyCon e = do
  ctx <- get STGCtxt
  let Right (Just adtinfo) = lookupHaskellDtName ctx.adts e
      | Right Nothing => coreFail $ InternalError "lookupHaskellAssociatedTyCon: No ADTInfo is found for \{show e}"
      | Left err => coreFail $ InternalError "lookupHaskellAssociatedTyCon: \{err}"
  let Right ud = haskellUnique adtinfo
      | Left err => coreFail $ InternalError "lookupHaskellAssociatedTyCon: \{err}"
  let Right ut = lookupDataConUniqueToTyCon ctx.adts ud
      | Left err => coreFail $ InternalError "lookupHaskellAssociatedTyCon: \{err}"
  let Just stycon = lookupSTGTyCon ctx.adts ut
      | Nothing => coreFail $ InternalError "lookupHaskellAssociatedTyCon: No associated STyCon is found for \{show e}"
  pure stycon      

export
lookupAssociatedTyCon
  : Ref STGCtxt STGContext => Core.Name.Name -> Core STyCon
lookupAssociatedTyCon n = case constructorExtName n of
  Nothing     => lookupIdrisAssociatedTyCon n
  Just (e, _) => lookupHaskellAssociatedTyCon e

export
lookupIdrisTypeDataConDef : Ref STGCtxt STGContext => Unique -> Core SDataConSg
lookupIdrisTypeDataConDef u = do
  ctx <- get STGCtxt
  let Just d = lookupIdrisSTGDataCon ctx.adts u
      | Nothing => coreFail $ InternalError "lookupIdrisTypeDataConDef: No STG datacon for \{show u}"
  pure d

export
insertIdrisTypeDataCon : Ref STGCtxt STGContext => Core.Name.Name -> Unique -> Unique -> Core ()
insertIdrisTypeDataCon n ut ud = do
  ctx <- get STGCtxt
  let Right adts' = insertIdrisTyName ctx.adts n (MkIdrisTyUnique ut ud)
      | Left err => coreFail $ InternalError "insertIdrisTypeDataCon \{err}"
  put STGCtxt ({adts := adts'} ctx)

-- TODO: Unify with lookupIdrisTypeDataCon, returning IdrisTyUnique
export
lookupIdrisTypeNamespace : Ref STGCtxt STGContext => Core.Name.Name -> Core (Maybe IdrisTyUnique)
lookupIdrisTypeNamespace n = do
  ctx <- get STGCtxt
  case lookupIdrisTyName ctx.adts n of
    Right (Just (IdrisTyCon _ u)) => pure $ Just u
    Right Nothing => pure Nothing
    Right other => coreFail $ InternalError "lookupIdrisTypeNamespace: found other constructor than IdrisTyCon."
    Left err => coreFail $ InternalError "lookupIdrisTypeNamespace: \{err}"


export
lookupHaskellTypeNamespace : Ref STGCtxt STGContext => ExtName -> Core (Maybe Unique)
lookupHaskellTypeNamespace e = do
  ctx <- get STGCtxt
  case lookupHaskellTyName ctx.adts e of
    Right (Just (HaskellTyCon _ u)) => pure $ Just u
    Right Nothing => pure Nothing
    Right other => coreFail $ InternalError "lookupHaskellTypeNamespace: found other constructor than IdrisTyCon."
    Left err => coreFail $ InternalError "lookupHaskellTypeNamespace: \{err}"

export
insertHaskellTypeNamespace : Ref STGCtxt STGContext => ExtName -> Unique -> Core ()
insertHaskellTypeNamespace e u = do
  ctx <- get STGCtxt
  let Right adts' = insertHaskellTyName ctx.adts e u
      | Left err => coreFail $ InternalError "insertHaskellTypeNamespace \{err}"
  put STGCtxt ({adts := adts'} ctx)

export
lookupIdrisTermNamespace : Ref STGCtxt STGContext => Core.Name.Name -> Core (Maybe Unique)
lookupIdrisTermNamespace n = do
  ctx <- get STGCtxt
  case lookupIdrisDtName ctx.adts n of
    Right (Just (IdrisDtCon _ u)) => pure $ Just u
    Right Nothing => pure Nothing
    Right other => coreFail $ InternalError "lookupIdrisTermNamespace: found other constructor than IdrisTyCon."
    Left err => coreFail $ InternalError "lookupIdrisTermNamespace: \{err}"

export
insertIdrisTermNamespace : Ref STGCtxt STGContext => Core.Name.Name -> Unique -> Core ()
insertIdrisTermNamespace n u = do
  logLine Debug "Insert name \{show n} \{show u}"
  ctx <- get STGCtxt
  let Right adts' = insertIdrisDtName ctx.adts n u
      | Left err => coreFail $ InternalError "insertIdrisTermNamespace \{err}"
  put STGCtxt ({adts := adts'} ctx)

export
lookupHaskellTermNamespace : Ref STGCtxt STGContext => ExtName -> Core (Maybe Unique)
lookupHaskellTermNamespace e = do
  ctx <- get STGCtxt
  case lookupHaskellDtName ctx.adts e of
    Right (Just (HaskellDtCon _ u)) => pure $ Just u
    Right Nothing => pure Nothing
    Right other => coreFail $ InternalError "lookupIdrisTermNamespace: found other constructor than IdrisTyCon."
    Left err => coreFail $ InternalError "lookupIdrisTermNamespace: \{err}"

export
insertHaskellTermNamespace : Ref STGCtxt STGContext => ExtName -> Unique -> Core ()
insertHaskellTermNamespace e u = do
  ctx <- get STGCtxt
  let Right adts' = insertHaskellDtName ctx.adts e u
      | Left err => coreFail $ InternalError "insertHaskellTermNamespace \{err}"
  put STGCtxt ({adts := adts'} ctx)

export
addDataType : Ref STGCtxt STGContext => UnitId -> ModuleName -> STyCon -> Core ()
addDataType (MkUnitId u) (MkModuleName m) s = do
  logLine Debug $ "addDataType: \{show u} \{show m} \{show s}"
  ctx <- get STGCtxt
  let Right adts1 = registerSTyCon ctx.adts s
      | Left err => coreFail $ InternalError "addDataType: \{err}"
  put STGCtxt $ 
    { dataTypes  $= merge (singleton u (singleton m [s]))
    , adts := adts1
    } ctx


export
getUniqueDataCon : Ref STGCtxt STGContext => Unique -> Core SDataConSg
getUniqueDataCon u = do
  ctx <- get STGCtxt
  let Just d = lookupSTGDataCon ctx.adts u
      | Nothing => do
          coreFail $ InternalError "getUniqueDataCon: datacon is not found for \{show u}"
  pure d

export
getIdrisDataCon : Ref STGCtxt STGContext => Core.Name.Name -> Core SDataConSg
getIdrisDataCon n = do
  ctx <- get STGCtxt
  let Right (Just adtInfo) = lookupIdrisDtName ctx.adts n
      | Right Nothing => coreFail $ InternalError "getIdrisDataCon: \{show n} is not found."
      | Left err => coreFail $ InternalError "getIdrisDataCon: \{err}"
  let Right u = idrisUnique adtInfo
      | Left err => coreFail $ InternalError "getIdrisDataCon: \{err}"
  let Just d = lookupSTGDataCon ctx.adts u
      | Nothing => coreFail $ InternalError "getIdrisDataCon: DataCon is not found for \{show n}"
  pure d

export
registerIdrisDCtoTC : Ref STGCtxt STGContext => Core.Name.Name -> Core.Name.Name -> Core ()
registerIdrisDCtoTC d t = do
  ctx <- get STGCtxt
  let Right adts' = registerIdrisDataConToTyCon ctx.adts d t
      | Left err => coreFail $ InternalError "registerIdrisDCtoTC: \{err}"
  put STGCtxt ({ adts := adts'} ctx)

export
getHaskellDataCon : Ref STGCtxt STGContext => ExtName -> Core SDataConSg
getHaskellDataCon n = do
  ctx <- get STGCtxt
  let Right (Just adtInfo) = lookupHaskellDtName ctx.adts n
      | Right Nothing => coreFail $ InternalError "getHaskellDataCon: \{show n} is not found."
      | Left err => coreFail $ InternalError "getHaskellDataCon: \{err}"
  let Right u = haskellUnique adtInfo
      | Left err => coreFail $ InternalError "getHaskellDataCon: \{err}"
  let Just d = lookupSTGDataCon ctx.adts u
      | Nothing => coreFail $ InternalError "getHaskellDataCon: DataCon is not found for \{show n}"
  pure d

export
registerHaskellDCtoTC : Ref STGCtxt STGContext => ExtName -> ExtName -> Core ()
registerHaskellDCtoTC d t = do
  ctx <- get STGCtxt
  let Right adts' = registerHaskellDataConToTyCon ctx.adts d t
      | Left err => coreFail $ InternalError "registerHaskellDCtoTC: \{err}"
  put STGCtxt ({ adts := adts'} ctx)

export
getTyConId : Ref STGCtxt STGContext => Unique -> Core (Maybe STyCon)
getTyConId u = do
  ctx <- get STGCtxt
  pure $ lookupSTGTyCon ctx.adts u

export
getDataTypes : Ref STGCtxt STGContext => Core DataTypeMap
getDataTypes = do
  ctx <- get STGCtxt
  pure ctx.dataTypes

export
lookupADTResolved : Ref STGCtxt STGContext => Int -> Core (Maybe STyCon)
lookupADTResolved r = do
  ctx <- get STGCtxt
  pure (lookup r ctx.adtResolved)

export
insertADTResolved : Ref STGCtxt STGContext => Int -> STyCon -> Core ()
insertADTResolved r s = do
  ctx <- get STGCtxt
  put STGCtxt ({ adtResolved $= insert r s } ctx)

export
lookupADTNamed : Ref STGCtxt STGContext => Core.Name.Name -> Core (Maybe STyCon)
lookupADTNamed n = do
  ctx <- get STGCtxt
  pure (lookup n ctx.adtNamed)

export
insertADTNamed : Ref STGCtxt STGContext => Core.Name.Name -> STyCon -> Core ()
insertADTNamed n s = do
  ctx <- get STGCtxt
  put STGCtxt ({ adtNamed $= insert n s } ctx)

export
lookupStringTable : Ref STGCtxt STGContext => String -> Core (Maybe TopBinding)
lookupStringTable s = do
  ctx <- get STGCtxt
  pure (lookup s ctx.stringTable)

export
insertStringTable : Ref STGCtxt STGContext => String -> TopBinding -> Core ()
insertStringTable s t = do
  ctx <- get STGCtxt
  put STGCtxt ({ stringTable $= insert s t } ctx)

export
getStringTable : Ref STGCtxt STGContext => Core StringTableMap
getStringTable = do
  ctx <- get STGCtxt
  pure ctx.stringTable

export
lookupExtBinds : Ref STGCtxt STGContext => ExtName -> Core (Maybe SBinderSg)
lookupExtBinds x = do
  ctx <- get STGCtxt
  pure $ lookupExtNameBinder ctx x

export
insertExtBinds : Ref STGCtxt STGContext => ExtName -> SBinderSg -> Core ()
insertExtBinds e b = do
  ctx <- get STGCtxt
  let Right ctx' = insertExtNameBinder ctx e b
      | Left err => coreFail $ InternalError "insertExtBinds \{err}"
  put STGCtxt ctx'

export
getExtBinds : Ref STGCtxt STGContext => Core (List (ExtName, SBinderSg))
getExtBinds = do
  ctx <- get STGCtxt
  pure $ extNameBinderList ctx

export
getIdrisTypesTyCon : Ref STGCtxt STGContext => Core TyConId
getIdrisTypesTyCon = do
  ctx <- get STGCtxt
  let Just stycon = lookupIdrisTypesSTyCon ctx
      | Nothing => coreFail $ InternalError "ADT for type of types is not registered."
  pure stycon.Id


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
  let mainUnique      = MkUnique 'm' 0
  let mainArgUnique   = MkUnique 'm' 1
  ds <- learnDirectives
  newRef STGCtxt (MkSTGContext
    { configuration = MkConfiguration
        { foreignDirectory
            = fromMaybe "./.foreign" $ foreignDir ds
        , logLevel
            = if debugInfo ds then Debug else Message
        }
    , counter              = 2
    , adtResolved          = empty
    , adtNamed             = empty
    , dataTypes            = empty
    , stringTable          = empty
    , adts                 = emptyADTs
    , mainUnique           = mainUnique
    , mainArgUnique        = mainArgUnique
    , idrisTypesSTyCon     = Nothing
    , extNameBinders       = empty
    })
