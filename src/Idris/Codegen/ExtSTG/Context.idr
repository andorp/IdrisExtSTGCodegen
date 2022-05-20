module Idris.Codegen.ExtSTG.Context

import Core.Core
import Core.Context
import Core.Options 
import Libraries.Data.StringMap
import Libraries.Data.IntMap
import Idris.Codegen.ExtSTG.STG
import Idris.Codegen.ExtSTG.Configuration
import Core.Context.Context


public export
DataTypeMap : Type
DataTypeMap = StringMap {-UnitId-} (StringMap {-ModuleName-} (List STyCon))

public export
DataConIdMap : Type
DataConIdMap = StringMap {-Unique-} (List SDataConSg) -- Should be unique

public export
TyConIdMap : Type
TyConIdMap = StringMap {-Unique-} (List STyCon) -- Should be unique

||| Name for module dependency with fully qualified name.
public export
data ExtName = MkExtName String (List String) String

public export
ExtBindMap : Type
ExtBindMap = StringMap (ExtName, SBinderSg)

public export
StringTableMap : Type
StringTableMap = StringMap TopBinding

public export
HaskellNameMap : Type
HaskellNameMap = StringMap {-UnitId-} (StringMap {-ModulePath-} (StringMap {-name-} Unique))

||| Insert a unique for the name, if its already there, ignore the new one.
insert : ExtName -> Unique -> HaskellNameMap -> HaskellNameMap
insert (MkExtName u p n) x m = mergeWith (mergeWith mergeLeft) m (singleton u (singleton path (singleton n x)))
  where
    path : String
    path = concat (intersperse "." p)

lookup : ExtName -> HaskellNameMap -> Maybe Unique
lookup (MkExtName u p n) m = do
  um <- lookup u m
  nm <- lookup (concat (intersperse "." p)) um
  lookup n nm

export
data STGCtxt : Type where

export
record STGContext where
  constructor MkSTGContext
  configuration        : Configuration
  counter              : Int
  idrisTypeNamespace   : StringMap Unique
  idrisTermNamespace   : StringMap Unique
  haskellTypeNamespace : HaskellNameMap
  haskellTermNamespace : HaskellNameMap
  adtResolved          : IntMap    STyCon
  adtNamed             : StringMap STyCon
  dataTypes            : DataTypeMap
  dataIdCons           : DataConIdMap
  tyConIds             : TyConIdMap
  extBinds             : ExtBindMap  
  stringTable          : StringTableMap

export
mkSTGContext
  :  Ref Ctxt Defs
  => Core (Ref STGCtxt STGContext)
mkSTGContext = do
  ds <- getDirectives (Other "stg")
  let loglevel = if elem "debug-info" ds then Debug else Message
  newRef STGCtxt (MkSTGContext
    { configuration = MkConfiguration
        { foreignDirectory  = "./.foreign"
        , logLevel          = loglevel
        }
    , counter              = 0
    , idrisTypeNamespace   = empty
    , idrisTermNamespace   = empty
    , haskellTypeNamespace = empty
    , haskellTermNamespace = empty
    , adtResolved          = empty
    , adtNamed             = empty
    , dataTypes            = empty
    , dataIdCons           = empty
    , tyConIds             = empty
    , extBinds             = empty
    , stringTable          = empty
    })

export
Show ExtName where
  show (MkExtName p m f) = "MkExtName " ++ show p ++ show m ++ show f

export
getConfiguration : Ref STGCtxt STGContext => Core Configuration
getConfiguration = map (.configuration) (get STGCtxt)

export
logLine : Ref STGCtxt STGContext => Configuration.LogLevel -> String -> Core ()
logLine levelOfMsg msg = do
  logLvl <- map (\c => c.configuration.logLevel) $ get STGCtxt
  when (logLvl <= levelOfMsg) $ coreLift $ putStrLn msg

export
incCounter : Ref STGCtxt STGContext => Core Int
incCounter = do
  ctx <- get STGCtxt
  put STGCtxt ({counter $= (+1)} ctx)
  pure ctx.counter

export
lookupIdrisTypeNamespace : Ref STGCtxt STGContext => STG.Name -> Core (Maybe Unique)
lookupIdrisTypeNamespace n = do
  ctx <- get STGCtxt
  pure (lookup n ctx.idrisTypeNamespace)

export
insertIdrisTypeNamespace : Ref STGCtxt STGContext => STG.Name -> Unique -> Core ()
insertIdrisTypeNamespace n u = do
  logLine Debug "Insert type \{n} \{show u}"
  ctx <- get STGCtxt
  put STGCtxt ({idrisTypeNamespace $= insert n u} ctx)

export
lookupHaskellTypeNamespace : Ref STGCtxt STGContext => ExtName -> Core (Maybe Unique)
lookupHaskellTypeNamespace e = do
  ctx <- get STGCtxt
  pure (lookup e ctx.haskellTypeNamespace)

export
insertHaskellTypeNamespace : Ref STGCtxt STGContext => ExtName -> Unique -> Core ()
insertHaskellTypeNamespace e u = do
  logLine Debug "Insert Haskell type \{show e} \{show u}"
  ctx <- get STGCtxt
  put STGCtxt ({haskellTypeNamespace $= insert e u} ctx)

export
lookupIdrisTermNamespace : Ref STGCtxt STGContext => STG.Name -> Core (Maybe Unique)
lookupIdrisTermNamespace n = do
  ctx <- get STGCtxt
  pure (lookup n ctx.idrisTermNamespace)

export
insertIdrisTermNamespace : Ref STGCtxt STGContext => STG.Name -> Unique -> Core ()
insertIdrisTermNamespace n u = do
  logLine Debug "Insert name \{n} \{show u}"
  ctx <- get STGCtxt
  put STGCtxt ({ idrisTermNamespace $= insert n u } ctx)

export
lookupHaskellTermNamespace : Ref STGCtxt STGContext => ExtName -> Core (Maybe Unique)
lookupHaskellTermNamespace e = do
  ctx <- get STGCtxt
  pure (lookup e ctx.haskellTermNamespace)

export
insertHaskellTermNamespace : Ref STGCtxt STGContext => ExtName -> Unique -> Core ()
insertHaskellTermNamespace e u = do
  logLine Debug "Insert Haskell name \{show e} \{show u}"
  ctx <- get STGCtxt
  put STGCtxt ({haskellTermNamespace $= insert e u} ctx)

export
addDataType : Ref STGCtxt STGContext => UnitId -> ModuleName -> STyCon -> Core ()
addDataType (MkUnitId u) (MkModuleName m) s = do
  c <- get STGCtxt
  put STGCtxt $ 
      { dataTypes  $= merge (singleton u (singleton m [s]))
      , dataIdCons $= \dc => foldl merge dc $ map (\d => singleton (show (dataConUnique (ident (snd d)))) [d]) s.DataCons
      , tyConIds   $= merge (singleton (show (tyConUnique s.Id)) [s])
      } c

export
getDataCons : Ref STGCtxt STGContext => Unique -> Core (Maybe (List SDataConSg))
getDataCons u = do
  ctx <- get STGCtxt
  pure (lookup (show u) ctx.dataIdCons)

export
getUniqueDataCon : Ref STGCtxt STGContext => Unique -> Core SDataConSg
getUniqueDataCon u = do
  case !(getDataCons u) of
    Nothing   => coreFail $ InternalError "getUniqueDataCon: Couldn't find Binder for \{show u} ."
    Just []   => coreFail $ InternalError "getUniqueDataCon: Couldn't find Binder for \{show u} . Empty list, this should not have happened."
    Just [d]  => pure d
    Just ds   => coreFail $ InternalError "getUniqueDataCon: Found more than one Binders for \{show u} ."

export
getTyConIds : Ref STGCtxt STGContext => Unique -> Core (Maybe (List STyCon))
getTyConIds u = do
  ctx <- get STGCtxt
  pure (lookup (show u) ctx.tyConIds)

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
lookupADTNamed : Ref STGCtxt STGContext => String -> Core (Maybe STyCon)
lookupADTNamed n = do
  ctx <- get STGCtxt
  pure (lookup n ctx.adtNamed)

export
insertADTNamed : Ref STGCtxt STGContext => String -> STyCon -> Core ()
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
lookupExtBinds : Ref STGCtxt STGContext => String -> Core (Maybe (ExtName, SBinderSg))
lookupExtBinds x = do
  ctx <- get STGCtxt
  pure (lookup x ctx.extBinds)

export
insertExtBinds : Ref STGCtxt STGContext => String -> (ExtName, SBinderSg) -> Core ()
insertExtBinds s b = do
  ctx <- get STGCtxt
  put STGCtxt ({ extBinds $= insert s b } ctx)

export
getExtBinds : Ref STGCtxt STGContext => Core ExtBindMap
getExtBinds = do
  ctx <- get STGCtxt
  pure ctx.extBinds
