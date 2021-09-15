module Idris.Codegen.ExtSTG.Context

import Core.Core
import Libraries.Data.StringMap
import Libraries.Data.IntMap
import Idris.Codegen.ExtSTG.STG
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

export
data STGCtxt : Type where

export
record STGContext where
  constructor MkSTGContext
  counter       : Int
  typeNamespace : StringMap Unique
  termNamespace : StringMap Unique
  adtResolved   : IntMap    STyCon
  adtNamed      : StringMap STyCon
  dataTypes     : DataTypeMap
  dataIdCons    : DataConIdMap
  tyConIds      : TyConIdMap
  extBinds      : ExtBindMap  
  stringTable   : StringTableMap

export
mkSTGContext : Core (Ref STGCtxt STGContext)
mkSTGContext = newRef STGCtxt (MkSTGContext
  { counter       = 0
  , typeNamespace = empty
  , termNamespace = empty
  , adtResolved   = empty
  , adtNamed      = empty
  , dataTypes     = empty
  , dataIdCons    = empty
  , tyConIds      = empty
  , extBinds      = empty
  , stringTable   = empty
  })

export
Show ExtName where
  show (MkExtName p m f) = "MkExtName " ++ show p ++ show m ++ show f

export
logLine : String -> Core ()
logLine msg = coreLift $ putStrLn msg

export
incCounter : Ref STGCtxt STGContext => Core Int
incCounter = do
  ctx <- get STGCtxt
  put STGCtxt (record { counter $= (+1) } ctx)
  pure ctx.counter

export
lookupTypeNamespace : Ref STGCtxt STGContext => STG.Name -> Core (Maybe Unique)
lookupTypeNamespace n = do
  ctx <- get STGCtxt
  pure (lookup n ctx.typeNamespace)

export
insertTypeNamespace : Ref STGCtxt STGContext => STG.Name -> Unique -> Core ()
insertTypeNamespace n u = do
  logLine "Insert type \{n} \{show u}"
  ctx <- get STGCtxt
  put STGCtxt (record { typeNamespace $= insert n u } ctx)

export
lookupTermNamespace : Ref STGCtxt STGContext => STG.Name -> Core (Maybe Unique)
lookupTermNamespace n = do
  ctx <- get STGCtxt
  pure (lookup n ctx.termNamespace)

export
insertTermNamespace : Ref STGCtxt STGContext => STG.Name -> Unique -> Core ()
insertTermNamespace n u = do
  logLine "Insert name \{n} \{show u}"
  ctx <- get STGCtxt
  put STGCtxt (record { termNamespace $= insert n u } ctx)

export
addDataType : Ref STGCtxt STGContext => UnitId -> ModuleName -> STyCon -> Core ()
addDataType (MkUnitId u) (MkModuleName m) s = do
  c <- get STGCtxt
  put STGCtxt $ 
    record
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
  put STGCtxt (record { adtResolved $= insert r s } ctx)

export
lookupADTNamed : Ref STGCtxt STGContext => String -> Core (Maybe STyCon)
lookupADTNamed n = do
  ctx <- get STGCtxt
  pure (lookup n ctx.adtNamed)

export
insertADTNamed : Ref STGCtxt STGContext => String -> STyCon -> Core ()
insertADTNamed n s = do
  ctx <- get STGCtxt
  put STGCtxt (record { adtNamed $= insert n s } ctx)

export
lookupStringTable : Ref STGCtxt STGContext => String -> Core (Maybe TopBinding)
lookupStringTable s = do
  ctx <- get STGCtxt
  pure (lookup s ctx.stringTable)

export
insertStringTable : Ref STGCtxt STGContext => String -> TopBinding -> Core ()
insertStringTable s t = do
  ctx <- get STGCtxt
  put STGCtxt (record { stringTable $= insert s t } ctx)

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
  put STGCtxt (record { extBinds $= insert s b } ctx)

export
getExtBinds : Ref STGCtxt STGContext => Core ExtBindMap
getExtBinds = do
  ctx <- get STGCtxt
  pure ctx.extBinds
