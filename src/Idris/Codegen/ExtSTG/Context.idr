module Idris.Codegen.ExtSTG.Context

import Core.Core
import Libraries.Data.StringMap
import Libraries.Data.IntMap
import Idris.Codegen.ExtSTG.STG


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

public export
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
