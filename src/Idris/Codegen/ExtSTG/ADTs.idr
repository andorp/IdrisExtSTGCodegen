module Idris.Codegen.ExtSTG.ADTs

import Core.TT
import Data.List
import Data.SortedMap
import Data.SortedSet

import Idris.Codegen.ExtSTG.ExtName
import Idris.Codegen.ExtSTG.STG

%hide STG.Name
%default total

{-
In STG there is a distinction between Data Constructors and Type Constructors.
Every type constructor has a Unique ID, and it refers to a list of data constructors
through DataConIDs, which are also Unique IDs.

There are some places when the STyCon information is used:
- Creation of an ADT value (StgConApp refers to DataConId)
- Case expression can refert to STyCon when it is used matching on data constructors.
- ... ?

When STG is generated, STyConID and SDataConIDs need to be created and referenced.

In the STG backend for Idris, there are different data and type constructors that needs to
be compiled to and used as STG Type and Data constructors. These situations are:

Origins of an STyCon and SDataCon
- Idris defined datatype must be translated to STyCon and SDataCon.
  ADTs of any kind are represented as GlobalDef and TCon and DCon
- Idris defined type must be added to the STyCon which represent types as new SDataCon.
  Idris is able to pattern match on types, for that reason the STG backend should be
  able to represent this functionality, one way to represent this is to have a STyCon
  where all the DataCons are the types defined in Idris.
- STyCon and SDataCon defined in other STG modules.
  These became available when we import them via the ExternalTopIds part of the STG module.

There should be a map which stores the different kind of type of data constructors and their
associated UniqueIDs. The different kinds of data definitions should have their own register methods
and lookup methods, but also we should be able to lookup based on the UniqueID (or name too?).
-}

public export
TypeOfTypeDataCon : Type
TypeOfTypeDataCon = SDataConSg

public export
record PrimTypeADTDesc where
  constructor MkPrimTypeADTDesc
  dataConName   : ExtName
  dataConSTG    : SDataConSg
  typeConName   : ExtName
  typeConSTG    : STyCon
  dataConOfType : TypeOfTypeDataCon

export
record ADTs2 where
  constructor MkADTs2
  idrisDt       : SortedMap Name SDataConSg
  idrisTy       : SortedMap Name (STyCon, TypeOfTypeDataCon)
  aliasDt       : SortedMap Name (ExtName, SDataConSg)
  aliasTy       : SortedMap Name (ExtName, STyCon, TypeOfTypeDataCon)
  extDt         : SortedMap ExtName SDataConSg
  extTy         : SortedMap ExtName STyCon
  primType      : SortedMap PrimType PrimTypeADTDesc
  typeOfDataCon : SortedMap DataConIdSg STyCon
  typeOfTypes   : Maybe STyCon

public export
DefinedDataTypes : Type
DefinedDataTypes = List (UnitId, List (ModuleName, List STyCon))

export
definedDataTypes : ADTs2 -> DefinedDataTypes
definedDataTypes adts = SortedMap.toList $ map toList $ mergeMaps [idrisTyMap, aliasTyMap, extTyMap, primTypeMap]
  where
    STyConMap : Type
    STyConMap = SortedMap UnitId (SortedMap ModuleName (List STyCon))

    mergeMaps : List STyConMap -> STyConMap
    mergeMaps = foldl (mergeWith (mergeWith (++))) empty

    idrisTyMap : STyConMap
    idrisTyMap
      = singleton (MkUnitId MAIN_UNIT)
      $ singleton (MkModuleName MAIN_MODULE)
      $ (map (\(_, (stycon, _)) => stycon) (SortedMap.toList adts.idrisTy)) ++
        (foldMap singleton adts.typeOfTypes)

    aliasTyMap : STyConMap
    aliasTyMap
      = mergeMaps
      $ map (\(_,extName,stycon,_) => singleton (mkUnitId extName) (singleton (mkModuleName extName) [stycon]))
      $ SortedMap.toList adts.aliasTy

    extTyMap : STyConMap
    extTyMap
      = mergeMaps
      $ map (\(extName, stycon) => singleton (mkUnitId extName) (singleton (mkModuleName extName) [stycon]))
      $ SortedMap.toList adts.extTy

    primTypeMap : STyConMap
    primTypeMap
      = mergeMaps
      $ map (\(_, d) => singleton (mkUnitId d.typeConName) (singleton (mkModuleName d.typeConName) [d.typeConSTG]))
      $ SortedMap.toList adts.primType

export
(.getPrimTypeMap) : ADTs2 -> SortedMap PrimType PrimTypeADTDesc
(.getPrimTypeMap) = (.primType)

export
(.getIdrisTyMap) : ADTs2 -> SortedMap Name (STyCon, TypeOfTypeDataCon)
(.getIdrisTyMap) = (.idrisTy)

export
(.getAliasTyMap) : ADTs2 -> SortedMap Name (ExtName, STyCon, TypeOfTypeDataCon)
(.getAliasTyMap) = (.aliasTy)

export
(.getTypeOfTypes) : ADTs2 -> Maybe STyCon
(.getTypeOfTypes) = (.typeOfTypes)

export
insertIdrisDt : Name -> SDataConSg -> ADTs2 -> Either String ADTs2
insertIdrisDt n d adts = do
  let Nothing = lookup n adts.idrisDt
      | Just _ => Left "\{show n} is already defined."
  Right ({ idrisDt $= insert n d } adts)

export
lookupIdrisDt : Name -> ADTs2 -> Maybe SDataConSg
lookupIdrisDt n adts = lookup n adts.idrisDt

export
insertAliasDt : Name -> ExtName -> SDataConSg -> ADTs2 -> Either String ADTs2
insertAliasDt n e d adts = do
  let Nothing = lookup n adts.aliasDt
      | Just _ => Left "\{show n} is already defined."
  Right ({ aliasDt $= insert n (e,d) } adts)

export
lookupAliasDt : Name -> ADTs2 -> Maybe (ExtName, SDataConSg)
lookupAliasDt n adts = lookup n adts.aliasDt

export
insertIdrisTy : Name -> STyCon -> TypeOfTypeDataCon -> ADTs2 -> Either String ADTs2
insertIdrisTy n s d adts = do
  let Nothing = lookup n adts.idrisTy
      | Just _ => Left "\{show n} is already defined."
  typeOfDataConNew 
    <- map (the (SortedMap DataConIdSg STyCon) . fromList) $ traverse
          (\d => do
            let i : DataConIdSg := identSg d
            let Nothing = lookup i adts.typeOfDataCon
                | Just _ => Left "DataCon is already registered \{show (name (snd d))}"
            Right (i,s))
          (DataCons s)
  Right $
    { typeOfDataCon $= mergeLeft typeOfDataConNew
    , idrisTy $= insert n (s,d)
    } adts

export
lookupIdrisTy : Name -> ADTs2 -> Maybe (STyCon, TypeOfTypeDataCon)
lookupIdrisTy n adts = lookup n adts.idrisTy

export
insertAliasTy : Name -> ExtName -> STyCon -> TypeOfTypeDataCon -> ADTs2 -> Either String ADTs2
insertAliasTy n e s d adts = do
  let Nothing = lookup n adts.aliasTy
      | Just _ => Left "\{show n} is already defined."
  typeOfDataConNew 
    <- map (the (SortedMap DataConIdSg STyCon) . fromList) $ traverse
          (\d => do
            let i : DataConIdSg := identSg d
            let Nothing = lookup i adts.typeOfDataCon
                | Just _ => Left "DataCon is already registered \{show (name (snd d))}"
            Right (i,s))
          (DataCons s)
  Right $
    { typeOfDataCon $= mergeLeft typeOfDataConNew
    , aliasTy $= insert n (e,s,d)
    } adts

export
lookupAliasTy : Name -> ADTs2 -> Maybe (ExtName, STyCon, TypeOfTypeDataCon)
lookupAliasTy n adts = lookup n adts.aliasTy

export
insertTypeOfTypes : STyCon -> ADTs2 -> Either String ADTs2
insertTypeOfTypes t adts = case adts.typeOfTypes of
  Nothing => Right $ { typeOfTypes := Just t } adts
  Just _  => Left "Type of types is already set."

export
lookupSTypeOfDataCon : SDataConSg -> ADTs2 -> Maybe STyCon
lookupSTypeOfDataCon d adts = lookup (identSg d) adts.typeOfDataCon

export
insertPrimTypeADTs2 : PrimType -> ExtName -> SDataConSg -> ExtName -> STyCon -> TypeOfTypeDataCon -> ADTs2 -> Either String ADTs2
insertPrimTypeADTs2 p es s et t d adts = do
  let Nothing = lookup p adts.primType
      | Just _ => Left "PrimType is already registered: \{show p}"
  let Nothing = lookup (identSg s) adts.typeOfDataCon
      | Just _ => Left "Inconsistency: DataCon for \{show p} is already defined in Type Of Data Con map."
  let desc := MkPrimTypeADTDesc
                { dataConName   = es
                , dataConSTG    = s
                , typeConName   = et
                , typeConSTG    = t
                , dataConOfType = d
                }
  Right $
    { typeOfDataCon $= insert (identSg s) t
    , primType $= insert p desc
    } adts     

export
lookupPrimType : PrimType -> ADTs2 -> Maybe PrimTypeADTDesc
lookupPrimType pt adts = lookup pt adts.primType

export
insertExtDataCon : ExtName -> SDataConSg -> ADTs2 -> Either String ADTs2
insertExtDataCon e d adts = do
  let Nothing = lookup e adts.extDt
      | Just _ => Left "\{show e} is already defined."
  Right ({ extDt $= insert e d } adts)

export
lookupExtDataCon : ExtName -> ADTs2 -> Maybe SDataConSg
lookupExtDataCon e adts = lookup e adts.extDt

export
insertExtTyCon : ExtName -> STyCon -> ADTs2 -> Either String ADTs2
insertExtTyCon e s adts = do
  let Nothing = lookup e adts.extTy
      | Just _ => Left "\{show e} is already defined."
  typeOfDataConNew
    <- map (the (SortedMap DataConIdSg STyCon) . fromList) $ traverse
        (\d => do
          let i : DataConIdSg := identSg d
          let Nothing = lookup i adts.typeOfDataCon
              | Just _ => Left "DataCon is already registered \{show (name (snd d))}"
          Right (i,s))
        (DataCons s)
  Right $
    { typeOfDataCon $= mergeLeft typeOfDataConNew
    , extTy $= insert e s
    } adts

export
lookupExtTyCon : ExtName -> ADTs2 -> Maybe STyCon
lookupExtTyCon e adts = lookup e adts.extTy

primTypeCode : PrimType -> Nat
primTypeCode IntType      = 0
primTypeCode Int8Type     = 1
primTypeCode Int16Type    = 2
primTypeCode Int32Type    = 3
primTypeCode Int64Type    = 4
primTypeCode IntegerType  = 5
primTypeCode Bits8Type    = 6
primTypeCode Bits16Type   = 7
primTypeCode Bits32Type   = 8
primTypeCode Bits64Type   = 9
primTypeCode StringType   = 10
primTypeCode CharType     = 11
primTypeCode DoubleType   = 12
primTypeCode WorldType    = 13

export
primTypeOf : Constant -> PrimType
primTypeOf (I i)      = IntType
primTypeOf (I8 i)     = Int8Type
primTypeOf (I16 i)    = Int16Type
primTypeOf (I32 i)    = Int32Type
primTypeOf (I64 i)    = Int64Type
primTypeOf (BI i)     = IntegerType
primTypeOf (B8 m)     = Bits8Type
primTypeOf (B16 m)    = Bits16Type
primTypeOf (B32 m)    = Bits32Type
primTypeOf (B64 m)    = Bits64Type
primTypeOf (Str str)  = StringType
primTypeOf (Ch c)     = CharType
primTypeOf (Db dbl)   = DoubleType
primTypeOf (PrT pty)  = pty
primTypeOf WorldVal   = WorldType

Ord PrimType where
  compare x y = compare (primTypeCode x) (primTypeCode y)

export
createADTs2 : ADTs2
createADTs2 = MkADTs2
  { idrisDt       = empty
  , idrisTy       = empty
  , aliasDt       = empty
  , aliasTy       = empty
  , extDt         = empty
  , extTy         = empty
  , primType      = empty
  , typeOfDataCon = empty
  , typeOfTypes   = Nothing
  }
