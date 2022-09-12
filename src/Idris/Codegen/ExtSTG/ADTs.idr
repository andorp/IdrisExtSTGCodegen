module Idris.Codegen.ExtSTG.ADTs

import Data.List
import Data.SortedMap
import Data.SortedSet
import Core.TT
import Idris.Codegen.ExtSTG.STG
import Idris.Codegen.ExtSTG.ExtName

%hide STG.Name

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
record IdrisTyUnique where
  constructor MkIdrisTyUnique
  tyUnique    : Unique
  tyADTUnique : Unique

public export
data ADTInfo
  = IdrisTyCon   Name   IdrisTyUnique
  | IdrisDtCon   Name   Unique
  | HaskellTyCon ExtName Unique
  | HaskellDtCon ExtName Unique

dtCon : ADTInfo -> Maybe ADTInfo
dtCon (IdrisTyCon n x) = Nothing
dtCon (IdrisDtCon n x) = Just (IdrisDtCon n x)
dtCon (HaskellTyCon x y) = Nothing
dtCon (HaskellDtCon x y) = Just (HaskellDtCon x y)

tyCon : ADTInfo -> Maybe ADTInfo
tyCon (IdrisTyCon n x) = Just (IdrisTyCon n x)
tyCon (IdrisDtCon n x) = Nothing
tyCon (HaskellTyCon x y) = Just (HaskellTyCon x y)
tyCon (HaskellDtCon x y) = Nothing

export
idrisUnique : ADTInfo -> Either String Unique
idrisUnique (IdrisTyCon n (MkIdrisTyUnique tyUnique tyADTUnique)) = Right tyUnique
idrisUnique (IdrisDtCon n x)              = Right x
idrisUnique (HaskellTyCon x y)            = Left "Found Haskell TyCon instead of Idris one."
idrisUnique (HaskellDtCon x y)            = Left "Found Haskell DataCon instead of Idris one."

export
haskellUnique : ADTInfo -> Either String Unique
haskellUnique (IdrisTyCon n x)              = Left "Found Idris TyCon instead of Haskell one."
haskellUnique (IdrisDtCon n x)              = Left "Found Idris DataCon instead of Haskell one."
haskellUnique (HaskellTyCon x y)            = Right y
haskellUnique (HaskellDtCon x y)            = Right y

export
record ADTs where
  constructor MkADTs
  uniqueToADTInfo  : SortedMap Unique ADTInfo
  dataConToTyCon   : SortedMap Unique Unique
  idrisDtNames     : SortedMap Name Unique
  haskellDtNames   : SortedMap ExtName Unique
  idrisTyNames     : SortedMap Name IdrisTyUnique
  idrisTyDNames    : SortedSet Unique
  haskellTyNames   : SortedMap ExtName Unique
  idrisSTGDataCon  : SortedMap Unique SDataConSg
  idrisSTGTyCon    : SortedMap Unique STyCon

export
statistics : ADTs -> String
statistics adts =
  """
  ADT statistics:
    uniqueToADTInfo  : \{show (length (SortedMap.toList adts.uniqueToADTInfo))}
    dataConToTyCon   : \{show (length (SortedMap.toList adts.dataConToTyCon))}
    idrisDtNames     : \{show (length (SortedMap.toList adts.idrisDtNames))}
    haskellDtNames   : \{show (length (SortedMap.toList adts.haskellDtNames))}
    idrisTyNames     : \{show (length (SortedMap.toList adts.idrisTyNames))}
    idrisTyDNames    : \{show (length (SortedSet.toList adts.idrisTyDNames))}
    haskellTyNames   : \{show (length (SortedMap.toList adts.haskellTyNames))}
    idrisSTGDataCon  : \{show (length (SortedMap.toList adts.idrisSTGDataCon))}
    idrisSTGTyCon    : \{show (length (SortedMap.toList adts.idrisSTGTyCon))}
  """

{-
Invariants:
- Every unique in idrisDtNames must be present in unqiueToADTInfo and it has to be IdrisDtCon
- Every datacon must have an associated tycon in dataConToTyCon
- Every Unique in haskellDtNames must be present in uniqueToADTInfo and it has to be HaskellDtCon
- Every ExtName defined in haskellDtToIdris must be defined in haskellDtName
- Every Unique defined in idrisTyNames must be present in uniqueToADTInfo
- Every Unique defined in idrisTyDNames must be presetn in uniqueToADTInfo
- Every unique in haskellTyNames must be present in uniqueToADTInfo
- Every Name in haskellTyToIdris must be present in idrisTyNames
- Every Unique in idrisSTGDataCon must be present in uniqueToADTInfo and should have IdrisDtCon or HaskellDtCon info.
- Every Unique in idrisSTGTyCon must be present in uniqueToADTInfo and should have IdrisTyCon or HaskellTyCon info.
-}

export
registerSDataCon : ADTs -> SDataConSg -> Either String ADTs
registerSDataCon adts datacon = do
  let u = dataConUnique (ident (snd datacon))
  let Just adtInfo0 = lookup u adts.uniqueToADTInfo
      | Nothing => Left "Unique \{show u} of SDataCon \{show (name (snd datacon))} is not registered. "
  let Just adtInfo = dtCon adtInfo0      
      | Nothing => Left "Not a data constructor for \{show u}"
  Right $ { idrisSTGDataCon $= insert u datacon } adts

export
registerSTyCon : ADTs -> STyCon -> Either String ADTs
registerSTyCon adts stycon = do
  let u = tyConUnique (Id stycon)
  let Just adtInfo0 = lookup u adts.uniqueToADTInfo
      | Nothing => Left "Unique \{show u} of STyCon \{show (Name stycon)} is not registered."
  let Just adtInfo = tyCon adtInfo0
      | Nothing => Left "Not a type constructor for \{show u}"
  adts' <- foldlM registerSDataCon adts (DataCons stycon)
  Right $ { idrisSTGTyCon $= insert u stycon } adts'

export
lookupSTGDataCon : ADTs -> Unique -> Maybe SDataConSg
lookupSTGDataCon adts u = lookup u adts.idrisSTGDataCon

export
lookupSTGTyCon : ADTs -> Unique -> Maybe STyCon
lookupSTGTyCon adts u = lookup u adts.idrisSTGTyCon

export
emptyADTs : ADTs
emptyADTs = MkADTs
  { uniqueToADTInfo     = empty
  , dataConToTyCon      = empty
  , idrisDtNames        = empty
  , idrisTyNames        = empty
  , idrisTyDNames       = empty
  , haskellDtNames      = empty
  , haskellTyNames      = empty
  , idrisSTGDataCon     = empty
  , idrisSTGTyCon       = empty
  }

-- Data constructor related operations.

export
lookupIdrisDtName : ADTs -> Name -> Either String (Maybe ADTInfo)
lookupIdrisDtName adts iname = do
  let Just u = lookup iname adts.idrisDtNames
      | Nothing => Right Nothing
  let Just i = lookup u adts.uniqueToADTInfo
      | Nothing => Left "Idris data constructor does not have registered unique."
  Right (Just i)

export
lookupIdrisSTGDataCon : ADTs -> Unique -> Maybe SDataConSg
lookupIdrisSTGDataCon adts u = lookup u adts.idrisSTGDataCon

export
insertIdrisDtName : ADTs -> Name -> Unique -> Either String ADTs
insertIdrisDtName adts iname un = do
  let Nothing = lookup un adts.uniqueToADTInfo
      | Just i => Left "Unique is already registered \{show iname} \{show un}."
  case lookup iname adts.idrisDtNames of
    Nothing => Right ()
    Just u =>
      if u == un
        then Right ()
        else Left "Idris name is meant to be registered with different \{show iname} new \{show un} old \{show u}."
  Right $
    { uniqueToADTInfo $= insert un (IdrisDtCon iname un)
    , idrisDtNames    $= insert iname un
    } adts

export
lookupHaskellDtName : ADTs -> ExtName -> Either String (Maybe ADTInfo)
lookupHaskellDtName adts hname = do
  let Just u = lookup hname adts.haskellDtNames
      | Nothing => Right Nothing
  let Just h = lookup u adts.uniqueToADTInfo
      | Nothing => Left "Haskell data constructor does not have a registered unique."
  Right (Just h)

export
insertHaskellDtName : ADTs -> ExtName -> Unique -> Either String ADTs
insertHaskellDtName adts hname un = do
  let Nothing = lookup un adts.uniqueToADTInfo
      | Just i => Left "Unique is already registered."
  case lookup hname adts.haskellDtNames of
    Nothing => Right ()
    Just u =>
      if u == un
        then Right ()
        else Left "Haskell name is meant to be registered with different \{show hname} new \{show un} old \{show u}."
  Right $
    { uniqueToADTInfo $= insert un (HaskellDtCon hname un)
    , haskellDtNames  $= insert hname un
    } adts

-- Type constrictor related operations.

export
lookupIdrisTyName : ADTs -> Name -> Either String (Maybe ADTInfo)
lookupIdrisTyName adts iname = do
  let Just (MkIdrisTyUnique u _) = lookup iname adts.idrisTyNames
      | Nothing => Right Nothing
  let Just i = lookup u adts.uniqueToADTInfo
      | Nothing => Left "Idris data constructor does not have registered unique."
  Right (Just i)

export
insertIdrisTyName : ADTs -> Name -> IdrisTyUnique -> Either String ADTs
insertIdrisTyName adts iname un@(MkIdrisTyUnique ut ud) = do
  let Nothing = lookup ut adts.uniqueToADTInfo
      | Just i => Left "Unique is already registered."
  let Nothing = lookup iname adts.idrisTyNames
      | Just u => Left "Name is already registered."
  let False = contains ud adts.idrisTyDNames
      | True => Left "Name is already registered in type datacon set."
  Right $
    { uniqueToADTInfo $= insert ut (IdrisTyCon iname un)
    , idrisTyNames    $= insert iname un
    , idrisTyDNames   $= insert ud
    } adts

export
lookupHaskellTyName : ADTs -> ExtName -> Either String (Maybe ADTInfo)
lookupHaskellTyName adts hname = do
  let Just u = lookup hname adts.haskellTyNames
      | Nothing => Right Nothing
  let Just h = lookup u adts.uniqueToADTInfo
      | Nothing => Left "Haskell data constructor does not have a registered unique."
  Right (Just h)

export
insertHaskellTyName : ADTs -> ExtName -> Unique -> Either String ADTs
insertHaskellTyName adts hname un = do
  let Nothing = lookup un adts.uniqueToADTInfo
      | Just i => Left "Unique is already registered."
  let Nothing = lookup hname adts.haskellTyNames
      | Just u => Left "Name is already registered."
  Right $
    { uniqueToADTInfo $= insert un (HaskellTyCon hname un)
    , haskellTyNames  $= insert hname un
    } adts

-- Data constructor and type constructor registration

export
registerIdrisDataConToTyCon : ADTs -> Name -> Name -> Either String ADTs
registerIdrisDataConToTyCon adts dname tname = do
  let Just ud = lookup dname adts.idrisDtNames
      | Nothing => Left "Idris datacon name is not registered."
  let Just (MkIdrisTyUnique ut _) = lookup tname adts.idrisTyNames
      | Nothing => Left "Idris typecon name is not registered."
  case lookup ut adts.dataConToTyCon of
    Just ut2 => case ut == ut2 of
      True  => Right adts
      False => Left "Idris datacon is already registered with a different tycon."
    Nothing => Right $
      { dataConToTyCon $= insert ud ut
      } adts

export
registerHaskellDataConToTyCon : ADTs -> ExtName -> ExtName -> Either String ADTs
registerHaskellDataConToTyCon adts dname tname = do
  let Just ud = lookup dname adts.haskellDtNames
      | Nothing => Left "Haskell datacon name is not registered."
  let Just ut = lookup tname adts.haskellTyNames
      | Nothing => Left "Haskell typecon name is not registered."
  case lookup ut adts.dataConToTyCon of
    Just ut2 => case ut == ut2 of
      True  => Right adts
      False => Left "Idris datacon is already registered with a different tycon."
    Nothing => Right $
      { dataConToTyCon $= insert ud ut
      } adts
  
export
lookupDataConUniqueToTyCon : ADTs -> Unique -> Either String Unique
lookupDataConUniqueToTyCon adts u = do
  let Just u = lookup u adts.dataConToTyCon
      | Nothing => Left "No TyCon unqiue is found for \{show u}"
  Right u
