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

public export
StringTableMap : Type
StringTableMap = StringMap TopBinding

export
data STGCtxt : Type where

public export
record STGContext where
  constructor MkSTGContext
  configuration : Configuration
  counter       : Int
  stringTable   : StringTableMap
  adts          : ADTs
  binders       : Binders
  ffiFiles      : FFIFiles
  adtAliasFiles : ADTAliasFiles

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

export
getExtBinds : Ref STGCtxt STGContext => Core (List (ExtName, SBinderSg))
getExtBinds = do
  logLine Debug "getExtBinds"
  ctx <- get STGCtxt
  pure $ getExtBinders ctx.binders

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
  ds <- learnDirectives
  newRef STGCtxt (MkSTGContext
    { configuration = MkConfiguration
        { foreignDirectory
            = fromMaybe "./.foreign" $ foreignDir ds
        , logLevel
            = if debugInfo ds then Debug else Message
        }
    , counter              = 0
    , stringTable          = empty
    , adts                 = createADTs2
    , binders              = createBinders
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
      let Right adts  = insertIdrisDt fn dataCon ctx.adts 
          | Left err => coreFail $ InternalError "insertDTCon: \{err}"
      modifySTGCtxt ({ adts  := adts  })
    AliasedName n en a => do
      logLine Debug "insertDTCon: \{show (n,en)} \{show (identSg dataCon)}"
      let Right adts  = insertAliasDt fn en dataCon ctx.adts 
          | Left err => coreFail $ InternalError "insertDTCon: \{err}"
      modifySTGCtxt ({ adts  := adts  })
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
      let Just datacon = lookupIdrisDt n ctx.adts 
          | Nothing => coreFail $ InternalError "lookupDTCon: No registered datacon found for \{show fn}"
      pure datacon
    AliasedName n en a => do
      let Just (extName, datacon) = lookupAliasDt n ctx.adts 
          | Nothing => coreFail $ InternalError "lookupDTCon: No registered datacon found for \{show fn}"
      pure datacon

  let Just stycon = lookupSTypeOfDataCon datacon ctx.adts 
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
      let Right adts  = insertIdrisTy fn stycon typeConDCon ctx.adts 
          | Left err => coreFail $ InternalError "insertTYCon: \{err}"
      modifySTGCtxt ({ adts  := adts  })
      pure stycon
    Just ex => do
      stycon <- createSTyCon (Right ex) ds span
      logLine Debug "insertTYCon: \{show ex} \{show (Id stycon,map identSg (DataCons stycon))}"
      let Right adts  = insertAliasTy fn ex stycon typeConDCon ctx.adts 
          | Left err => coreFail $ InternalError "insertTYCon: \{err}"
      modifySTGCtxt ({ adts  := adts  })
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
      let Just d = lookupPrimType pt ctx.adts 
          | Nothing => coreFail $ InternalError "lookupTYCon: No STyCon found for \{show pt}"
      pure (d.typeConSTG, d.dataConOfType)
    Nothing => case !(typeExtName fn) of
      Nothing => do
        let Just td = lookupIdrisTy fn ctx.adts 
            | Nothing => coreFail $ InternalError "lookupTYCon: No STyCon found for \{show fn}"
        pure td
      Just _ => do
        let Just (_, td) = lookupAliasTy fn ctx.adts 
            | Nothing => coreFail $ InternalError "lookupTYCon: No STyCon found for \{show fn}"
        pure td

export
createTypeOfTypes : Ref STGCtxt STGContext => Core (UnitId, ModuleName, STyCon)
createTypeOfTypes = do
  ctx <- get STGCtxt
  let primTypeToTDataCons : List TypeOfTypeDataCon = map (dataConOfType . snd) $ SortedMap.toList $ ctx.adts .getPrimTypeMap
  let idrisTyConDataCons : List TypeOfTypeDataCon = map snd $ toList $ ctx.adts .getIdrisTyMap
  let aliasTyConDataCons : List TypeOfTypeDataCon = map (snd . snd) $ toList $ ctx.adts .getAliasTyMap
  let datacons = primTypeToTDataCons ++ idrisTyConDataCons ++ aliasTyConDataCons
  let stycon : STyCon := MkSTyCon
        { Name = "::.Type.Of.Type"
        , Id = MkTyConId !(mkUnique 'w')
        , DataCons = datacons
        , DefLoc = SsUnhelpfulSpan "TypeOfTypes"
        }
  let Right adts  = insertTypeOfTypes stycon ctx.adts 
      | Left err => coreFail $ InternalError "createTypeOfTypes: \{err}"
  modifySTGCtxt ({ adts  := adts  })
  pure (MkUnitId MAIN_UNIT, MkModuleName MAIN_MODULE, stycon)

export
getTypeOfTypes : Ref STGCtxt STGContext => Core STyCon
getTypeOfTypes = do
  ctx <- get STGCtxt
  let Just stycon = ctx.adts .getTypeOfTypes
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
  let Right adts  = insertPrimTypeADTs2 pt constExt datacon tyExt stycon dataconToT ctx.adts 
      | Left err => coreFail $ InternalError "discoverPrimType \{show pt} \{err}"
  modifySTGCtxt ({ adts  := adts  })

export
lookupPrimType : Ref STGCtxt STGContext => PrimType -> Core PrimTypeADTDesc
lookupPrimType p = do
  logLine Debug "lookupPrimType: \{show p}"
  ctx <- get STGCtxt
  let Just info = lookupPrimType p ctx.adts 
      | Nothing => coreFail $ InternalError "lookupPrimType: \{show p} is not registered."
  pure info

export
insertExtNameDTCon : Ref STGCtxt STGContext => ExtName -> SDataConSg -> Core ()
insertExtNameDTCon e d = do
  logLine Debug "insertExtNameDTCon: \{show e} \{show (identSg d)}"
  ctx <- get STGCtxt
  let Right adts  = insertExtDataCon e d ctx.adts 
      | Left err => coreFail $ InternalError "insertExtNameDTCon: \{err}"
  put STGCtxt ({ adts  := adts  } ctx)

export
lookupExtNameDTCon : Ref STGCtxt STGContext => ExtName -> Core SDataConSg
lookupExtNameDTCon e = do
  logLine Debug "lookupExtNameDTCon: \{show e}"
  ctx <- get STGCtxt
  let Just d = lookupExtDataCon e ctx.adts 
      | Nothing => coreFail $ InternalError "lookupExtNameDTCon: \{show e} is not registered."
  pure d

export
insertExtNameTyCon : Ref STGCtxt STGContext => ExtName -> STyCon -> Core ()
insertExtNameTyCon e s = do
  logLine Debug "insertExtNameTyCon: \{show e} \{show (Id s, map identSg (DataCons s))}"
  ctx <- get STGCtxt
  let Right adts  = insertExtTyCon e s ctx.adts 
      | Left err => coreFail $ InternalError "insertExtNameTyCon: \{err}"
  put STGCtxt ({ adts  := adts  } ctx)

export
lookupExtNameTyCon : Ref STGCtxt STGContext => ExtName -> Core STyCon
lookupExtNameTyCon e = do
  logLine Debug "lookupExtNameTyCon: \{show e}"
  ctx <- get STGCtxt
  let Just s = lookupExtTyCon e ctx.adts 
      | Nothing => coreFail $ InternalError "lookupExtNameTyCon: \{show e} is not registered."
  pure s

export
getDefinedDataTypes : Ref STGCtxt STGContext => Core DefinedDataTypes
getDefinedDataTypes = do
  ctx <- get STGCtxt
  pure $ definedDataTypes ctx.adts 

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

export
defineSoloDataType : Ref STGCtxt STGContext => Core ()
defineSoloDataType = do
  datacon <- createExtSDataCon soloExtName (UnboxedTupleCon 1) (SsUnhelpfulSpan "Solo")
  stycon <- createSTyCon (Right soloExtName) [datacon] (SsUnhelpfulSpan "Solo")
  insertExtNameDTCon soloExtName datacon
  insertExtNameTyCon soloExtName stycon
