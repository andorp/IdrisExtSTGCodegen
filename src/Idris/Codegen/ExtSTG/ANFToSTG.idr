module Idris.Codegen.ExtSTG.ANFToSTG

import public Idris.Codegen.ExtSTG.Core

import Compiler.ANF
import Core.Context
import Core.Core
import Core.TT
import Data.IOArray
import Data.IntMap
import Data.List
import Data.StringMap
import Data.Strings
import Data.Vect
import Idris.Codegen.ExtSTG.STG
import Idris.Codegen.ExtSTG.String
import Prelude
import Idris.Codegen.ExtSTG.StringTable
import Idris.Codegen.ExtSTG.PrimOp
import Idris.Codegen.ExtSTG.Erased
import Idris.Codegen.ExtSTG.ADTMap
import Idris.Codegen.ExtSTG.Foreign
import Idris.Codegen.ExtSTG.ExternalTopIds

import Debug.Trace

tracef : (Show b) => (a -> b) -> a -> a
tracef f a = trace (show (f a)) a

{-
Implementation notes

 * Idris primitive types are represented as Boxed values in STG. They are unboxed when they are applied to
   primitive operations, because primitive operations in STG work unboxed values. And they are unboxed
   when Idris' case expression matches on primitive values.
 * Handle of String literals from Idris to STG is different. In Idris ANF we can case match on string literals,
   but in STG it is not possible. In this case we have to generate a ifthenelse like embedded case matching
   for STG where the case scrutinee is a primitive operation to compare the value from the literal found
   in the case alternatives. Which introduces the next problem, string literals in STG are top-binders and
   they are represented as Addr# by the CMM. Having LitString with String is very misleading.
   So the points here:
   - When the code generator finds a String matcing case, it creates a top level bindings for String,
     which is considered as Addr#
   - When the Strings values are created, eg via readLine, they should do ByteArray allocations, which
     will be handled bye the garbage collector. When implementing a String Equality check (we must know
     the representation of the string OR just default back to Addr# ???)
     This step needs to be further checking TODO
   - String comparism primitive cStrCmp must be implemented in STG to solve this problem, this should a top
     level binding, whith a recursive function cStrCmp function.
   - The case chain which represents the ifthenelse chain should use the cStrCmp function.
 * Datatypes in STG and Idris ANF's IR are similar.
   - In STG datatypes are created by the STyCon which associates a type name with a Unique
     identifier and the list of data constructors SDataCon with their unique IDs too.
   - In ANF the data constructors and type constructors occupy the same namespace and the ones which
     are used only appear in the structure of the ANF program. This is not enough information for
     the STG backend to generate STG data type definitions.
   - Because there is only partial information in the ANF program, there is a need to look into
     the GlobalDef Context which is part of the Core compiler abstraction via `Ref Ctxt Defs`.
     From that source of information we have to remap type names and constructor names,
     which is currently done by a hack. All this detail can be found in the:
     TConsAndDCons namespace
   - TODO: Some words about matching data and type constructors
 * In ANF Erased values occupy arguments and they meant to be special values. In STG we create a
   simple data type with one constructor: Erased = Erased. During the ANF compilation when Erased in
   encuntered the corresponding value constructor in STG is referred.
 * TODO: Integer and String and AnyPtr
 * ...

TODOs
[+] Remove (Name, Name, Name) parameter from StgApp
[+] Add FC information to binders
[+] Write DataCon -> TypeCon association
[+] Separate STG Type and Term namespaces
[+] Fix mkDataConId
[+] Implement primitive values marshalled to the right GHC boxed primitives
[+] Implement Erased values, erased variables
[+] Generate StringTable for String literals
[.] Handle Data/Type constructors
    [+] Create StgConApp for data constructors
    [ ] Represent Type Constructors in unified datatype
    [ ] Match Type constructors in case expressions
[ ] Implement Crash primitive
[ ] Handle primitive case matches accordingly
[ ] Generate STG main entry
[.] Handle String matches with ifthenelse chains, using stringEq primop from STG
    [ ] Create a test program which reads from input.
[.] Implement primitive operations
    [ ] Implement casting
    [ ] ShiftL/ShiftR for Word needs a wrapper: Differences in parameters at STG and ANF side.
    [ ] DoubleFloor/Ceiling also needs a wrapper function as in STG the result is an Integer.
    [ ] Check if the BelieveMe operation is correct in STG
[*] Implement String handling STG code.
[ ] FFI calls AExtPrim
    [ ] Create a test program which FFI calls into a library.
    [ ] Foreign definitions should be looked up from a file, which can be modified by the user.
[ ] Module compilation
[ ] FIX: Use StgCase instead of StgLet, otherwise strict semantics breaks.
[ ] ...
-}


||| Define an STG data type with one constructor.
definePrimitiveDataType
  :  UniqueMapRef
  => Ref Counter Int
  => DataTypeMapRef
  => (String, String, Constant)
  -> Core ()
definePrimitiveDataType (u, m, StringType) = do
  logLine "Defining String datatype."
  -- defineDataType (MkUnitId u) (MkModuleName m) !IdrisString -- TODO
definePrimitiveDataType (u, m, c) = do
  t <- typeConNameForConstant c
  n <- dataConNameForConstant c
  d <- createSTyCon (t, SsUnhelpfulSpan t) [(n, AlgDataCon !(constantToPrimRep c), SsUnhelpfulSpan n)]
  defineDataType (MkUnitId u) (MkModuleName m) d

||| Create the primitive types section in the STG module.
|||
||| Idris primitive types are represented as boxed values in STG, with a datatype with one constructor.
||| Eg: data IdrInt = IdrInt #IntRep
definePrimitiveDataTypes
  :  UniqueMapRef
  => Ref Counter Int
  => DataTypeMapRef
  => Core ()
definePrimitiveDataTypes = traverse_ definePrimitiveDataType
 [ ("ghc-prim", "GHC.Types",  IntType)
 , (MAIN_UNIT,  MAIN_MODULE,  IntegerType) -- TODO: This is bad, GMP Integer is needed here
 , ("base",     "GHC.Word",   Bits8Type)
 , ("base",     "GHC.Word",   Bits16Type)
 , ("base",     "GHC.Word",   Bits32Type)
 , ("base",     "GHC.Word",   Bits64Type)
 , ("ghc-prim", "GHC.Types",  CharType)
 , ("ghc-prim", "GHC.Types",  DoubleType)
-- , (MAIN_UNIT,  MAIN_MODULE,  StringType)
 , (MAIN_UNIT,  MAIN_MODULE,  WorldType)
 ]

-- TODO: Create ifthenelse chain for String literals
||| Compile constant for case alternative.
compileAltConstant : Constant -> Core Lit
compileAltConstant (I i)   = pure $ LitNumber LitNumInt $ cast i
compileAltConstant (BI i)  = pure $ LitNumber LitNumWord i
compileAltConstant (B8 i)  = pure $ LitNumber LitNumWord $ cast i
compileAltConstant (B16 i) = pure $ LitNumber LitNumWord $ cast i
compileAltConstant (B32 i) = pure $ LitNumber LitNumWord $ cast i
compileAltConstant (B64 i) = pure $ LitNumber LitNumWord64 i
compileAltConstant (Ch c)  = pure $ LitChar c
compileAltConstant (Db d)  = pure $ LitDouble d
compileAltConstant c = coreFail $ InternalError $ "compileAltConstant " ++ show c

||| Compile constant for APrimVal, Boxing a value in STG.
compileConstant : Constant -> Core Lit
compileConstant (I i)   = pure $ LitNumber LitNumInt $ cast i
compileConstant (BI i)  = pure $ LitNumber LitNumWord i
compileConstant (B8 i)  = pure $ LitNumber LitNumWord $ cast i
compileConstant (B16 i) = pure $ LitNumber LitNumWord $ cast i
compileConstant (B32 i) = pure $ LitNumber LitNumWord $ cast i
compileConstant (B64 i) = pure $ LitNumber LitNumWord64 i
compileConstant (Ch c)  = pure $ LitChar c
compileConstant (Db d)  = pure $ LitDouble d
compileConstant c = coreFail $ InternalError $ "compileAltConstant " ++ show c


||| Determine the Data constructor for the boxed primitive value.
||| Used in creating PrimVal
|||
||| The name of terms should coincide the ones that are defined in GHC's ecosystem. This
||| would make the transition easier, I hope.
dataConIdForValueConstant : DataTypeMapRef => UniqueMapRef => Ref Counter Int => FC -> Constant -> Core DataConId
dataConIdForValueConstant _ (I _)    = mkDataConIdStr "I#"
dataConIdForValueConstant _ (BI _)   = mkDataConIdStr "GMPInt" -- TODO: This should be GMP int
dataConIdForValueConstant _ (B8 _)   = mkDataConIdStr "W8#"
dataConIdForValueConstant _ (B16 _)  = mkDataConIdStr "W16#"
dataConIdForValueConstant _ (B32 _)  = mkDataConIdStr "W32#"
dataConIdForValueConstant _ (B64 _)  = mkDataConIdStr "W64#"
dataConIdForValueConstant _ (Ch _)   = mkDataConIdStr "C#"
dataConIdForValueConstant _ (Db _)   = mkDataConIdStr "D#"
dataConIdForValueConstant _ WorldVal = mkDataConIdStr "IdrWorld"
dataConIdForValueConstant fc other   = coreFail $ InternalError $ "dataConIdForValueConstant " ++ show other ++ ":" ++ show fc

tyConIdForValueConstant : UniqueMapRef => Ref Counter Int => FC -> Constant -> Core TyConId
tyConIdForValueConstant _ (I _)    = tyConIdForConstant IntType
tyConIdForValueConstant _ (BI _)   = tyConIdForConstant IntegerType
tyConIdForValueConstant _ (B8 _)   = tyConIdForConstant Bits8Type
tyConIdForValueConstant _ (B16 _)  = tyConIdForConstant Bits16Type
tyConIdForValueConstant _ (B32 _)  = tyConIdForConstant Bits32Type
tyConIdForValueConstant _ (B64 _)  = tyConIdForConstant Bits64Type
tyConIdForValueConstant _ (Ch _)   = tyConIdForConstant CharType
tyConIdForValueConstant _ (Db _)   = tyConIdForConstant DoubleType
tyConIdForValueConstant _ WorldVal = tyConIdForConstant WorldType
tyConIdForValueConstant fc other   = coreFail $ InternalError $ "tyConIdForValueConstant " ++ show other ++ ":" ++ show fc

primTypeForValueConstant : UniqueMapRef => Ref Counter Int => FC -> Constant -> Core PrimRep
primTypeForValueConstant _ (I _)    = pure IntRep
primTypeForValueConstant _ (BI _)   = pure IntRep
primTypeForValueConstant _ (B8 _)   = pure Word8Rep
primTypeForValueConstant _ (B16 _)  = pure Word16Rep
primTypeForValueConstant _ (B32 _)  = pure Word32Rep
primTypeForValueConstant _ (B64 _)  = pure Word64Rep
primTypeForValueConstant _ (Ch _)   = pure Word8Rep
primTypeForValueConstant _ (Db _)   = pure DoubleRep
primTypeForValueConstant fc other   = coreFail $ InternalError $ "primTypeForValueConstant " ++ show other ++ ":" ++ show fc

mutual
  compileANF
    : UniqueMapRef => Ref Counter Int => Ref ADTs ADTMap => StringTableRef
    => Ref Ctxt Defs => DataTypeMapRef
    => Core.Name.Name -> ANF
    -> Core Expr
  compileANF funName (AV fc var)
    = pure $ StgApp !(mkBinderIdVar fc funName var) [] stgRepType

  compileANF funToCompile (AAppName fc funToCall args)
    = pure $ StgApp !(mkBinderIdName funToCall)
                    !(traverse (mkStgArg fc funToCompile) args)
                    stgRepType

  compileANF funToCompile (AUnderApp fc funToCall _ args)
    = pure $ StgApp !(mkBinderIdName funToCall)
                    !(traverse (mkStgArg fc funToCompile) args)
                    stgRepType

  compileANF funName (AApp fc closure arg)
    = pure $ StgApp !(mkBinderIdVar fc funName closure)
                    [!(mkStgArg fc funName arg)]
                    stgRepType

  compileANF funName (ALet fc var expr body) = do
    pure
      $ StgCase
          !(compileANF funName expr)
          !(mkSBinderLocal fc funName var)
          PolyAlt -- It could be ForceUnbox, and only AltDefault should be used in Strict Let
          [ MkAlt AltDefault [] !(compileANF funName body) ]

  -- TODO: Implement
  compileANF _ acon@(ACon fc name Nothing args) = do
    -- Types probably will be represented with one STG TyCon and DataCon
    -- for every type.
    coreFail $ InternalError $ "Figure out how to represent a type as a value!"

  compileANF funName acon@(ACon fc name (Just tag) args) = do
    -- Lookup the constructor based on the name.
    -- The tag information is not relevant here.
    -- Args are variables
    pure $ StgConApp !(mkDataConId name) !(traverse (map StgVarArg . mkBinderIdVar fc funName) args) []

  compileANF funName (AOp fc prim args)
    = compilePrimOp fc funName prim args

  -- TODO: Implement
  compileANF _ aext@(AExtPrim _ name args) = do
    logLine $ "To be implemented: " ++ show aext
    pure $ StgLit $ LitString $ "AExtPrim " ++ show name ++ " " ++ show args

  compileANF funName (AConCase fc scrutinee alts mdef) = do
    -- Compute the alt-type
    altType <- do
      -- Lookup the STyCon definition from the alt names
      namesAndTyCons
        <- traverse
            (\(MkAConAlt name tag args body) => map (name,) (lookupTyCon name))
            alts
      -- Check if there is exactly one STyCon definition is found
      [] <- pure $ mapMaybe
                    (\(n,x) => case x of { Nothing => Just n; _ => Nothing})
                    namesAndTyCons
        | nonDefinedConstructors => coreFail $ InternalError $ unlines $
            "Constructors not having type information: " :: map show nonDefinedConstructors
      tyCon <- case mapMaybe snd namesAndTyCons of
                []  => coreFail $ InternalError $ "No type constructor is found for: " ++ show fc
                [t] => pure $ STyCon.Id t
                ts  => case nub (map STyCon.Id ts) of
                         []  => coreFail $ InternalError "Impossible case in compile AConCase"
                         [t] => pure t
                         ts  => coreFail $ InternalError $ "More than TyCon found for: " ++ show fc
      -- Use the STyCon definition in the altType
      pure $ AlgAlt tyCon
    scrutBinder <- mkBinderIdVar fc funName scrutinee
    let stgScrutinee = StgApp scrutBinder [] stgRepType
    caseBinder <- mkFreshSBinderStr LocalScope fc "conCaseBinder"
    stgDefAlt <- maybe
      (pure [])
      (\x => do
        stgBody <- compileANF funName x
        pure [MkAlt AltDefault [] stgBody])
      mdef
    stgAlts <- traverse (compileConAlt fc funName) alts
    pure $ StgCase stgScrutinee caseBinder altType (stgDefAlt ++ stgAlts)

  compileANF funName (AConstCase fc scrutinee alts mdef) = do
    let checkStringAlt : AConstAlt -> Bool
        checkStringAlt (MkAConstAlt (Str _) _) = True
        checkStringAlt _                       = False
    let getAltConstant : AConstAlt -> Constant
        getAltConstant (MkAConstAlt c _) = c
    case partition checkStringAlt alts of
      -- No String alts
      ([], []) => coreFail $ InternalError "Empty alternatives..."
      ([], alts@(alt::_)) => do
        let altType = PrimAlt LiftedRep
        primVal <- mkFreshSBinderStr LocalScope fc "primVal"
        stgDefAlt <- maybe
          (pure [])
          (\x => do
            stgBody <- compileANF funName x
            pure [MkAlt AltDefault [] stgBody])
          mdef
        stgAlts <- traverse (compileConstAlt funName) alts
        [tyCon] <- map nub $ traverse (tyConIdForValueConstant fc . getAltConstant) alts
          | ts => coreFail $ InternalError $ "Constant case found " ++ show (length ts) ++ " type constructors in " ++ show fc
        dtCon    <- dataConIdForValueConstant fc $ getAltConstant alt
        primType <- primTypeForValueConstant  fc $ getAltConstant alt
        pure
          $ StgCase
              (StgApp !(mkBinderIdVar fc funName scrutinee) [] stgRepType)
              !nonused
              (AlgAlt tyCon)
              [ MkAlt (AltDataCon dtCon) [primVal]
              $ StgCase (StgApp primVal.Id [] (SingleValue primType)) !nonused (PrimAlt primType) (stgDefAlt ++ stgAlts)
              ]

      -- String alts
      (strAlts, []) => do

        mStgDef <- traverseOpt (compileANF funName) mdef
        scrutBinder <- mkBinderIdVar fc funName scrutinee

        -- TODO: Make this typesafe
        let caseChain : List AConstAlt -> Core Expr
            caseChain [] = coreFail $ InternalError "Impossible, empty string alternatives."
            caseChain [MkAConstAlt (Str s) body] = do
              stringLit             <- mkFreshSBinderStr LocalScope fc "stringLitBinder"
              stringEqResult        <- mkFreshSBinderStr LocalScope fc "stringEqResult"
              unusedBinder          <- mkFreshSBinderStr LocalScope fc "unusedBinder"
              stringEqResultUnboxed <- mkFreshSBinderStr LocalScope fc "stringEqResultUnboxed"
              sBinderId <- registerString fc s
              pure $
                -- Look up the string from the string table.
                StgCase
                  -- TODO: Fix litDataCon: It got an extra layer
                  (StgConApp !litDataConId [StgVarArg sBinderId] [])
                  stringLit
                  (AlgAlt !idrisStringTyConId)
                  [ MkAlt AltDefault []
                    -- Call the strEq function
                  $ StgCase
                      (StgApp !(mkBinderIdStr "Idris.String.strEq")
                              [StgVarArg scrutBinder, StgVarArg stringLit.Id]
                              stgRepType)
                      stringEqResult
                      (AlgAlt !(tyConIdForConstant IntType))
                      [ MkAlt (AltDataCon !(dataConIdForConstant IntType))
                              [stringEqResultUnboxed] $
                              -- Unbox the result
                              StgCase
                                (StgApp stringEqResultUnboxed.Id [] stgRepType)
                                unusedBinder
                                (PrimAlt IntRep)
                                $ catMaybes
                                  [ -- Match on False: call default
                                    (MkAlt AltDefault []) <$> mStgDef
                                  , -- Match on True: call body
                                    Just $ MkAlt (AltLit (LitNumber LitNumInt 1)) []
                                                 !(compileANF funName body)
                                  ]
                      ]
                  ]
            caseChain ((MkAConstAlt (Str s) body) :: rest) = do
              stringLit             <- mkFreshSBinderStr LocalScope fc "stringLitBinder"
              stringEqResult        <- mkFreshSBinderStr LocalScope fc "stringEqResult"
              unusedBinder          <- mkFreshSBinderStr LocalScope fc "unusedBinder"
              stringEqResultUnboxed <- mkFreshSBinderStr LocalScope fc "stringEqResultUnboxed"
              sBinderId <- registerString fc s
              pure $
                -- Look up the string from the string table.
                StgCase
                  -- TODO: Fix litDataCon: It got an extra layer
                  (StgConApp !litDataConId [StgVarArg sBinderId] [])
                  stringLit
                  (AlgAlt !idrisStringTyConId)
                  [ MkAlt AltDefault []
                    -- Call the strEq function
                  $ StgCase
                      (StgApp !(mkBinderIdStr "Idris.String.strEq")
                              [StgVarArg scrutBinder, StgVarArg stringLit.Id]
                              stgRepType)
                      stringEqResult
                      (AlgAlt !(tyConIdForConstant IntType))
                      [ MkAlt (AltDataCon !(dataConIdForConstant IntType))
                              [stringEqResultUnboxed] $
                              -- Unbox the result
                              StgCase
                                (StgApp stringEqResultUnboxed.Id [] stgRepType)
                                unusedBinder
                                (PrimAlt IntRep)
                                $ -- Match on False: check the next alternative
                                  [ MkAlt AltDefault [] !(caseChain rest)
                                    -- Match on True: call body
                                  , MkAlt (AltLit (LitNumber LitNumInt 1)) []
                                          !(compileANF funName body)
                                  ]
                      ]
                  ]
            caseChain ((MkAConstAlt _ _)::_) =
              coreFail $ InternalError "Impossible, not expected non-string literal."
        caseChain strAlts

      -- Mixed alternatives
      _ => coreFail $ InternalError $ "Mixed string and non-string constant alts" ++ show fc

  compileANF _ (APrimVal fc (Str str)) = do
    topLevelBinder <- registerString fc str
    stringAddress  <- mkPrimFreshSBinderStr LocalScope fc AddrRep "stringPrimVal"
    pure $ StgCase
            (StgApp topLevelBinder [] (SingleValue AddrRep)) -- TODO: Is this right?
            stringAddress
            (PrimAlt AddrRep)
            [MkAlt AltDefault [] (StgApp !stringFromAddrBinderId [StgVarArg (stringAddress.Id)] stgRepType)]

  compileANF _ (APrimVal fc c)
   = StgConApp
      <$> dataConIdForValueConstant fc c
          -- TODO: Make this mapping safer with indexed type
      <*> (traverse (map StgLitArg . compileConstant)
                    (case c of { WorldVal => [] ; other => [other] }))
      <*> (pure [])

  compileANF _ (AErased _)
    = pure $ StgApp (MkBinderId !(uniqueForTerm ERASED_TOPLEVEL_NAME)) [] (SingleValue LiftedRep)

  -- TODO: Implement: Use Crash primop. errorBlech2 for reporting error ("%s", msg)
  compileANF _ ac@(ACrash _ msg) = do
    logLine $ "To be implemented: " ++ show ac
    pure $ StgLit $ LitString $ "ACrash " ++ msg

  compileConAlt
    : UniqueMapRef => Ref Counter Int => Ref ADTs ADTMap
    => StringTableRef => Ref Ctxt Defs => DataTypeMapRef
    => FC -> Core.Name.Name -> AConAlt
    -> Core Alt
  compileConAlt fc funName c@(MkAConAlt name Nothing args body) = do
    coreFail $ InternalError $ "Figure out how to do pattern match on type: " ++ show name
  compileConAlt fc funName c@(MkAConAlt name (Just tag) args body) = do
    stgArgs     <- traverse (mkSBinderLocal fc funName) args
    stgBody     <- compileANF funName body
    stgDataCon  <- mkDataConId name
    pure $ MkAlt (AltDataCon stgDataCon) stgArgs stgBody

  compileConstAlt
    : UniqueMapRef
    => Ref Counter Int
    => Ref ADTs ADTMap
    => StringTableRef
    => Ref Ctxt Defs
    => DataTypeMapRef
    => Core.Name.Name -> AConstAlt
    -> Core Alt
  compileConstAlt funName (MkAConstAlt constant body) = do
    stgBody <- compileANF funName body
    lit <- compileAltConstant constant
    pure $ MkAlt (AltLit lit) [] stgBody

compileTopBinding
  : UniqueMapRef => Ref Counter Int => Ref Ctxt Defs => StringTableRef
  => Ref ADTs ADTMap => Ref ExternalBinder ExtBindMap => DataTypeMapRef
  => (Core.Name.Name, ANFDef)
  -> Core (Maybe TopBinding)
compileTopBinding (funName,MkAFun args body) = do
--  logLine $ "Compiling: " ++ show funName
  funBody       <- compileANF funName body
  funArguments  <- traverse (mkSBinderLocal emptyFC funName) args
  funNameBinder <- mkSBinderName emptyFC funName
  rhs           <- pure $ StgRhsClosure ReEntrant funArguments funBody
  -- Question: Is Reentrant OK here?
  -- TODO: Calculate Rec or NonRec
  binding       <- pure $ StgRec [(funNameBinder,rhs)]
  -- Question: Is non-recursive good here? Test it.
  pure $ Just $ StgTopLifted binding
compileTopBinding (name,con@(MkACon tag arity)) = do
  -- logLine $ "TopLevel MkACon: " ++ show (name, con)
  -- Covered in the LearnDataTypes section
  pure Nothing
compileTopBinding (name,MkAForeign css fargs rtype) = do
  -- logLine $ "Found foreign: " ++ show name
  map Just $ foreign name css fargs rtype
compileTopBinding (name,MkAError body) = do
  logLine $ "Skipping error: " ++ show name
  pure Nothing

groupExternalTopIds
  : List (UnitId, ModuleName, SBinder) -> List (UnitId, List (ModuleName, List SBinder))
groupExternalTopIds = resultList . unionsMap . map singletonMap
  where
    EntryMap : Type
    EntryMap = StringMap (StringMap (List SBinder))

    resultList : EntryMap -> List (UnitId, List (ModuleName, List SBinder))
    resultList
      = map (bimap MkUnitId (map (mapFst MkModuleName) . toList))
      . toList

    unionsMap : List EntryMap -> EntryMap
    unionsMap = foldl (mergeWith (mergeWith (++))) empty

    singletonMap : (UnitId, ModuleName, SBinder) -> EntryMap
    singletonMap (MkUnitId n, MkModuleName m, sbinder) = singleton n (singleton m [sbinder])

defineMain : UniqueMapRef => Ref Counter Int => Core TopBinding
defineMain = do
  main <- mkSBinderExtId emptyFC "main"
  progMain <- mkSBinderTopLevel "{__mainExpression:0}"
  pure
    $ topLevel main []
    $ StgApp progMain.Id [] stgRepType

-- We compile only one enormous module
export
compileModule
  :  {auto _ : UniqueMapRef}
  -> {auto _ : Ref Counter Int}
  -> {auto _ : DataTypeMapRef}
  -> {auto _ : StringTableRef}
  -> {auto _ : Ref Ctxt Defs}
  -> {auto _ : Ref ExternalBinder ExtBindMap}
  -> List (Core.Name.Name, ANFDef)
  -> Core Module
compileModule anfDefs = do
  adts <- mkADTs
  definePrimitiveDataTypes
  defineErasedADT
  createDataTypes
  defineStringTypes
  let phase              = "Main"
  let moduleUnitId       = MkUnitId "main"
  let name               = MkModuleName "Main" -- : ModuleName
  let sourceFilePath     = "some.idr" -- : String
  let foreignStubs       = NoStubs -- : ForeignStubs -- ???
  let hasForeignExported = False -- : Bool
  let dependency         = [] -- : List (UnitId, List ModuleName)
  mainTopBinding         <- defineMain
  erasedTopBinding       <- erasedTopBinding
  strFunctions1          <- String.stgTopBindings
  strFunctions2          <- catMaybes <$> traverse compileTopBinding topLevelANFDefs
  let stringTopBindings = strFunctions1 ++ strFunctions2
  compiledTopBindings    <- catMaybes <$> traverse compileTopBinding anfDefs
  stringTableBindings    <- StringTable.topLevelBinders
  let topBindings        = mainTopBinding ::
                           erasedTopBinding ::
                           stringTopBindings ++
                           stringTableBindings ++
                           compiledTopBindings
  tyCons                 <- getDefinedDataTypes -- : List (UnitId, List (ModuleName, List tcBnd))
  let foreignFiles       = [] -- : List (ForeignSrcLang, FilePath)
  externalTopIds         <- groupExternalTopIds <$> genExtTopIds
  pure $ MkModule
    phase
    moduleUnitId
    name
    sourceFilePath
    foreignStubs
    hasForeignExported
    dependency
    externalTopIds
    tyCons
    topBindings
    foreignFiles


{-
RepType: How doubles are represented? Write an example program: Boxed vs Unboxed
https://gitlab.haskell.org/ghc/ghc/-/blob/master/compiler/GHC/Builtin/primops.txt.pp
-}

