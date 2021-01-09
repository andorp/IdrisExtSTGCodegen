module Idris.Codegen.ExtSTG.TTtoSTG

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
[ ] Implement Crash primitive
[ ] Handle primitive case matches accordingly
[ ] Generate STG main entry
[ ] Handle String matches with ifthenelse chains, using stringEq primop from STG
    - Create a test program which reads from input.
[ ] Implement casting
[.] Implement primitive operations
    [ ] ShiftL/ShiftR for Word needs a wrapper: Differences in parameters at STG and ANF side.
    [ ] DoubleFloor/Ceiling also needs a wrapper function as in STG the result is an Integer.
    [ ] Check if the BelieveMe operation is correct in STG
[*] Implement String handling STG code.
[ ] FFI calls AExtPrim
    [ ] Create a test program which FFI calls into a library.
    [ ] Foreign definitions should be looked up from a file, which can be modified by the user.
[ ] Module compilation
[ ] ...
-}


||| Define an STG data type with one constructor.
definePrimitiveDataType
  :  {auto _ : UniqueMapRef}
  -> {auto _ : Ref Counter Int}
  -> {auto _ : DataTypeMapRef}
  -> (String, String, Constant)
  -> Core ()
definePrimitiveDataType (u, m, StringType) = do
  logLine "Defining String datatype."
  defineDataType (MkUnitId u) (MkModuleName m) !IdrisString
definePrimitiveDataType (u, m, c) = do
  t <- typeConNameForConstant c
  n <- dataConNameForConstant c
  d <- pure $ MkSTyCon t !(tyConIdForConstant c)
                         [ MkSDataCon n !(dataConIdForConstant c)
                                        (AlgDataCon !(constantToPrimRep c))
                                        !(mkSBinderStr emptyFC ("mk" ++ n))
                                        (SsUnhelpfulSpan "<no location>") ]
                         (SsUnhelpfulSpan "<no location>")
  defineDataType (MkUnitId u) (MkModuleName m) d

||| Create the primitive types section in the STG module.
|||
||| Idris primitive types are represented as boxed values in STG, with a datatype with one constructor.
||| Eg: data IdrInt = IdrInt #IntRep
definePrimitiveDataTypes
  :  {auto _ : UniqueMapRef}
  -> {auto _ : Ref Counter Int}
  -> {auto _ : DataTypeMapRef}
  -> Core ()
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
dataConIdForValueConstant
  :  {auto _ : UniqueMapRef}
  -> {auto _ : Ref Counter Int}
  -> FC -> Constant
  -> Core DataConId
dataConIdForValueConstant _ (I _)    = MkDataConId <$> uniqueForTerm "I#"
dataConIdForValueConstant _ (BI _)   = MkDataConId <$> uniqueForTerm "IdrInteger" -- TODO: This should be GMP int
dataConIdForValueConstant _ (B8 _)   = MkDataConId <$> uniqueForTerm "W8#"
dataConIdForValueConstant _ (B16 _)  = MkDataConId <$> uniqueForTerm "W16#"
dataConIdForValueConstant _ (B32 _)  = MkDataConId <$> uniqueForTerm "W32#"
dataConIdForValueConstant _ (B64 _)  = MkDataConId <$> uniqueForTerm "W64#"
dataConIdForValueConstant _ (Ch _)   = MkDataConId <$> uniqueForTerm "C#"
dataConIdForValueConstant _ (Db _)   = MkDataConId <$> uniqueForTerm "D#"
dataConIdForValueConstant _ WorldVal = MkDataConId <$> uniqueForTerm "IdrWorld"
dataConIdForValueConstant fc other   = coreFail $ InternalError $ "dataConIdForValueConstant " ++ show other ++ ":" ++ show fc

mutual
  compileANF
    :  {auto _ : UniqueMapRef}
    -> {auto _ : Ref Counter Int}
    -> {auto _ : Ref ADTs ADTMap}
    -> {auto _ : StringTableRef}
    -> {auto _ : Ref Ctxt Defs}
    -> Core.Name.Name -> ANF
    -> Core SExpr
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
    pure $ StgLet
      (StgNonRec
        !(mkSBinderLocal fc funName var)
        (StgRhsClosure Updatable [] !(compileANF funName expr)))
      !(compileANF funName body)

  -- TODO: Implement
  compileANF _ acon@(ACon fc name Nothing args) = do
    -- Types probably will be represented with one STG TyCon and DataCon
    -- for every type.
    coreFail $ InternalError $ "Figure out how to represent a type as a value!"

  -- TODO: Implement
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
    case partition checkStringAlt alts of
      -- No String alts
      ([], alts) => do
        -- TODO: Unbox with case and match on primitves with the according representation.
        let altType = PrimAlt UnliftedRep -- Question: Is this the right reptype?
        scrutBinder <- mkBinderIdVar fc funName scrutinee
        let stgScrutinee = StgApp scrutBinder [] stgRepType
        caseBinder <- mkFreshSBinderStr LocalScope fc "constCaseBinder"
        stgDefAlt <- maybe
          (pure [])
          (\x => do
            stgBody <- compileANF funName x
            pure [MkAlt AltDefault [] stgBody])
          mdef
        stgAlts <- traverse (compileConstAlt funName) alts
        pure $ StgCase stgScrutinee caseBinder altType (stgDefAlt ++ stgAlts)
      -- String alts
      (strAlts, []) => do
        logLine $ "To be implemented: " ++ show ("AConstCase",fc,scrutinee)
        pure $ StgLit $ LitString "AConstCase string alternatives"
      _ => coreFail $ InternalError $ "Mixed string and non-string constant alts" ++ show fc

  compileANF _ (APrimVal fc (Str str)) = do
    topLevelBinder <- registerString fc str
    caseBinder     <- mkFreshSBinderStr LocalScope fc "stringPrimVal"
    pure $ StgCase
            (StgApp topLevelBinder [] (SingleValue AddrRep)) -- TODO: Is this right?
            caseBinder
            (PrimAlt AddrRep)
            [MkAlt AltDefault [] (StgConApp !litConId [StgVarArg (Id caseBinder)] [])]

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
    :  {auto _ : UniqueMapRef}
    -> {auto _ : Ref Counter Int}
    -> {auto _ : Ref ADTs ADTMap}
    -> {auto _ : StringTableRef}
    -> {auto _ : Ref Ctxt Defs}
    -> FC -> Core.Name.Name -> AConAlt
    -> Core SAlt
  compileConAlt fc funName c@(MkAConAlt name Nothing args body) = do
    coreFail $ InternalError $ "Figure out how to do pattern match on type: " ++ show name
  compileConAlt fc funName c@(MkAConAlt name (Just tag) args body) = do
    stgArgs     <- traverse (mkSBinderLocal fc funName) args
    stgBody     <- compileANF funName body
    stgDataCon  <- mkDataConId name
    pure $ MkAlt (AltDataCon stgDataCon) stgArgs stgBody

  compileConstAlt
    :  {auto _ : UniqueMapRef}
    -> {auto _ : Ref Counter Int}
    -> {auto _ : Ref ADTs ADTMap}
    -> {auto _ : StringTableRef}
    -> {auto _ : Ref Ctxt Defs}
    -> Core.Name.Name -> AConstAlt
    -> Core SAlt
  compileConstAlt funName (MkAConstAlt constant body) = do
    stgBody <- compileANF funName body
    lit <- compileAltConstant constant
    pure $ MkAlt (AltLit lit) [] stgBody

compileTopBinding
  :  {auto _ : UniqueMapRef}
  -> {auto _ : Ref Counter Int}
  -> {auto _ : Ref Ctxt Defs}
  -> {auto _ : StringTableRef}
  -> {auto _ : Ref ADTs ADTMap}
  -> (Core.Name.Name, ANFDef)
  -> Core (Maybe STopBinding)
compileTopBinding (funName,MkAFun args body) = do
--  coreLift $ putStrLn $ "Compiling: " ++ show funName
  funBody       <- compileANF funName body
  funArguments  <- traverse (mkSBinderLocal emptyFC funName) args
  funNameBinder <- mkSBinderName emptyFC funName
  rhs           <- pure $ StgRhsClosure ReEntrant funArguments funBody
  -- Question: Is Reentrant OK here?
  binding       <- pure $ StgNonRec funNameBinder rhs
  -- Question: Is non-recursive good here? Test it.
  pure $ Just $ StgTopLifted binding
compileTopBinding (name,con@(MkACon tag arity)) = do
  -- logLine $ "TopLevel MkACon: " ++ show (name, con)
  -- Covered in the LearnDataTypes section
  pure Nothing
compileTopBinding (name,MkAForeign css fargs rtype) = do
  logLine $ "Skipping foreign: " ++ show name
  pure Nothing
compileTopBinding (name,MkAError body) = do
  logLine $ "Skipping error: " ++ show name
  pure Nothing

-- TODO: Learn this from the STG module
||| Register the string function bindings
strFunctionTopBindings
  :  {auto _ : UniqueMapRef}
  -> {auto _ : Ref Counter Int}
  -> Core ()
strFunctionTopBindings = traverse_ (mkSBinderStr emptyFC)
  [ "strHead"
  , "strLength"
  , "strHead"
  , "strTail"
  , "strIndex"
  , "strCons"
  , "strAppend"
  , "strReverse"
  , "strSubstr"
  ]

-- We compile only one enormous module
export
compileModule
  :  {auto _ : UniqueMapRef}
  -> {auto _ : Ref Counter Int}
  -> {auto _ : DataTypeMapRef}
  -> {auto _ : StringTableRef}
  -> {auto _ : Ref Ctxt Defs}
  -> List (Core.Name.Name, ANFDef)
  -> Core SModule
compileModule anfDefs = do
  adts <- mkADTs
  definePrimitiveDataTypes
  defineErasedADT
  createDataTypes
  let phase              = "Main"
  let moduleUnitId       = MkUnitId "MainUnit"
  let name               = MkModuleName "Main" -- : ModuleName
  let sourceFilePath     = "some.idr" -- : String
  let foreignStubs       = NoStubs -- : ForeignStubs -- ???
  let hasForeignExported = False -- : Bool
  let dependency         = [] -- : List (UnitId, List ModuleName)
  let externalTopIds     = [] -- : List (UnitId, List (ModuleName, List idBnd))
  erasedTopLevel         <- erasedTopBinding
  strFunctions           <- strFunctionTopBindings
  compiledTopBindings    <- mapMaybe id <$> traverse compileTopBinding anfDefs
  stringTableBindings    <- StringTable.topLevelBinders
  stringImplBindings     <- String.stringModuleFunctions
  let topBindings        = erasedTopLevel :: stringTableBindings ++ stringImplBindings ++ compiledTopBindings
  tyCons                 <- getDefinedDataTypes -- : List (UnitId, List (ModuleName, List tcBnd))
  let foreignFiles       = [] -- : List (ForeignSrcLang, FilePath)
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
public export
data Constant
    = I Int
    | BI Integer
    | B8 Int
    | B16 Int
    | B32 Int
    | B64 Integer
    | Str String
    | Ch Char
    | Db Double
    | WorldVal

    | IntType
    | IntegerType
    | Bits8Type
    | Bits16Type
    | Bits32Type
    | Bits64Type
    | StringType
    | CharType
    | DoubleType
    | WorldType

data AVar : Type where
     ALocal : Int -> AVar
     ANull : AVar -- Erased variable

data ANF where
    -- Reference a variable
    AV : FC -> AVar -> ANF
    | StgApp with zero argument, it works on local variables.

    -- Apply a function to the list of arguments
    AAppName : FC -> Name -> List AVar -> ANF
    -- StgApp

    -- Function application with less arguments than needed.
    AUnderApp : FC -> Name -> (missing : Nat) -> (args : List AVar) -> ANF
    -- StgApp ???
    -- Let underapp, RhsClosure

    -- Apply a closure to an argument
    AApp : FC -> (closure : AVar) -> (arg : AVar) -> ANF
    -- StgApp ???

    -- Create a let binding
    ALet : FC -> (var : Int) -> ANF -> ANF -> ANF
    -- StgLet
    -- StgLetNoEscape
    = StgNonRec idBnd (Rhs' idBnd idOcc dcOcc tcOcc)
    | StgRec    (List (idBnd, Rhs' idBnd idOcc dcOcc tcOcc))
    = StgRhsClosure UpdateFlag (List idBnd) (Expr' idBnd idOcc dcOcc tcOcc)
    | StgRhsCon dcOcc (List (Arg' idOcc))
    -- Write a recursive let example and investigate its IR: No recursive lets are allowed.

    -- Create a con value. -- TODO: What is the tag parameter?
    ACon : FC -> Name -> (tag : Maybe Int) -> List AVar -> ANF
    -- StgConApp: how to add types

    -- Apply a primitive to some arguments
    AOp : FC -> PrimFn arity -> Vect arity AVar -> ANF
    -- StgOpApp

    -- Apply an external primitive to some arguments
    AExtPrim : FC -> Name -> List AVar -> ANF
    -- StgOpApp
    -- StgFCallOp

    -- Case expression that matches some Con values
    AConCase : FC -> AVar -> List AConAlt -> Maybe ANF -> ANF
    | StgCase expr idBnd alttype alts

    -- Case expression that matches some constant values
    AConstCase : FC -> AVar -> List AConstAlt -> Maybe ANF -> ANF
    | StgCase expr idBnd alttype alts
    | Simple values represented as our boxed types

    -- Create a primitive value
    APrimVal : FC -> Constant -> ANF
    | StgLit Lit
    | We need to box the simple values

    -- Erased ANF
    AErased : FC -> ANF
    | Represent as (StgLit LitNullAddr) ?
    | Generate any trash

    -- Runtime error
    ACrash : FC -> String -> ANF
    | Represent as StgApp error?
    | Impossbile Or not?
    | There is a primop which does that

  public export
  data AConAlt : Type where
       MkAConAlt : Name -> (tag : Maybe Int) -> (args : List Int) ->
                   ANF -> AConAlt

  record Alt' (idBnd : Type) (idOcc : Type) (dcOcc : Type) (tcOcc : Type) where
    constructor MkAlt
    Con     : AltCon' dcOcc
    Binders : List idBnd
    RHS     : Expr' idBnd idOcc dcOcc tcOcc
  data AltCon' dcOcc
    = AltDataCon dcOcc
    | AltLit     Lit
    | AltDefault

  public export
  data AConstAlt : Type where
       MkAConstAlt : Constant -> ANF -> AConstAlt

  record Alt' (idBnd : Type) (idOcc : Type) (dcOcc : Type) (tcOcc : Type) where
    constructor MkAlt
    Con     : AltCon' dcOcc
    Binders : List idBnd
    RHS     : Expr' idBnd idOcc dcOcc tcOcc
  data AltCon' dcOcc
    = AltDataCon dcOcc
    | AltLit     Lit
    | AltDefault

public export
data ANFDef : Type where
  MkAFun : (args : List Int) -> ANF -> ANFDef
  MkACon : (tag : Maybe Int) -> (arity : Nat) -> ANFDef
  MkAForeign : (ccs : List String) -> (fargs : List CFType) ->
              CFType -> ANFDef
  MkAError : ANF -> ANFDef

public export
record CompileData where
  constructor MkCompileData
  mainExpr : CExp [] -- main expression to execute. This also appears in
                     -- the definitions below as MN "__mainExpression" 0
  namedDefs : List (Name, FC, NamedDef)
  lambdaLifted : List (Name, LiftedDef)
       -- ^ lambda lifted definitions, if required. Only the top level names
       -- will be in the context, and (for the moment...) I don't expect to
       -- need to look anything up, so it's just an alist.
  anf : List (Name, ANFDef)
       -- ^ lambda lifted and converted to ANF (all arguments to functions
       -- and constructors transformed to either variables or Null if erased)
  vmcode : List (Name, VMDef)
       -- ^ A much simplified virtual machine code, suitable for passing
       -- to a more low level target such as C

RepType: How doubles are represented? Write an example program: Boxed vs Unboxed
-}

{-
https://gitlab.haskell.org/ghc/ghc/-/blob/master/compiler/GHC/Builtin/primops.txt.pp
-}

{-
How to represent String and idris FFI calls?
-}
