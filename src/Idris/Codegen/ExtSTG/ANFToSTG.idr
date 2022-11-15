module Idris.Codegen.ExtSTG.ANFToSTG

import Compiler.ANF
import Core.CompileExpr
import Core.Context
import Core.Core
import Core.TT
import Data.IOArray
import Data.List
import Data.SortedMap
import Data.String
import Data.Vect
import Prelude

import Idris.Codegen.ExtSTG.ADTs
import Idris.Codegen.ExtSTG.Configuration
import Idris.Codegen.ExtSTG.Context
import Idris.Codegen.ExtSTG.DiscoverADTs
import Idris.Codegen.ExtSTG.ExtName
import Idris.Codegen.ExtSTG.Foreign
import Idris.Codegen.ExtSTG.PrimOp
import Idris.Codegen.ExtSTG.STG
import Idris.Codegen.ExtSTG.StringTable

import public Idris.Codegen.ExtSTG.Core

%default total

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
[x] Generate STG main entry
[.] Handle String matches with ifthenelse chains, using stringEq primop from STG
    [ ] Create a test program which reads from input.
[.] Implement primitive operations
    [ ] Implement casting
    [ ] ShiftL/ShiftR for Word needs a wrapper: Differences in parameters at STG and ANF side.
    [ ] DoubleFloor/Ceiling also needs a wrapper function as in STG the result is an Integer.
    [ ] Check if the BelieveMe operation is correct in STG
[*] Implement String handling STG code.
[.] FFI calls AExtPrim
    [.] Create a test program which FFI calls into a library.
    [.] Foreign definitions should be looked up from a file, which can be modified by the user.
[ ] Module compilation
[+] FIX: Use StgCase instead of StgLet, otherwise strict semantics breaks.
[ ] Store defined/referred topLevelBinders
[+] Figure out how to use MkAlt with indexed types
[ ] Figure out the semantics for LazyReason in ANF Apps
[ ] ...
-}

||| Compile constant for case alternative.
compileAltConstant : Constant -> Core Lit
compileAltConstant (I i)   = pure $ LitNumber LitNumInt $ cast i
compileAltConstant (I8 i)  = pure $ LitNumber LitNumInt $ cast i
compileAltConstant (I16 i) = pure $ LitNumber LitNumInt $ cast i
compileAltConstant (I32 i) = pure $ LitNumber LitNumInt $ cast i
compileAltConstant (I64 i) = pure $ LitNumber LitNumInt64 $ cast i
compileAltConstant (B8 i)  = pure $ LitNumber LitNumWord $ cast i
compileAltConstant (B16 i) = pure $ LitNumber LitNumWord $ cast i
compileAltConstant (B32 i) = pure $ LitNumber LitNumWord $ cast i
compileAltConstant (B64 i) = pure $ LitNumber LitNumWord64 $ cast i
compileAltConstant (Ch c)  = pure $ LitChar c
compileAltConstant (Db d)  = pure $ LitDouble d
compileAltConstant c = coreFail $ InternalError $ "compileAltConstant " ++ show c

||| Compile constant for APrimVal, Boxing a value in STG.
compileConstant : Constant -> Core Lit
compileConstant (I i)   = pure $ LitNumber LitNumInt $ cast i
compileConstant (I8 i)  = pure $ LitNumber LitNumInt $ cast i
compileConstant (I16 i) = pure $ LitNumber LitNumInt $ cast i
compileConstant (I32 i) = pure $ LitNumber LitNumInt $ cast i
compileConstant (I64 i) = pure $ LitNumber LitNumInt64 $ cast i
compileConstant (B8 i)  = pure $ LitNumber LitNumWord $ cast i
compileConstant (B16 i) = pure $ LitNumber LitNumWord $ cast i
compileConstant (B32 i) = pure $ LitNumber LitNumWord $ cast i
compileConstant (B64 i) = pure $ LitNumber LitNumWord64 $ cast i
compileConstant (Ch c)  = pure $ LitChar c
compileConstant (Db d)  = pure $ LitDouble d
compileConstant c = coreFail $ InternalError $ "compileConstant " ++ show c

||| Determine the Data constructor for the boxed primitive value.
||| Used in creating PrimVal
|||
||| The name of terms should coincide the ones that are defined in GHC's ecosystem.
dataConIdForValueConstant : Ref STGCtxt STGContext => Constant -> Core DataConIdSg
dataConIdForValueConstant = map (identSg . dataConSTG) . lookupPrimType . primTypeOf

tyConIdForValueConstant
  :  Ref STGCtxt STGContext
  => FC -> Constant -> Core TyConId
tyConIdForValueConstant _ (I _)    = tyConIdForPrimType IntType
tyConIdForValueConstant _ (BI _)   = tyConIdForPrimType IntegerType
tyConIdForValueConstant _ (I8 _)   = tyConIdForPrimType Int8Type
tyConIdForValueConstant _ (I16 _)  = tyConIdForPrimType Int16Type
tyConIdForValueConstant _ (I32 _)  = tyConIdForPrimType Int32Type
tyConIdForValueConstant _ (I64 _)  = tyConIdForPrimType Int64Type
tyConIdForValueConstant _ (B8 _)   = tyConIdForPrimType Bits8Type
tyConIdForValueConstant _ (B16 _)  = tyConIdForPrimType Bits16Type
tyConIdForValueConstant _ (B32 _)  = tyConIdForPrimType Bits32Type
tyConIdForValueConstant _ (B64 _)  = tyConIdForPrimType Bits64Type
tyConIdForValueConstant _ (Ch _)   = tyConIdForPrimType CharType
tyConIdForValueConstant _ (Db _)   = tyConIdForPrimType DoubleType
tyConIdForValueConstant _ WorldVal = tyConIdForPrimType WorldType
tyConIdForValueConstant fc other   = coreFail $ InternalError $ "tyConIdForValueConstant " ++ show other ++ ":" ++ show fc

createAlternatives
  :  (r : RepType)
  -> List (Lit, Expr Core.stgRepType)
  -> Maybe (List (Alt r Core.stgRepType))
createAlternatives r [] = Just []
createAlternatives r ((l,b) :: ls) = do
  Refl <- decLitRepType l r
  a    <- decAltLit l
  map ((MkAlt (AltLit l) () b) ::) $ createAlternatives r ls

data Convertible : PrimRep -> PrimRep -> Type where
  SameRep      : Convertible p p
  Conversion   : {p : String} -> PrimOp p [p1] p2 -> Convertible p1 p2
  NoConversion : Convertible p1 p2

convertible : (p1, p2 : PrimRep) -> Convertible p1 p2
convertible WordRep Word8Rep  = Conversion NarrowWord8
convertible Word8Rep WordRep  = Conversion ExtendWord8
convertible WordRep Word16Rep = Conversion NarrowWord16
convertible Word16Rep WordRep = Conversion ExtendWord16
convertible WordRep Word32Rep = Conversion NarrowWord32
convertible Int8Rep IntRep    = Conversion ExtendInt8
convertible IntRep  Int8Rep   = Conversion NarrowInt8
convertible Int16Rep IntRep   = Conversion ExtendInt16
convertible IntRep   Int16Rep = Conversion NarrowInt16
convertible IntRep   Int32Rep = Conversion NarrowInt32
convertible p1 p2 with (semiDecEq p1 p2)
  _ | Nothing         = NoConversion
  _ | (Just p1p2Same) = rewrite p1p2Same in SameRep

checkNonEmpty : (l : List a) -> Core (NonEmpty l)
checkNonEmpty [] = coreFail $ InternalError "Given list was empty, expected non-empty."
checkNonEmpty (x :: xs) = pure $ IsNonEmpty

||| Creates an STGCase expression which refers to top-level String value and calls the
||| String conversion functions.
compileStringValue : Ref STGCtxt STGContext => FC -> String -> Core (Expr Core.stgRepType)
compileStringValue fc str = do
  -- Strings compiled to top-level references. String table implementation with
  -- top-level binding.
  topLevelBinder <- registerString fc str
  stringAddress  <- mkFreshSBinderStr LocalScope fc "stringPrimVal"
  mkStrFromAddrExpr <- createExtSTGPureApp
                        (MkExtName "main" ["Idris", "Runtime", "String"] "mkStrFromAddr")
                        [mkArgSg $ StgVarArg $ getBinderId stringAddress]
  pure
    $ StgCase
        (PrimAlt AddrRep)
        (StgApp topLevelBinder [] (SingleValue AddrRep))
        stringAddress
        [ MkAlt AltDefault () mkStrFromAddrExpr ]

compileBigIntegerValue : Ref STGCtxt STGContext => FC -> Integer -> Core (Expr Core.stgRepType)
compileBigIntegerValue fc i = do
  -- Bigintegers are handled as String literals 
  topLevelBinder <- registerString fc $ show i
  stringAddress  <- mkFreshSBinderStr LocalScope fc "stringPrimVal"
  mkStrFromAddrExpr <- createExtSTGPureApp
                        (MkExtName "main" ["Idris", "Runtime", "String"] "mkStrFromAddr")
                        [mkArgSg $ StgVarArg $ getBinderId stringAddress]
  strResultBinder <- mkFreshSBinderStr LocalScope fc "strResult"
  bigIntFromStr <- createExtSTGPureApp
                        (MkExtName "main" ["Idris", "Runtime", "Integer"] "fromStr")
                        [mkArgSg $ StgVarArg $ getBinderId strResultBinder ]
  pure
    $ StgCase
        (PrimAlt AddrRep)
        (StgApp topLevelBinder [] (SingleValue AddrRep))
        stringAddress
        [ MkAlt AltDefault ()
        $ StgCase
            PolyAlt
            mkStrFromAddrExpr
            strResultBinder
            [ MkAlt AltDefault ()
            $ bigIntFromStr
            ]
        ]

data ConstAltKind = BigIntAlt | StringAlt | Mixed | Other

join : ConstAltKind -> ConstAltKind -> ConstAltKind
join BigIntAlt BigIntAlt = BigIntAlt
join StringAlt StringAlt = StringAlt
join Other     Other     = Other
join _         _         = Mixed

constAltKind : AConstAlt -> ConstAltKind
constAltKind (MkAConstAlt (BI x)  _) = BigIntAlt
constAltKind (MkAConstAlt (Str x) _) = StringAlt
constAltKind (MkAConstAlt _       _) = Other

detectConstAltsKind : (as : List AConstAlt) -> {auto ok : NonEmpty as} -> ConstAltKind
detectConstAltsKind (x :: xs) {ok = IsNonEmpty} = foldl (\a , x => join a (constAltKind x)) (constAltKind x) xs

covering
detectTyConIdOfConAlts
  :  Ref Ctxt Defs
  => Ref STGCtxt STGContext
  => (as : List AConAlt) -> {auto ok : NonEmpty as}
  -> Core STyCon
detectTyConIdOfConAlts (MkAConAlt n c t as b :: xs) {ok = IsNonEmpty} = do
  (_, stycon) <- lookupDTCon n
  foldlC
    (\t , a => case a of
      (MkAConAlt n1 x tag args y) => do
        (_, t2) <- lookupDTCon n1
        if t.Id == t2.Id
          then pure t
          else coreFail $ InternalError "Different TyCon found in alternatives. Expected: \{show t} , found: \{show t2}")
    stycon
    xs

||| Create a StdVarArg for the Argument of a function application.
mkStgArg
  :  Ref STGCtxt STGContext
  => Core.Name.Name -> AVar
  -> Core ArgSg
mkStgArg n a = map (mkArgSg . StgVarArg . binderId) $ lookupLocalVarBinder n a

||| Determine the Data constructor for the boxed primitive type.
|||
||| The name of terms should coincide the ones that are defined in GHC's ecosystem. This
||| would make the transition easier, I hope.
dataConIdForPrimType : Ref STGCtxt STGContext => PrimType -> Core DataConIdSg
dataConIdForPrimType = map (identSg . dataConSTG) . lookupPrimType

mutual

  partial -- TODO: Fix annotation
  compileANF
    :  Ref Ctxt Defs
    => Ref STGCtxt STGContext
    => Core.Name.Name
    -> ANF
    -> Core (Expr Core.stgRepType)
  compileANF funName (AV fc var) = do
    logLine Debug "Compiling AV \{show fc} \{show var}"
    pure $ StgApp (binderId !(lookupLocalVarBinder funName var)) [] stgRepType

  -- TODO: Figure out the semantics for LazyReason
  compileANF funName (AAppName fc Nothing funToCall args) = do
    logLine Debug "Compiling AAppName \{show fc} \{show funToCall}"
    pure $ StgApp (binderId !(lookupFunctionBinder funToCall))
                  !(traverse (mkStgArg funName) args)
                   stgRepType
  compileANF funName (AAppName fc (Just lazyReason) funToCall args) = do
    coreFail $ InternalError "New version of laziness annotation is not supported yet."

  compileANF funName (AUnderApp fc funToCall _ args) = do
    logLine Debug "Compiling AUnderApp \{show fc} \{show funToCall} \{show (length args)}"
    pure $ StgApp (binderId !(lookupFunctionBinder funToCall))
                  !(traverse (mkStgArg funName) args)
                  stgRepType

  -- TODO: Figure out the semantics for LazyReason
  compileANF funName (AApp fc Nothing closure arg) = do
    logLine Debug "Compiling AApp \{show fc}"
    pure $ StgApp (binderId !(lookupLocalVarBinder funName closure))
                  [!(mkStgArg funName arg)]
                  stgRepType
  compileANF funName (AApp fc (Just lazyReason) closure arg) = do
    coreFail $ InternalError "New version of laziness annotation is not supported yet."

  compileANF funName (ALet fc var expr body) = do
    logLine Debug "Compiling ALet \{show fc} \{show var}"
    pure $ StgCase
            PolyAlt -- It could be ForceUnbox, and only AltDefault should be used in Strict Let
            !(compileANF funName expr)
            !(createLocalVarBinder fc funName (ALocal var))
            [ MkAlt AltDefault () !(compileANF funName body) ]

  compileANF funName acon@(ACon fc name TYCON tag args) = do
    logLine Debug "Compiling ACon \{show fc} TYCON \{show name}"
    (rep ** tyDataConId) <- mkTyDataConId name
    conArgs <- compileConArgs fc funName args rep
    pure $ StgConApp tyDataConId conArgs

  compileANF funName acon@(ACon fc name nonTYCON tag args) = do
    -- TODO: Use of conInfo information in mapping.
    -- Lookup the constructor based on the name.
    -- The tag information is not relevant here.
    -- Args are variables
    logLine Debug "Compiling ACon \{show fc} \{show name} \{show nonTYCON} \{show (length args)}"
    (rep ** dataConId) <- mkDataConIdStr name
    conArgs <- compileConArgs fc funName args rep
    pure $ StgConApp dataConId conArgs

  compileANF funName (AOp fc Nothing prim args) = do
    logLine Debug "Compiling AOp \{show fc} \{show prim} \{show (length args)}"
    compilePrimOp fc funName prim args
  compileANF funName (AOp fc (Just lazyReason) prim args) = do
    coreFail $ InternalError "New version of laziness annotation is not supported yet."

  -- External primitives should be pure functions. If not all should be IO?
  compileANF funName aext@(AExtPrim fc Nothing name as) = do
    logLine Debug "Compiling AExtPrim \{show fc} \{show name} \{show (length as)}"
    args <- traverse (map (mkArgSg . StgVarArg . binderId) . lookupLocalVarBinder funName) $ toList as
    ext <- extPrimName name
    createExtSTGPureApp ext args
  compileANF funName aext@(AExtPrim fc (Just _) name args) = do
    coreFail $ InternalError "New version of laziness annotation is not supported yet."

  compileANF funName (AConCase fc scrutinee [] Nothing) =
    coreFail $ InternalError "Empty case expression in \{show (fc,funName)}"
  compileANF funName (AConCase fc scrutinee [] (Just def)) = do
    logLine Debug "Compiling AConCase \{show fc} with one default branch."
    compileANF funName def
  compileANF funName (AConCase fc scrutinee (a :: as) mdef) = do
    logLine Debug "Compiling AConCase \{show fc}"
    let alts = a :: as
    scrutBinder <- map binderId $ lookupLocalVarBinder funName scrutinee
    caseBinder <- mkFreshSBinderStr LocalScope fc "conCaseBinder"
    stgDefAlt <- maybe
      (pure [])
      (\x => do
        stgBody <- compileANF funName x
        pure [MkAlt AltDefault () stgBody])
      mdef
    let stgScrutinee = StgApp scrutBinder [] stgRepType
    case nub (map (\(MkAConAlt n c t as b) => c) alts) of
      [TYCON] => do
        logLine Debug "Compiling AConCase \{show fc} : Pattern match on Type"
        tyCon <- getTypeOfTypes
        stgAlts <- traverse (compileConAlt fc funName) alts
        pure $ StgCase (AlgAlt tyCon.Id) stgScrutinee caseBinder (stgDefAlt ++ stgAlts)
      _ => do
        logLine Debug "Compiling AConCase \{show fc} : Pattern match on Value"
        tyCon <- detectTyConIdOfConAlts (a :: as)
        stgAlts <- traverse (compileConAlt fc funName) alts
        pure $ StgCase (AlgAlt tyCon.Id) stgScrutinee caseBinder (stgDefAlt ++ stgAlts)

  compileANF funName (AConstCase fc scrutinee [] Nothing) = 
    coreFail $ InternalError "Empty case expression in \{show (fc,funName)}"
  compileANF funName (AConstCase fc scrutinee [] (Just def)) = do
    logLine Debug "Compiling AConstCase \{show fc} with one default branch."
    compileANF funName def
  compileANF funName (AConstCase fc scrutinee (a :: as) mdef) = do
    logLine Debug "Compiling AConstCase \{show fc}"
    mStgDef <- traverseOpt (compileANF funName) mdef
    scrutBinder <- map binderId $ lookupLocalVarBinder funName scrutinee
    case detectConstAltsKind (a :: as) of
      Mixed => coreFail $ InternalError "Case expression has mixed set of alternatives: \{show (fc, funName)}"
      BigIntAlt => do
        logLine Debug "Compiling AConstCase \{show fc} BigIntAlt"
        compileConstAltsToEqChain funName fc scrutBinder (a :: as) mStgDef
      StringAlt => do
        logLine Debug "Compiling AConstCase \{show fc} StringAlt"
        compileConstAltsToEqChain funName fc scrutBinder (a :: as) mStgDef
      Other => do
        logLine Debug "Compiling AConstCase \{show fc} Other"
        compileSimplePrimConstAlts funName fc scrutBinder (a :: as) mStgDef

  -- Strings compiled to top-level references. String table implementation with
  -- top-level binding.
  compileANF _ (APrimVal fc (Str str)) = do
    logLine Debug "Compiling APrimVal \{show fc} String: \{str}"
    compileStringValue fc str
  compileANF n (APrimVal fc (BI i)) = compileBigIntegerValue fc i

  compileANF _ (APrimVal fc WorldVal) = do
    logLine Debug "Compiling APrimVall \{show fc} WorldVal"
    (AlgDataCon [] ** dataConId) <- dataConIdForValueConstant WorldVal
      | other => coreFail $ InternalError $ show (fc,WorldVal) ++ " has different representation: " ++ show other
    pure $ StgConApp dataConId ()

  compileANF n (APrimVal fc c) = do
    logLine Debug "Compiling APrimVal \{show fc} \{show c}"
    (AlgDataCon [rep] ** dataConId) <- dataConIdForValueConstant c
      | other => coreFail $ InternalError $ show (fc, c) ++ " has different representation: " ++ show other
    lit <- compileConstant c
    case convertible (litPrimRep lit) rep of
      NoConversion
        => coreFail $ InternalError "\{show (fc,c)} no conversion STG function is found for \{show (rep, litPrimRep lit)}"
      SameRep
        => pure $ StgConApp dataConId (StgLitArg lit)
      (Conversion primOp)
        => do
          logLine Debug "APrimVal \{show fc} conversion is needed for \{show lit} with \{name primOp}"
          convertResultBinder <- mkFreshSBinderStr LocalScope fc "convertResultBinder"
          pure
            $ StgCase
                (PrimAlt rep)
                (StgOpApp primOp (StgLitArg lit))
                convertResultBinder
                [ MkAlt AltDefault ()
                  $ StgConApp dataConId (StgVarArg (getBinderId convertResultBinder))
                ]

  compileANF _ (AErased fc) = do
    logLine Debug "Compiling AErased \{show fc}"
    pure $ StgApp (binderId !(extNameLR erasedExtName)) [] (SingleValue LiftedRep)

  compileANF _ ac@(ACrash fc msg) = do
    logLine Debug "Compiling ACrash \{show fc} \{msg}"
    strExpr <- compileStringValue fc msg
    strLiteral <- mkFreshSBinderStr LocalScope fc "strLiteral"
    crashExpr
      <- createExtSTGIOApp
          (MkExtName "main" ["Idris", "Runtime", "Crash"] "crash")
          [ mkArgSg $ StgVarArg $ binderId strLiteral ]
    pure $ StgCase PolyAlt strExpr strLiteral [ MkAlt AltDefault () crashExpr ]

  partial
  compileSimplePrimConstAlts
    :  Ref STGCtxt STGContext
    => Ref Ctxt Defs
    => Core.Name.Name -> FC
    -> BinderId Core.stgRepType
    -> (as : List AConstAlt)
    -> {auto ok : NonEmpty as}
    -> Maybe (Expr Core.stgRepType)
    -> Core (Expr Core.stgRepType)
  compileSimplePrimConstAlts fn fc scrut (alt :: as) {ok = IsNonEmpty} mdef = do
    let alts = alt :: as
    let getAltConstant : AConstAlt -> Constant
        getAltConstant (MkAConstAlt c _) = c
    [tyCon] <- map nub $ traverse (tyConIdForValueConstant fc . getAltConstant) alts
      | ts => coreFail $ InternalError $ "Constant case found " ++ show (length ts) ++ " type constructors in " ++ show fc
    let altConstant = getAltConstant alt
    ((AlgDataCon [dataFieldRep]) ** dtCon) <- dataConIdForValueConstant altConstant
      | wrongRep => coreFail $ InternalError $ "DataConId has wrong RepType: " ++ show (fc, fn, wrongRep)
    litBodies
      <- traverse
          (\(MkAConstAlt c b) => do
              lit <- compileAltConstant c
              body <- compileANF fn b
              pure (lit, body))
          alts

    -- TODO: Learn every LitRep, check if all the same
    nonEmpty <- checkNonEmpty (map fst litBodies)
    let lit : Lit = head $ map fst litBodies
    let litRep : PrimRep = litPrimRep lit
    
    let Just stgAlts = createAlternatives (SingleValue litRep) litBodies
        | Nothing => coreFail $ InternalError "TODO: Error message"
    let stgDefAlt
          := maybe
              []
              (\x => [the (Alt (SingleValue litRep) Core.stgRepType) (MkAlt AltDefault () x)])
              mdef
    -- scrutinee refers to a variable which has a constant/primitive value. This primitive value in STG representation is
    -- Data constructor with some primitive value. The
    primVal <- mkFreshSBinderStr LocalScope fc "primVal"
    convertOrLookupVar <- case convertible dataFieldRep litRep of
      SameRep => do
        -- Just look up the variable
        pure (StgApp (binderId primVal) [] (SingleValue litRep))
      (Conversion primOp) => do
        -- First apply the PrimOp on the variable
        pure (StgOpApp primOp (StgVarArg (getBinderId primVal)))
      NoConversion => do
        coreFail $ InternalError "Primitive rep \{show dataFieldRep} in data constructor \{show dtCon} is not convertible to primitive rep \{show litRep} to literal \{show (map fst litBodies)}"
    pure
      -- Unwraps the literal value from the data constructor.
      $ StgCase
          (AlgAlt tyCon)
          (StgApp scrut [] stgRepType)
          !nonused
          [ MkAlt (AltDataCon (mkDataConIdSg dtCon)) primVal
            -- Matches the primVal with the literals
          $ StgCase
              (PrimAlt litRep)
              convertOrLookupVar
              !(nonusedRep (SingleValue litRep))
              (stgDefAlt ++ stgAlts)
          ]

  -- The list of alternatives should be homogeneous. All strings or all bigints.
  -- That property is checked in the client code of this function.
  partial
  compileConstAltsToEqChain
    :  Ref Ctxt Defs
    => Ref STGCtxt STGContext
    => Core.Name.Name -> FC
    -> BinderId Core.stgRepType
    -> List AConstAlt
    -> Maybe (Expr Core.stgRepType)
    -> Core (Expr Core.stgRepType)
  compileConstAltsToEqChain fn fc scrut [] Nothing
    = pure !(createExtSTGPureApp
              (MkExtName "main" ["Idris","Runtime","Crash"] "missingDefault")
              [])
  compileConstAltsToEqChain fn fc scrut [] (Just defAlt) = pure defAlt
  compileConstAltsToEqChain fn fc scrut (MkAConstAlt (BI i) body :: as) defAlt = do
    -- compile the BigInteger to a value producing function
    createBigIntExpr <- compileBigIntegerValue fc i
    -- bind the created value to a variable
    bigIntegerLiteral <- mkFreshSBinderStr LocalScope fc "bigIntegerLiteral"
    -- call the compare function on the biginteger literal and the scrutinee
    compareToScrutExpr
      <- createExtSTGPureApp
          (MkExtName "main" ["Idris", "Runtime", "Integer"] "eq")
          [ mkArgSg $ StgVarArg scrut
          , mkArgSg $ StgVarArg $ binderId bigIntegerLiteral
          ]
    intTyCon <- tyConIdForPrimType IntType
    ((AlgDataCon [IntRep]) ** ti) <- dataConIdForPrimType IntType
      | wrongRep => coreFail $ InternalError $ "DataConId has wrong RepType: \{show (fc,fn,wrongRep)}"
    compareResult        <- mkFreshSBinderStr LocalScope fc "compareResult"
    compareResultUnboxed <- mkFreshSBinderStr LocalScope fc "compareResultUnboxed"
    unusedBinder         <- mkFreshSBinderStr LocalScope fc "unusedBinder"
    -- create an STG case
    -- for matching case compile the body
    -- for non matching case continue on the default case.
    pure
      $ StgCase
          PolyAlt
          createBigIntExpr
          bigIntegerLiteral
          [ MkAlt AltDefault ()
          $ StgCase
              (AlgAlt intTyCon)
              compareToScrutExpr
              compareResult
              [ MkAlt (AltDataCon (mkDataConIdSg ti)) compareResultUnboxed
                $ StgCase
                    (PrimAlt IntRep)
                    (StgApp (binderId compareResultUnboxed) [] (SingleValue IntRep))
                    unusedBinder
                    [ -- Match on False: check the next alternative, when there is nothing left, insert default
                      MkAlt AltDefault () !(compileConstAltsToEqChain fn fc scrut as defAlt)
                      -- Match on True: call body
                    , MkAlt (AltLit (LitNumber LitNumInt 1)) () !(compileANF fn body)
                    ]
              ]
          ]
  compileConstAltsToEqChain fn fc scrut (MkAConstAlt (Str s) body :: as) defAlt = do
    stringLit             <- mkFreshSBinderStr LocalScope fc "stringLitBinder"
    stringEqResult        <- mkFreshSBinderStr LocalScope fc "stringEqResult"
    unusedBinder          <- mkFreshSBinderStr LocalScope fc "unusedBinder"
    stringEqResultUnboxed <- mkFreshSBinderStr LocalScope fc "stringEqResultUnboxed"
    sBinderId <- registerString fc s
    ((AlgDataCon [IntRep]) ** ti) <- dataConIdForPrimType IntType
      | wrongRep => coreFail $ InternalError $ "DataConId has wrong RepType: \{show (fc,fn,wrongRep)}"
    pure $
      -- Look up the string from the string table.
      StgCase
        PolyAlt
        !(createExtSTGPureApp
          (MkExtName "main" ["Idris", "Runtime", "String"] "mkStrFromAddr")
          [mkArgSg $ StgVarArg sBinderId])
        stringLit
        [ MkAlt AltDefault ()
          -- Call the strEq function
        $ StgCase
            (AlgAlt !(tyConIdForPrimType IntType))
            !(createExtSTGIOApp
                (MkExtName "main" ["Idris", "Runtime", "String"] "strEq")
                [ mkArgSg $ StgVarArg scrut
                , mkArgSg $ StgVarArg $ binderId stringLit
                ])
            stringEqResult
            [ MkAlt (AltDataCon (mkDataConIdSg ti)) stringEqResultUnboxed $
                -- Unbox the result
                StgCase
                  (PrimAlt IntRep)
                  (StgApp (binderId stringEqResultUnboxed) [] (SingleValue IntRep))
                  unusedBinder
                  [ -- Match on False: check the next alternative, when there is nothing left, insert default
                    MkAlt AltDefault () !(compileConstAltsToEqChain fn fc scrut as defAlt)
                    -- Match on True: call body
                  , MkAlt (AltLit (LitNumber LitNumInt 1)) () !(compileANF fn body)
                  ]
            ]
        ]
  compileConstAltsToEqChain fn fc scrut (MkAConstAlt _ _ :: as) defAlt
    = coreFail $ InternalError "Impossible, not expected non-string literal."

  -- This is a hack, this is not needed as the ArgList should always hold
  -- LiftedRep, when we compile Idris constructors, everything is represented
  -- as boxed values. This needs to be improved.
  failOnNonLiftedRep : (p : PrimRep) -> LocalVarBinder -> Core (SBinder (SingleValue p))
  failOnNonLiftedRep LiftedRep binder = pure binder
  failOnNonLiftedRep other     _      = coreFail $ InternalError "failOnNonLiftedRep: \{show other}"

  mkArgList
    :  Ref STGCtxt STGContext
    => FC -> Name.Name -> List AVar -> (ps : List PrimRep)
    -> Core (ArgList ps)
  mkArgList _ _ [] [] = pure []
  mkArgList f n (a::as) (r::rs) = do
    bdr <- lookupLocalVarBinder n a
    arg <- map (StgVarArg . binderId) $ failOnNonLiftedRep r bdr
    args <- mkArgList f n as rs
    pure (arg :: args)
  mkArgList _ _ _ _ = coreFail $ InternalError "mkArgList: inconsistent state."

  compileConArgs
    :  Ref STGCtxt STGContext
    => FC -> Name.Name -> List AVar -> (r : DataConRep)
    -> Core (StgConAppArgType r)
  compileConArgs fc funName _   (UnboxedTupleCon _)
    = coreFail $ InternalError "compileConArgs: UnboxedTupleCon"
  compileConArgs fc funName []  (AlgDataCon [])
    = pure ()
  compileConArgs fc funName [a] (AlgDataCon [r])
    = do bdr <- lookupLocalVarBinder funName a
         xid <- failOnNonLiftedRep r bdr
         pure $ StgVarArg $ binderId xid 
  compileConArgs fc funName (a0 :: a1 :: as) (AlgDataCon (r0 :: r1 :: rs))
    = mkArgList fc funName (a0 :: a1 :: as) (r0 :: r1 :: rs)
  compileConArgs _ _ _ _ = coreFail $ InternalError "compileConArgs: inconsistent state #2"

  createConAltBinders
    :  Ref STGCtxt STGContext
    => FC -> Core.Name.Name -> List Int -> (ps : List PrimRep)
    -> Core (BList ps)
  createConAltBinders fc funName [] [] = pure []
  createConAltBinders fc funName (i :: is) (p :: ps) = do
    bdr <- createLocalVarBinder fc funName (ALocal i)
    x  <- failOnNonLiftedRep p bdr
    xs <- createConAltBinders fc funName is ps
    pure (x :: xs)
  createConAltBinders fc funName is ps = coreFail $ InternalError $ "createConAltBinders found irregularities: " -- ++ show (is,ps)

  compileConAltArgs
    :  Ref STGCtxt STGContext
    => FC -> Core.Name.Name -> List Int -> (r : DataConRep)
    -> Core (AltDataConRepType r)
  compileConAltArgs fc funName _   (UnboxedTupleCon _)
    = coreFail $ InternalError $ "Encountered UnboxedTuple when compiling con alt: " ++ show (funName, fc)
  compileConAltArgs fc funName []  (AlgDataCon [])    = pure ()
  compileConAltArgs fc funName [i] (AlgDataCon [rep])
    = do bdr <- createLocalVarBinder fc funName (ALocal i)
         failOnNonLiftedRep rep bdr
  compileConAltArgs fc funName (i0 :: i1 :: is) (AlgDataCon (r0 :: r1 :: rs))
    = createConAltBinders fc funName (i0 :: i1 :: is) (r0 :: r1 :: rs)
  compileConAltArgs fc funname is alt
    = coreFail $ InternalError $ "Encountered irregularities " -- ++ show (is, alt)

  partial
  compileConAlt
    :  Ref Ctxt Defs
    => Ref STGCtxt STGContext
    => FC -> Core.Name.Name -> AConAlt
    -> Core (Alt (SingleValue LiftedRep) Core.stgRepType)
  compileConAlt fc funName c@(MkAConAlt name coninfo tag args body) = do
    stgDataCon  <- case coninfo of
                    TYCON => do
                      mkTyDataConId name
                    other => mkDataConIdStr name
    stgArgs     <- compileConAltArgs fc funName args (fst stgDataCon)
    stgBody     <- compileANF funName body
    pure $ MkAlt (AltDataCon stgDataCon) stgArgs stgBody

registerTopLevelFunctionBinder : Ref STGCtxt STGContext => (Core.Name.Name, ANFDef) -> Core ()
registerTopLevelFunctionBinder (funName, MkAFun args body)           = unitRet $ mkFunctionBinder emptyFC funName
registerTopLevelFunctionBinder (funName, MkAForeign css fargs rtype) = unitRet $ mkFunctionBinder emptyFC funName
registerTopLevelFunctionBinder _                                     = pure ()

partial
compileTopBinding
  :  Ref Ctxt Defs
  => Ref STGCtxt STGContext
  => (Core.Name.Name, ANFDef)
  -> Core (Maybe TopBinding)

compileTopBinding (funName,MkAFun args body) = do
  logLine Debug "TopBinding is being created: \{show funName}"
--  logLine $ "Compiling: " ++ show funName
  funNameBinder <- lookupFunctionBinder funName
  funArguments  <- traverse (map (MkDPair _) . createLocalVarBinder emptyFC funName . ALocal) args
  funBody       <- compileANF funName body
  rhs           <- pure $ StgRhsClosure ReEntrant funArguments funBody
  -- Question: Is Reentrant OK here?
  -- TODO: Calculate Rec or NonRec
  binding       <- pure $ StgRec [(funNameBinder,rhs)]
  -- Question: Is non-recursive good here? Test it.
  logLine Debug "TopBinding is created for: \{show funName}"
  dropLocalVars
  pure $ Just $ StgTopLifted binding

compileTopBinding (name,con@(MkACon aname tag arity)) = do
  logLine Debug $ "TopLevel MkACon: " ++ show (name, aname, con)
  -- Covered in the LearnDataTypes section
  pure Nothing

compileTopBinding (name,MkAForeign css fargs rtype) = do
  logLine Message "Foreign function - \{show name} \{show fargs} -> \{show rtype}"
  _  <- traverse (createLocalVarBinder emptyFC name) (createAVarList fargs)
  map Just $ foreign emptyFC css name fargs rtype
  where
    createAVarList : List a -> List AVar
    createAVarList xs = go 0 xs []
      where
        go : Int -> List a -> List AVar -> List AVar
        go n []        as = reverse as
        go n (x :: xs) as = go (n+1) xs (ALocal n :: as)  

compileTopBinding (name,MkAError body) = do
  logLine Error "Skipping error: \{show name}"
  pure Nothing

defineMain
  :  Ref STGCtxt STGContext
  => Core TopBinding
defineMain = do
  main     <- mainBinder
  voidArg  <- mainArgBinder
  progMain <- idrisMainEntryBinder
  pure
    $ topLevel main [mkSBinderSg voidArg]
    $ StgApp (binderId progMain) [] stgRepType

-- We compile only one enormous module
export
partial
compileModule
  :  Ref Ctxt Defs
  => Ref STGCtxt STGContext
  => List (Core.Name.Name, ANFDef)
  -> Core Module
compileModule anfDefs = do
  defineSoloDataType
  discoverADTs
  let phase              = "Main"
  let moduleUnitId       = MkUnitId "main"
  let name               = MkModuleName "Main" -- : ModuleName
  let sourceFilePath     = "some.idr" -- : String
  let foreignStubs       = NoStubs -- : ForeignStubs -- ???
  let hasForeignExported = False -- : Bool
  let dependency         = [] -- : List (UnitId, List ModuleName)
  traverse_ registerTopLevelFunctionBinder anfDefs
  mainTopBinding         <- defineMain
  compiledTopBindings    <- catMaybes <$> traverse compileTopBinding anfDefs
  stringTableBindings    <- StringTable.topLevelBinders
  let topBindings        = mainTopBinding ::
                           stringTableBindings ++
                           compiledTopBindings
  tyCons                 <- getDefinedDataTypes -- : List (UnitId, List (ModuleName, List tcBnd))
  let foreignFiles       = [] -- : List (ForeignSrcLang, FilePath)
  externalTopIds        <- genExtTopIds
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
