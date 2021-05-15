module Idris.Codegen.ExtSTG.ANFToSTG

import public Idris.Codegen.ExtSTG.Core

import Compiler.ANF
import Core.Context
import Core.Core
import Core.TT
import Data.IOArray
import Libraries.Data.IntMap
import Data.List
import Libraries.Data.StringMap
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
[+] FIX: Use StgCase instead of StgLet, otherwise strict semantics breaks.
[ ] Store defined/referred topLevelBinders
[+] Figure out how to use MkAlt with indexed types
[ ] Figure out the semantics for LazyReason in ANF Apps
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
compileAltConstant (BI i)  = pure $ LitNumber LitNumInt i -- ??? How to represent BIG integers ???
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
compileConstant (BI i)  = pure $ LitNumber LitNumInt i -- ??? How to represent BIG integers ???
compileConstant (B8 i)  = pure $ LitNumber LitNumWord $ cast i
compileConstant (B16 i) = pure $ LitNumber LitNumWord $ cast i
compileConstant (B32 i) = pure $ LitNumber LitNumWord $ cast i
compileConstant (B64 i) = pure $ LitNumber LitNumWord64 i
compileConstant (Ch c)  = pure $ LitChar c
compileConstant (Db d)  = pure $ LitDouble d
compileConstant c = coreFail $ InternalError $ "compileConstant " ++ show c

data ValueConstant : Constant -> Type where
  IntConstant    : ValueConstant (I x)
  BigIntConstant : ValueConstant (BI x)
  Byte8Constant  : ValueConstant (B8 x)
  Byte16Constant : ValueConstant (B16 x)
  Byte32Constant : ValueConstant (B32 x)
  Byte64Constant : ValueConstant (B64 x)
  CharConstant   : ValueConstant (Ch x)
  DoubleConstant : ValueConstant (Db x)
  WorldConstant  : ValueConstant WorldVal

checkValueConstant : (c : Constant) -> Maybe (ValueConstant c)
checkValueConstant (I _)    = Just IntConstant
checkValueConstant (BI _)   = Just BigIntConstant
checkValueConstant (B8 _)   = Just Byte8Constant
checkValueConstant (B16 _)  = Just Byte16Constant
checkValueConstant (B32 _)  = Just Byte32Constant
checkValueConstant (B64 _)  = Just Byte64Constant
checkValueConstant (Ch _)   = Just CharConstant
checkValueConstant (Db _)   = Just DoubleConstant
checkValueConstant WorldVal = Just WorldConstant
checkValueConstant _        = Nothing

checkValueConstantM : (c : Constant) -> Core (ValueConstant c)
checkValueConstantM c = case checkValueConstant c of
  Nothing => coreFail $ InternalError $ "checkValueConstantM: " ++ show c ++ " is not a value constant."
  Just vc => pure vc

valueConstantPrimReps : (c : Constant) -> ValueConstant c => List PrimRep
valueConstantPrimReps (I _)    = [IntRep]
valueConstantPrimReps (BI _)   = [IntRep]
valueConstantPrimReps (B8 _)   = [Word8Rep]
valueConstantPrimReps (B16 _)  = [Word16Rep]
valueConstantPrimReps (B32 _)  = [Word32Rep]
valueConstantPrimReps (B64 _)  = [Word64Rep]
valueConstantPrimReps (Ch _)   = [Word8Rep]
valueConstantPrimReps (Db _)   = [DoubleRep]
valueConstantPrimReps WorldVal = []

valueConstantAlgDataCon : (c : Constant) -> ValueConstant c => DataConRep
valueConstantAlgDataCon c = AlgDataCon (valueConstantPrimReps c)

valueConstantName : (c : Constant) -> ValueConstant c => String
valueConstantName (I _)    = "I#"
valueConstantName (BI _)   = "GMPInt"
valueConstantName (B8 _)   = "W8#"
valueConstantName (B16 _)  = "W16#"
valueConstantName (B32 _)  = "W32#"
valueConstantName (B64 _)  = "W64#"
valueConstantName (Ch _)   = "C#"
valueConstantName (Db _)   = "D#"
valueConstantName WorldVal = "IdrWorld"

||| Determine the Data constructor for the boxed primitive value.
||| Used in creating PrimVal
|||
||| The name of terms should coincide the ones that are defined in GHC's ecosystem. This
||| would make the transition easier, I hope.
dataConIdForValueConstant
  :  DataTypeMapRef => UniqueMapRef => Ref Counter Int
  => (c : Constant) -> ValueConstant c => Core DataConIdSg
dataConIdForValueConstant c = mkDataConIdStr (valueConstantName c)

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

createAlternatives
  : (r : RepType) -> List (Lit, Expr Core.stgRepType) -> Maybe (List (Alt r Core.stgRepType))
createAlternatives r [] = Just []
createAlternatives r ((l,b) :: ls) = do
  Refl <- decLitRepType l r
  a    <- decAltLit l
  map ((MkAlt (AltLit l) () b) ::) $ createAlternatives r ls

mutual
  compileANF
    : UniqueMapRef => Ref Counter Int => Ref ADTs ADTMap => StringTableRef
    => Ref Ctxt Defs => DataTypeMapRef
    => Core.Name.Name -> ANF
    -> Core (Expr Core.stgRepType)
  compileANF funName (AV fc var)
    = pure $ StgApp !(mkBinderIdVar fc funName Core.stgRepType var) [] stgRepType

  -- TODO: Figure out the semantics for LazyReason
  compileANF funToCompile (AAppName fc lazyReason funToCall args)
    = pure $ StgApp !(mkBinderIdName funToCall)
                    !(traverse (mkStgArg fc funToCompile) args)
                    stgRepType

  compileANF funToCompile (AUnderApp fc funToCall _ args)
    = pure $ StgApp !(mkBinderIdName funToCall)
                    !(traverse (mkStgArg fc funToCompile) args)
                    stgRepType

  -- TODO: Figure out the semantics for LazyReason
  compileANF funName (AApp fc lazyReason closure arg)
    = pure $ StgApp !(mkBinderIdVar fc funName Core.stgRepType closure)
                    [!(mkStgArg fc funName arg)]
                    stgRepType

  compileANF funName (ALet fc var expr body) = do
    pure
      $ StgCase
          PolyAlt -- It could be ForceUnbox, and only AltDefault should be used in Strict Let
          !(compileANF funName expr)
          !(mkSBinderLocal fc funName var)
          [ MkAlt AltDefault () !(compileANF funName body) ]

  -- TODO: Implement
  compileANF _ acon@(ACon fc name Nothing args) = do
    -- Types probably will be represented with one STG TyCon and DataCon
    -- for every type.
    coreFail $ InternalError $ "Figure out how to represent a type as a value!"

  compileANF funName acon@(ACon fc name (Just tag) args) = do
    -- Lookup the constructor based on the name.
    -- The tag information is not relevant here.
    -- Args are variables
    (rep ** dataConId) <- mkDataConId name
    conArgs <- compileConArgs fc funName args rep
    pure $ StgConApp dataConId conArgs

  -- TODO: Figure out the semantics for LazyReason
  compileANF funName (AOp fc lazyReason prim args)
    = compilePrimOp fc funName prim args

  -- TODO: Implement
  -- TODO: Figure out the semantics for LazyReason
  compileANF _ aext@(AExtPrim _ lazyReason name args) = do
    logLine $ "To be implemented: " ++ show aext
    pure
      $ StgApp (!(mkBinderIdStr STRING_FROM_ADDR))
               [ mkArgSg $ StgLitArg $ LitString $ "AExtPrim " ++ show name ++ " " ++ show args
               ]
               (SingleValue LiftedRep)

  compileANF funName (AConCase fc scrutinee alts mdef) = do
    -- Compute the alt-type
    tyCon <- do
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
      case mapMaybe snd namesAndTyCons of
        []  => coreFail $ InternalError $ "No type constructor is found for: " ++ show fc
        [t] => pure $ STyCon.Id t
        ts  => case nub (map STyCon.Id ts) of
                 []  => coreFail $ InternalError "Impossible case in compile AConCase"
                 [t] => pure t
                 ts  => coreFail $ InternalError $ "More than TyCon found for: " ++ show fc
    scrutBinder <- mkBinderIdVar fc funName Core.stgRepType scrutinee
    let stgScrutinee = StgApp scrutBinder [] stgRepType
    caseBinder <- mkFreshSBinderStr LocalScope fc "conCaseBinder"
    stgDefAlt <- maybe
      (pure [])
      (\x => do
        stgBody <- compileANF funName x
        pure [MkAlt AltDefault () stgBody])
      mdef
    stgAlts <- traverse (compileConAlt fc funName) alts
    pure $ StgCase (AlgAlt tyCon) stgScrutinee caseBinder (stgDefAlt ++ stgAlts)

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
        [tyCon] <- map nub $ traverse (tyConIdForValueConstant fc . getAltConstant) alts
          | ts => coreFail $ InternalError $ "Constant case found " ++ show (length ts) ++ " type constructors in " ++ show fc
        let altConstant = getAltConstant alt
        valueConst <- checkValueConstantM altConstant
        ((AlgDataCon [rep]) ** dtCon) <- dataConIdForValueConstant altConstant
          | wrongRep => coreFail $ InternalError $ "DataConId has wrong RepType: " ++ show (fc, funName, wrongRep)
        primVal <- mkFreshSBinderRepStr LocalScope (SingleValue rep) fc "primVal"
        litBodies
          <- traverse
              \case
                (MkAConstAlt c b) => do
                  lit <- compileAltConstant c
                  body <- compileANF funName b
                  pure (lit, body)
              alts
        let Just stgAlts = createAlternatives (SingleValue rep) litBodies
            | Nothing => coreFail
                       $ InternalError
                       $ "Representation in literal types were different then expexted:" ++ show (rep, map fst litBodies)
        stgDefAlt <- maybe
          (pure [])
          (\x => do
            stgBody <- compileANF funName x
            pure [the (Alt (SingleValue rep) Core.stgRepType) (MkAlt AltDefault () stgBody)])
          mdef
        -- stgAlts <- traverse (compileConstAlt funName) alts
        pure
          $ StgCase
              (AlgAlt tyCon)
              (StgApp !(mkBinderIdVar fc funName Core.stgRepType scrutinee) [] stgRepType)
              !nonused
              [ MkAlt (AltDataCon (mkDataConIdSg dtCon)) primVal
              $ StgCase
                  (PrimAlt rep)
                  (StgApp (binderId primVal) [] (SingleValue rep))
                  !(nonusedRep (SingleValue rep))
                  (stgDefAlt ++ stgAlts)
              ]

      -- String alts
      (strAlts, []) => do

        mStgDef <- traverseOpt (compileANF funName) mdef
        scrutBinder <- mkBinderIdVar fc funName Core.stgRepType scrutinee

        -- TODO: Make this typesafe
        let caseChain : List AConstAlt -> Core (Expr Core.stgRepType)
            caseChain [] = coreFail $ InternalError "Impossible, empty string alternatives."
            caseChain [MkAConstAlt (Str s) body] = do
              stringLit             <- mkFreshSBinderStr LocalScope fc "stringLitBinder"
              stringEqResult        <- mkFreshSBinderStr LocalScope fc "stringEqResult"
              unusedBinder          <- mkFreshSBinderRepStr LocalScope (SingleValue IntRep) fc "unusedBinder"
              stringEqResultUnboxed <- mkFreshSBinderRepStr LocalScope (SingleValue IntRep) fc "stringEqResultUnboxed"
              sBinderId <- registerString fc s
              ((AlgDataCon [IntRep]) ** ti) <- dataConIdForConstant IntType
                | wrongRep => coreFail $ InternalError $ "DataConId has wrong RepType: " ++ show (fc, funName, wrongRep)
              pure $
                -- Look up the string from the string table.
                StgCase
                  (AlgAlt !idrisStringTyConId)
                  (StgApp !stringFromAddrBinderId2 [mkArgSg $ StgVarArg sBinderId] (SingleValue LiftedRep))
                  stringLit
                  [ MkAlt AltDefault ()
                    -- Call the strEq function
                  $ StgCase
                      (AlgAlt !(tyConIdForConstant IntType))
                      (StgApp !(mkBinderIdStr "Idris.String.strEq")
                              [ mkArgSg $ StgVarArg $ scrutBinder
                              , mkArgSg $ StgVarArg $ binderId stringLit ]
                              stgRepType)
                      stringEqResult
                      [ MkAlt (AltDataCon (mkDataConIdSg ti)) stringEqResultUnboxed $
                              -- Unbox the result
                              StgCase
                                (PrimAlt IntRep)
                                (StgApp (binderId stringEqResultUnboxed) [] (SingleValue IntRep))
                                unusedBinder
                                $ catMaybes
                                  [ -- Match on False: call default
                                    (MkAlt AltDefault ()) <$> mStgDef
                                  , -- Match on True: call body
                                    Just $ MkAlt (AltLit (LitNumber LitNumInt 1)) ()
                                                 !(compileANF funName body)
                                  ]
                      ]
                  ]
            caseChain ((MkAConstAlt (Str s) body) :: rest) = do
              stringLit             <- mkFreshSBinderStr LocalScope fc "stringLitBinder"
              stringEqResult        <- mkFreshSBinderStr LocalScope fc "stringEqResult"
              unusedBinder          <- mkFreshSBinderRepStr LocalScope (SingleValue IntRep) fc "unusedBinder"
              stringEqResultUnboxed <- mkFreshSBinderRepStr LocalScope (SingleValue IntRep) fc "stringEqResultUnboxed"
              sBinderId <- registerString fc s
              ((AlgDataCon [IntRep]) ** ti) <- dataConIdForConstant IntType
                | wrongRep => coreFail $ InternalError $ "DataConId has wrong RepType: " ++ show (fc, funName, wrongRep)
              pure $
                -- Look up the string from the string table.
                StgCase
                  (AlgAlt !idrisStringTyConId)
                  (StgApp !stringFromAddrBinderId2 [mkArgSg $ StgVarArg sBinderId] (SingleValue LiftedRep))
                  stringLit
                  [ MkAlt AltDefault ()
                    -- Call the strEq function
                  $ StgCase
                      (AlgAlt !(tyConIdForConstant IntType))
                      (StgApp !(mkBinderIdStr "Idris.String.strEq")
                              [ mkArgSg $ StgVarArg scrutBinder
                              , mkArgSg $ StgVarArg $ binderId stringLit
                              ]
                              stgRepType)
                      stringEqResult
                      [ MkAlt (AltDataCon (mkDataConIdSg ti)) stringEqResultUnboxed $
                              -- Unbox the result
                              StgCase
                                (PrimAlt IntRep)
                                (StgApp (binderId stringEqResultUnboxed) [] (SingleValue IntRep))
                                unusedBinder
                                $ -- Match on False: check the next alternative
                                  [ MkAlt AltDefault () !(caseChain rest)
                                    -- Match on True: call body
                                  , MkAlt (AltLit (LitNumber LitNumInt 1)) ()
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
    stringAddress  <- mkFreshSBinderRepStr LocalScope (SingleValue AddrRep) fc "stringPrimVal"
    pure $ StgCase
            (PrimAlt AddrRep)
            (StgApp topLevelBinder [] (SingleValue AddrRep)) -- TODO: Is this right?
            stringAddress
            [ MkAlt AltDefault ()
            $ StgApp
                (snd !stringFromAddrBinderId)
                [ mkArgSg $ StgVarArg $ binderId stringAddress ]
                stgRepType
            ]

  compileANF _ (APrimVal fc WorldVal) = do
    (AlgDataCon [] ** dataConId) <- dataConIdForValueConstant WorldVal
      | other => coreFail $ InternalError $ show (fc,WorldVal) ++ " has different representation: " ++ show other
    pure $ StgConApp dataConId ()

  compileANF _ (APrimVal fc c) = do
    -- TODO: Handle WorldVal
    valueConstant <- checkValueConstantM c
    (AlgDataCon [rep] ** dataConId) <- dataConIdForValueConstant c
      | other => coreFail $ InternalError $ show (fc,c) ++ " has different representation: " ++ show other
    lit <- compileConstant c
    same <- checkSemiDecEq ("compileANF " ++ show (fc, c)) (SingleValue rep) (litRepType lit)
    pure $ StgConApp dataConId (StgLitArg lit)

  compileANF _ (AErased fc)
    = do sbinder <- mkSBinderStr fc ERASED_TOPLEVEL_NAME
         pure $ StgApp (binderId sbinder) [] (SingleValue LiftedRep)

  -- TODO: Implement: Use Crash primop. errorBlech2 for reporting error ("%s", msg)
  compileANF _ ac@(ACrash _ msg) = do
    logLine $ "To be implemented: " ++ show ac
    pure
      $ StgApp (!(mkBinderIdStr STRING_FROM_ADDR))
               [ mkArgSg $ StgLitArg $ LitString $ "ACrash " ++ msg
               ]
               (SingleValue LiftedRep)

  mkArgList
    :  UniqueMapRef => Ref Counter Int => Ref ADTs ADTMap
    => FC -> Name.Name -> List AVar -> (ps : List PrimRep) -> Core (ArgList ps)
  mkArgList _ _ [] [] = pure []
  mkArgList f n (a::as) (r::rs) = do
    arg <- map StgVarArg $ mkBinderIdVar f n (SingleValue r) a
    args <- mkArgList f n as rs
    pure (arg :: args)
  mkArgList _ _ _ _ = coreFail $ InternalError "mkArgList: inconsistent state."

  compileConArgs
    :  UniqueMapRef => Ref Counter Int => Ref ADTs ADTMap
    => FC -> Name.Name -> List AVar -> (r : DataConRep)
    -> Core (StgConAppArgType r)
  compileConArgs fc funName _   (UnboxedTupleCon _)
    = coreFail $ InternalError "compileConArgs: UnboxedTupleCon"
  compileConArgs fc funName []  (AlgDataCon [])
    = pure ()
  compileConArgs fc funName [a] (AlgDataCon [r])
    = StgVarArg <$> mkBinderIdVar fc funName (SingleValue r) a
  compileConArgs fc funName (a0 :: a1 :: as) (AlgDataCon (r0 :: r1 :: rs))
    = mkArgList fc funName (a0 :: a1 :: as) (r0 :: r1 :: rs)
  compileConArgs _ _ _ _ = coreFail $ InternalError "compileConArgs: inconsistent state #2"

  createConAltBinders
    :  UniqueMapRef => Ref Counter Int => Ref ADTs ADTMap
    => FC -> Core.Name.Name -> List Int -> (ps : List PrimRep)
    -> Core (BList ps)
  createConAltBinders fc funName [] [] = pure []
  createConAltBinders fc funName (i :: is) (p :: ps) = do
    x <- mkSBinderRepLocal (SingleValue p) fc funName i
    xs <- createConAltBinders fc funName is ps
    pure (x :: xs)
  createConAltBinders fc funName is ps = coreFail $ InternalError $ "createConAltBinders found irregularities: " -- ++ show (is,ps)

  compileConAltArgs
    :  UniqueMapRef => Ref Counter Int => Ref ADTs ADTMap
    => FC -> Core.Name.Name -> List Int -> (r : DataConRep)
    -> Core (DataConRepType r)
  compileConAltArgs fc funName _   (UnboxedTupleCon _)
    = coreFail $ InternalError $ "Encountered UnboxedTuple when compiling con alt: " ++ show (funName, fc)
  compileConAltArgs fc funName []  (AlgDataCon [])    = pure ()
  compileConAltArgs fc funName [i] (AlgDataCon [rep]) = mkSBinderRepLocal (SingleValue rep) fc funName i
  compileConAltArgs fc funName (i0 :: i1 :: is) (AlgDataCon (r0 :: r1 :: rs))
    = createConAltBinders fc funName (i0 :: i1 :: is) (r0 :: r1 :: rs)
  compileConAltArgs fc funname is alt
    = coreFail $ InternalError $ "Encountered irregularities " -- ++ show (is, alt)

  compileConAlt
    : UniqueMapRef => Ref Counter Int => Ref ADTs ADTMap
    => StringTableRef => Ref Ctxt Defs => DataTypeMapRef
    => FC -> Core.Name.Name -> AConAlt
    -> Core (Alt (SingleValue LiftedRep) Core.stgRepType)
  compileConAlt fc funName c@(MkAConAlt name Nothing args body) = do
    coreFail $ InternalError $ "Figure out how to do pattern match on type: " ++ show name
  compileConAlt fc funName c@(MkAConAlt name (Just tag) args body) = do
    stgBody     <- compileANF funName body
    stgDataCon  <- mkDataConId name
    stgArgs     <- compileConAltArgs fc funName args (fst stgDataCon)
    pure $ MkAlt (AltDataCon stgDataCon) stgArgs stgBody

compileTopBinding
  :  UniqueMapRef
  => Ref Counter Int
  => Ref Ctxt Defs
  => StringTableRef
  => Ref ADTs ADTMap
  => Ref ExternalBinder ExtBindMap
  => DataTypeMapRef
  => (Core.Name.Name, ANFDef)
  -> Core (Maybe TopBinding)
compileTopBinding (funName,MkAFun args body) = do
--  logLine $ "Compiling: " ++ show funName
  funBody       <- compileANF funName body
  funArguments  <- traverse (map mkSBinderSg . mkSBinderLocal emptyFC funName) args
  funNameBinder <- mkSBinderName emptyFC funName
  rhs           <- pure $ StgRhsClosure ReEntrant funArguments funBody
  -- Question: Is Reentrant OK here?
  -- TODO: Calculate Rec or NonRec
  binding       <- pure $ StgRec [(funNameBinder,rhs)]
  -- Question: Is non-recursive good here? Test it.
  pure $ Just $ StgTopLifted binding
compileTopBinding (name,con@(MkACon aname tag arity)) = do
  -- logLine $ "TopLevel MkACon: " ++ show (name, aname, con)
  -- Covered in the LearnDataTypes section
  pure Nothing
compileTopBinding (name,MkAForeign css fargs rtype) = do
  -- logLine $ "Found foreign: " ++ show name
  map Just $ foreign name css fargs rtype
compileTopBinding (name,MkAError body) = do
  logLine $ "Skipping error: " ++ show name
  pure Nothing

groupExternalTopIds
  :  List (UnitId, ModuleName, SBinderSg)
  -> List (UnitId, List (ModuleName, List SBinderSg))
groupExternalTopIds = resultList . unionsMap . map singletonMap
  where
    EntryMap : Type
    EntryMap = StringMap (StringMap (List SBinderSg))

    resultList : EntryMap -> List (UnitId, List (ModuleName, List SBinderSg))
    resultList
      = map (bimap MkUnitId (map (mapFst MkModuleName) . toList))
      . toList

    unionsMap : List EntryMap -> EntryMap
    unionsMap = foldl (mergeWith (mergeWith (++))) empty

    singletonMap : (UnitId, ModuleName, SBinderSg) -> EntryMap
    singletonMap (MkUnitId n, MkModuleName m, sbinder) = singleton n (singleton m [sbinder])

partitionBy : (a -> Either b c) -> List a -> (List b, List c)
partitionBy f [] = ([], [])
partitionBy f (x :: xs) =
  let (bs, cs) = partitionBy f xs
  in case f x of
      (Left b)  => (b :: bs, cs)
      (Right c) => (bs, c :: cs)

defineMain : UniqueMapRef => Ref Counter Int => Core TopBinding
defineMain = do
  main <- mkSBinderExtId emptyFC "main"
  voidArg <- mkSBinderRepLocalStr (SingleValue VoidRep) "mainVoidArg"
  progMain <- mkSBinderTopLevel "{__mainExpression:0}"
  pure
    $ topLevel main [mkSBinderSg voidArg]
    $ StgApp (binderId progMain) [] stgRepType

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
  registerHardcodedExtTopIds
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
  externalTopIds0        <- genExtTopIds
  let externalTopIds     = groupExternalTopIds externalTopIds0
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

