module Idris.Codegen.ExtSTG.TTtoSTG -- where

import Prelude
import Compiler.ANF
import Idris.Codegen.ExtSTG.STG
import Core.Core
import Core.Context
import Core.TT
import Data.Vect
import Data.List
import Data.StringMap


data Counter : Type where

export
mkCounter : Core (Ref Counter Int)
mkCounter = newRef Counter 0

data Uniques : Type where

export
UniqueMap : Type
UniqueMap = StringMap Unique

export
mkUniques : Core (Ref Uniques UniqueMap)
mkUniques = newRef Uniques empty

mkUnique : {auto _ : Ref Counter Int} -> Core Unique
mkUnique = do
  x <- get Counter
  let u = MkUnique 'i' x
  put Counter (x + 1)
  pure u

getUnique
  :  {auto _ : Ref Uniques UniqueMap}
  -> {auto _ : Ref Counter Int}
  -> String
  -> Core Unique
getUnique name = do
  x <- get Uniques
  case lookup name x of
    Nothing => do
      u <- mkUnique
      put Uniques (insert name u x)
      pure u
    Just u => do
      pure u

stgRepType : RepType
stgRepType = SingleValue UnliftedRep

-- TODO: Add FC
mkSBinder
  :  {auto _ : Ref Uniques UniqueMap}
  -> {auto _ : Ref Counter Int}
  -> Bool -> String
  -> Core SBinder
mkSBinder topLevel binderName = do
  binderId <- MkBinderId <$> getUnique binderName
  let typeSig = "mkSBinder: typeSig"
  let scope   = GlobalScope
  let details = VanillaId
  let info    = "mkSBinder: IdInfo"
  let defLoc  = SsUnhelpfulSpan "<no location>"
  pure $ MkSBinder
    binderName
    binderId
    stgRepType
    typeSig
    scope
    details
    info
    defLoc

mkSBinderTyCon
  :  {auto _ : Ref Uniques UniqueMap}
  -> {auto _ : Ref Counter Int}
  -> String -- TODO : Constant
  -> Core SBinder
mkSBinderTyCon = mkSBinder False

mkSBinderLocal
  :  {auto _ : Ref Uniques UniqueMap}
  -> {auto _ : Ref Counter Int}
  -> Core.Name.Name -> Int
  -> Core SBinder
mkSBinderLocal n x = mkSBinder False (show n ++ ":" ++ show x)

mkSBinderName
  :  {auto _ : Ref Uniques UniqueMap}
  -> {auto _ : Ref Counter Int}
  -> Core.Name.Name
  -> Core SBinder
mkSBinderName n = mkSBinder True $ show n

mkSBinderStr
  :  {auto _ : Ref Uniques UniqueMap}
  -> {auto _ : Ref Counter Int}
  -> String
  -> Core SBinder
mkSBinderStr = mkSBinder True

--export
--addData : {auto c : Ref Ctxt Defs} ->
--          List Name -> Visibility -> Int -> DataDef -> Core Int
--addData vars vis tidx (MkData (MkCon dfc tyn arity tycon) datacons)
--public export
--data DataDef : Type where
--     MkData : (tycon : Constructor) -> (datacons : List Constructor) ->
--              DataDef

mkSBinderVar
  :  {auto _ : Ref Uniques UniqueMap}
  -> {auto _ : Ref Counter Int}
  -> Core.Name.Name -> AVar
  -> Core SBinder
mkSBinderVar n (ALocal x) = mkSBinder False (show n ++ ":" ++ show x)
mkSBinderVar n ANull      = coreFail $ UserError "mkSBinderVar n ANull" -- mkSBinder False (show n ++ ":erased")"

mkBinderIdVar
  :  {auto _ : Ref Uniques UniqueMap}
  -> {auto _ : Ref Counter Int}
  -> Core.Name.Name -> AVar
  -> Core BinderId
mkBinderIdVar n (ALocal x) = MkBinderId <$> getUnique (show n ++ ":" ++ show x)
mkBinderIdVar n ANull      = coreFail $ UserError "mkBinderIdVar n ANull" -- MkBinderId <$> getUnique (show n ++ ":erased")

compileDataConId
  :  {auto _ : Ref Uniques UniqueMap}
  -> {auto _ : Ref Counter Int}
  -> Maybe Int -- What does Nothing mean here for DataConId
  -> Core DataConId
compileDataConId Nothing  = coreFail $ UserError "MkDataConId <$> mkUnique"
compileDataConId (Just t) = MkDataConId <$> getUnique ("DataCon:" ++ show t)

constantToTypeRep : Constant -> Maybe RepType
constantToTypeRep IntType     = Just $ SingleValue LiftedRep
constantToTypeRep IntegerType = Just $ SingleValue LiftedRep
constantToTypeRep Bits8Type   = Just $ SingleValue LiftedRep
constantToTypeRep Bits16Type  = Just $ SingleValue LiftedRep
constantToTypeRep Bits32Type  = Just $ SingleValue LiftedRep
constantToTypeRep Bits64Type  = Just $ SingleValue LiftedRep
constantToTypeRep StringType  = Just $ SingleValue LiftedRep
constantToTypeRep CharType    = Just $ SingleValue LiftedRep
constantToTypeRep DoubleType  = Just $ SingleValue LiftedRep
constantToTypeRep _           = Nothing

constantToPrimRep : Constant -> Maybe PrimRep
constantToPrimRep IntType     = Just IntRep
constantToPrimRep IntegerType = Just IntRep -- TODO: This is not the right representation for integer
constantToPrimRep Bits8Type   = Just Word8Rep
constantToPrimRep Bits16Type  = Just Word16Rep
constantToPrimRep Bits32Type  = Just Word32Rep
constantToPrimRep Bits64Type  = Just Word64Rep
constantToPrimRep DoubleType  = Just DoubleRep
constantToPrimRep other       = Nothing

{-
 * Create module with the boxed types and use in TyCons fields
   enumerating all the primitive types.
 * Use the DataConIds an the AltDataCon that are defined in the
   TyCon field.
 * Parameters needs to be unboxed one-by-one in cases.
-}

--public export
--record STyCon where
--  constructor MkSTyCon
--  Name     : Name
--  Id       : TyConId
--  DataCons : (List SDataCon)
--  DefLoc   : SrcSpan

tyConIdForConstant
  :  {auto _ : Ref Uniques UniqueMap}
  -> {auto _ : Ref Counter Int}
  -> Constant
  -> Core TyConId
tyConIdForConstant IntType     = MkTyConId <$> getUnique "type:IdrInt"
tyConIdForConstant IntegerType = MkTyConId <$> getUnique "type:IdrInteger"
tyConIdForConstant Bits8Type   = MkTyConId <$> getUnique "type:IdrBits8"
tyConIdForConstant Bits16Type  = MkTyConId <$> getUnique "type:IdrBits16"
tyConIdForConstant Bits32Type  = MkTyConId <$> getUnique "type:IdrBits32"
tyConIdForConstant Bits64Type  = MkTyConId <$> getUnique "type:IdrBits64"
tyConIdForConstant StringType  = MkTyConId <$> getUnique "type:IdrString"
tyConIdForConstant CharType    = MkTyConId <$> getUnique "type:IdrChar"
tyConIdForConstant DoubleType  = MkTyConId <$> getUnique "type:IdrDouble"
tyConIdForConstant other = coreFail $ UserError $ "No type constructor for " ++ show other

dataConIdForConstant
  :  {auto _ : Ref Uniques UniqueMap}
  -> {auto _ : Ref Counter Int}
  -> Constant
  -> Core DataConId
dataConIdForConstant IntType     = MkDataConId <$> getUnique "IdrInt"
dataConIdForConstant IntegerType = MkDataConId <$> getUnique "IdrInteger"
dataConIdForConstant Bits8Type   = MkDataConId <$> getUnique "IdrBits8"
dataConIdForConstant Bits16Type  = MkDataConId <$> getUnique "IdrBits16"
dataConIdForConstant Bits32Type  = MkDataConId <$> getUnique "IdrBits32"
dataConIdForConstant Bits64Type  = MkDataConId <$> getUnique "IdrBits64"
dataConIdForConstant StringType  = MkDataConId <$> getUnique "IdrString"
dataConIdForConstant CharType    = MkDataConId <$> getUnique "IdrChar"
dataConIdForConstant DoubleType  = MkDataConId <$> getUnique "IdrDouble"
dataConIdForConstant other = coreFail $ UserError $ "No data constructor for " ++ show other

||| Create the primitive types section in the STG module.
|||
||| Idris primitive types are represented as boxed values in STG, with a datatype with one constructor.
||| Eg: data IdrInt = IdrInt #IntRep
createPrimitiveTypes
  :  {auto _ : Ref Uniques UniqueMap}
  -> {auto _ : Ref Counter Int}
  -> Core (List STyCon)
createPrimitiveTypes = pure
  [ MkSTyCon "IdrInt" !(MkTyConId <$> getUnique "type:IdrInt")
                      [ MkSDataCon "IdrInt"
                         !(MkDataConId <$> getUnique "IdrInt")
                         (AlgDataCon [IntRep])
                         !(mkSBinderStr "mkIdrInt")
                         (SsUnhelpfulSpan "<no location>")
                      ]
                      (SsUnhelpfulSpan "<no location>")
  , MkSTyCon "IdrInteger" !(MkTyConId <$> getUnique "type:IdrInteger")
                          [ MkSDataCon "IdrInteger"
                             !(MkDataConId <$> getUnique "IdrInteger")
                             (AlgDataCon [IntRep])
                             !(mkSBinderStr "mkIdrInteger")
                             (SsUnhelpfulSpan "<no location>")
                          ]
                          (SsUnhelpfulSpan "<no location>")
  , MkSTyCon "IdrBits8" !(MkTyConId <$> getUnique "type:IdrBits8")
                        [ MkSDataCon "IdrBits8"
                            !(MkDataConId <$> getUnique "IdrBits8")
                            (AlgDataCon [WordRep])
                            !(mkSBinderStr "mkIdrBits8")
                            (SsUnhelpfulSpan "<no location>")
                        ]
                        (SsUnhelpfulSpan "<no location>")
  , MkSTyCon "IdrBits16" !(MkTyConId <$> getUnique "type:IdrBits16")
                         [ MkSDataCon "IdrBits16"
                             !(MkDataConId <$> getUnique "IdrBits16")
                             (AlgDataCon [WordRep])
                             !(mkSBinderStr "mkIdrBits16")
                             (SsUnhelpfulSpan "<no location>")
                         ]
                         (SsUnhelpfulSpan "<no location>")
  , MkSTyCon "IdrBits32" !(MkTyConId <$> getUnique "type:IdrBits32")
                         [ MkSDataCon "IdrBits32"
                             !(MkDataConId <$> getUnique "IdrBits32")
                             (AlgDataCon [WordRep])
                             !(mkSBinderStr "mkIdrBits32")
                             (SsUnhelpfulSpan "<no location>")
                         ]
                         (SsUnhelpfulSpan "<no location>")
  , MkSTyCon "IdrBits64" !(MkTyConId <$> getUnique "IdrBits64")
                         [ MkSDataCon "IdrBits64"
                             !(MkDataConId <$> getUnique "datacon:IdrBits64")
                             (AlgDataCon [WordRep])
                             !(mkSBinderStr "mkIdrBits64")
                             (SsUnhelpfulSpan "<no location>")
                         ]
                         (SsUnhelpfulSpan "<no location>")
  , MkSTyCon "IdrChar" !(MkTyConId <$> getUnique "type:IdrChar")
                       [ MkSDataCon "IdrChar"
                           !(MkDataConId <$> getUnique "datacon:IdrChar")
                           (AlgDataCon [IntRep]) -- ???
                           !(mkSBinderStr "mkIdrChar")
                           (SsUnhelpfulSpan "<no location>")
                       ]
                       (SsUnhelpfulSpan "<no location>")
  , MkSTyCon "IdrDouble" !(MkTyConId <$> getUnique "type:IdrDouble")
                         [ MkSDataCon "IdrDouble"
                            !(MkDataConId <$> getUnique "datacon:IdrDouble")
                            (AlgDataCon [DoubleRep])
                            !(mkSBinderStr "mkIdrDouble")
                            (SsUnhelpfulSpan "<no location>")
                         ]
                         (SsUnhelpfulSpan "<no location>")
  ]

createPrimitiveTypesSection
  :  {auto _ : Ref Uniques UniqueMap}
  -> {auto _ : Ref Counter Int}
  -> Core (List (UnitId, List (ModuleName, List STyCon)))
createPrimitiveTypesSection = pure
  [ ( MkUnitId "SomeUnit" -- TODO
    , [ ( MkModuleName "SomeModuleName" -- TODO
        , !createPrimitiveTypes
        )
      ]
    )
  ]

compilePrimOp
  :  {auto _ : Ref Uniques UniqueMap}
  -> {auto _ : Ref Counter Int}
  -> Core.Name.Name -> PrimFn arity -> Vect arity AVar
  -> Core SExpr
compilePrimOp {arity=2} n (Add ty) as = do
  op <- case ty of
    IntType     => pure $ StgPrimOp "+#"
    IntegerType => pure $ StgPrimOp "Add IntegerType" -- TODO: No GMP Integer 
    Bits8Type   => pure $ StgPrimOp "plusWord#"
    Bits16Type  => pure $ StgPrimOp "plusWord#"
    Bits32Type  => pure $ StgPrimOp "plusWord#"
    Bits64Type  => pure $ StgPrimOp "plusWord#"
    DoubleType  => pure $ StgPrimOp "+##"
    _           => throw $ InternalError $ "Type is not for adding: " ++ show ty
  [arg1, arg2] <- traverseVect (mkBinderIdVar n) as
  let Just resultType = constantToTypeRep ty
      | Nothing => throw $ InternalError $ "Type is not convertible: " ++ show ty
  let Just primRep = constantToPrimRep ty
      | Nothing => throw $ InternalError $ "Type is not primitive convertible: " ++ show ty
  let resultTypeName = Nothing
  pure $ StgCase
          (StgApp arg1 [] resultType ("?","?","?"))
          !(mkSBinderLocal n 3)
          !(AlgAlt <$> tyConIdForConstant ty)
          [ MkAlt !(AltDataCon <$> dataConIdForConstant ty) [!(mkSBinderLocal n 4)]
             (StgCase
                (StgApp arg2 [] resultType ("?","?","?"))
                !(mkSBinderLocal n 4)
                !(AlgAlt <$> tyConIdForConstant ty)
                [ MkAlt !(AltDataCon <$> dataConIdForConstant ty) [!(mkSBinderLocal n 5)]
                    (StgCase
                      (StgOpApp op
                        [ !(StgVarArg <$> mkBinderIdVar n (ALocal 4))
                        , !(StgVarArg <$> mkBinderIdVar n (ALocal 5))
                        ]
                        resultType
                        resultTypeName)
                      !(mkSBinderLocal n 6)
                      (PrimAlt primRep)
                      [ MkAlt AltDefault []
                          (StgConApp !(dataConIdForConstant ty) [!(StgVarArg <$> mkBinderIdVar n (ALocal 6))] [])
                      ])
                ])
          ]
--  pure $ StgOpApp op (toList args) resultType resultTypeName
  -- Case (StgApp (Var)) of
  --   [ AltDataCon val => StgOpApp...
  --   ]
  --   AltType (AlgAlt for primitive-type)

--     Add : (ty : Constant) -> PrimFn 2
--     Sub : (ty : Constant) -> PrimFn 2
--     Mul : (ty : Constant) -> PrimFn 2
--     Div : (ty : Constant) -> PrimFn 2
--     Mod : (ty : Constant) -> PrimFn 2
--     Neg : (ty : Constant) -> PrimFn 1
--     ShiftL : (ty : Constant) -> PrimFn 2
--     ShiftR : (ty : Constant) -> PrimFn 2

--     BAnd : (ty : Constant) -> PrimFn 2
--     BOr : (ty : Constant) -> PrimFn 2
--     BXOr : (ty : Constant) -> PrimFn 2

--     LT  : (ty : Constant) -> PrimFn 2
--     LTE : (ty : Constant) -> PrimFn 2
--     EQ  : (ty : Constant) -> PrimFn 2
--     GTE : (ty : Constant) -> PrimFn 2
--     GT  : (ty : Constant) -> PrimFn 2

--     StrLength : PrimFn 1
--     StrHead : PrimFn 1
--     StrTail : PrimFn 1
--     StrIndex : PrimFn 2
--     StrCons : PrimFn 2
--     StrAppend : PrimFn 2
--     StrReverse : PrimFn 1
--     StrSubstr : PrimFn 3

--     DoubleExp : PrimFn 1
--     DoubleLog : PrimFn 1
--     DoubleSin : PrimFn 1
--     DoubleCos : PrimFn 1
--     DoubleTan : PrimFn 1
--     DoubleASin : PrimFn 1
--     DoubleACos : PrimFn 1
--     DoubleATan : PrimFn 1
--     DoubleSqrt : PrimFn 1
--     DoubleFloor : PrimFn 1
--     DoubleCeiling : PrimFn 1

--     Cast : Constant -> Constant -> PrimFn 1
--     BelieveMe : PrimFn 3
--     Crash : PrimFn 2
compilePrimOp _ p as
  = pure
  $ StgLit
  $ LitString
  $ "compilePrimOp " ++ show p ++ " " ++ show as

compileConstant : Constant -> Core Lit
compileConstant (I i)   = pure $ LitNumber LitNumInt $ cast i
compileConstant (BI i)  = pure $ LitNumber LitNumWord i
compileConstant (B8 i)  = pure $ LitNumber LitNumWord $ cast i
compileConstant (B16 i) = pure $ LitNumber LitNumWord $ cast i
compileConstant (B32 i) = pure $ LitNumber LitNumWord $ cast i
compileConstant (B64 i) = pure $ LitNumber LitNumWord64 i
compileConstant (Str s) = pure $ LitString s
compileConstant (Ch c)  = pure $ LitChar c
compileConstant (Db d)  = pure $ LitDouble d
compileConstant _ = coreFail $ UserError "compileConstant"

mutual
  compileANF
    :  {auto _ : Ref Uniques UniqueMap}
    -> {auto _ : Ref Counter Int}
    -> Core.Name.Name -> ANF
    -> Core SExpr
  compileANF funName (AV _ var)
    = pure $ StgApp !(mkBinderIdVar funName var) [] stgRepType ("?", "?", "?")

  compileANF _ (AAppName _ fun args)
    = pure $ StgLit $ LitString $ "AAppName " ++ show fun ++ " " ++ show args

  compileANF _ (AUnderApp _ fun _ args)
    = pure $ StgLit (LitString "AUnderApp")

  compileANF _ (AApp _ closure arg)
    = pure $ StgLit $ LitString $ "AApp " ++ show closure ++ " " ++ show arg

  compileANF funName (ALet _ var expr body) = do
    binding <- do
      binder  <- mkSBinderLocal funName var
      stgExpr <- compileANF funName expr
      pure $ StgNonRec binder $ StgRhsClosure Updatable [] stgExpr
    stgBody <- compileANF funName body
    pure $ StgLet binding stgBody

  compileANF _ (ACon _ tag _ args)
    = pure $ StgLit (LitString "ACon")

  compileANF funName (AOp _ prim args)
    = compilePrimOp funName prim args

  compileANF _ (AExtPrim _ name args)
    = pure $ StgLit (LitString "AExtPrim")

  compileANF funName (AConCase _ scrutinee alts mdef) = do
    let altType = PolyAlt -- TODO
    scrutBinder <- mkBinderIdVar funName scrutinee
    let stgScrutinee = StgApp scrutBinder [] stgRepType ("fun-type-pp","res-type-pp","origin")
    binder <- mkSBinderVar funName scrutinee
    stgDefAlt <- maybe
      (pure [])
      (\x => do
        stgBody <- compileANF funName x
        pure [MkAlt AltDefault [] stgBody])
      mdef
    stgAlts <- traverse (compileConAlt funName) alts
    pure $ StgCase stgScrutinee binder altType (stgDefAlt ++ stgAlts)

  compileANF funName (AConstCase _ scrutinee alts mdef) = do
    let altType = PrimAlt UnliftedRep -- TODO
    scrutBinder <- mkBinderIdVar funName scrutinee
    let stgScrutinee = StgApp scrutBinder [] stgRepType ("fun-type-ppc","res-type-ppc","originc")
    binder <- mkSBinderVar funName scrutinee
    stgDefAlt <- maybe
      (pure [])
      (\x => do
        stgBody <- compileANF funName x
        pure [MkAlt AltDefault [] stgBody])
      mdef
    stgAlts <- traverse (compileConstAlt funName) alts
    pure $ StgCase stgScrutinee binder altType (stgDefAlt ++ stgAlts)

  compileANF _ (APrimVal _ constant)
    = pure $ StgLit (LitString "APrimVal")

  compileANF _ (AErased _)
    = pure $ StgLit (LitString "AErased")

  compileANF _ (ACrash _ msg)
    = pure $ StgLit (LitString "ACrash")

  compileConAlt
    :  {auto _ : Ref Uniques UniqueMap}
    -> {auto _ : Ref Counter Int}
    -> Core.Name.Name -> AConAlt
    -> Core SAlt
  compileConAlt funName (MkAConAlt name tag args body) = do
    stgArgs     <- traverse (mkSBinderLocal funName) args
    stgBody     <- compileANF funName body
    stgDataCon  <- compileDataConId tag
    pure $ MkAlt (AltDataCon stgDataCon) stgArgs stgBody

  compileConstAlt
    :  {auto _ : Ref Uniques UniqueMap}
    -> {auto _ : Ref Counter Int}
    -> Core.Name.Name -> AConstAlt
    -> Core SAlt
  compileConstAlt funName (MkAConstAlt constant body) = do
    stgBody <- compileANF funName body
    lit <- compileConstant constant
    pure $ MkAlt (AltLit lit) [] stgBody

compileTopBinding
  :  {auto _ : Ref Uniques UniqueMap}
  -> {auto _ : Ref Counter Int}
  -> (Core.Name.Name, ANFDef)
  -> Core (Maybe STopBinding)
compileTopBinding (funName,MkAFun args body) = do
  funBody       <- compileANF funName body
  funArguments  <- traverse (mkSBinderLocal funName) args
  funNameBinder <- mkSBinderName funName
  rhs           <- pure $ StgRhsClosure ReEntrant funArguments funBody
  binding       <- pure $ StgNonRec funNameBinder rhs
  pure $ Just $ StgTopLifted binding
compileTopBinding (_,MkACon tag arity) = do
  pure Nothing
compileTopBinding (_,MkAForeign css fargs rtype) = do
  pure Nothing
compileTopBinding (_,MkAError body) = do
  pure Nothing

-- We compile only one enormous module
export
compileModule
  :  {auto _ : Ref Uniques UniqueMap}
  -> {auto _ : Ref Counter Int}
  -> List (Core.Name.Name, ANFDef)
  -> Core SModule
compileModule defs = do
  let phase              = "Main"
  let moduleUnitId       = MkUnitId "MainUnit"
  let name               = MkModuleName "Main" -- : ModuleName
  let sourceFilePath     = "some.idr" -- : String
  let foreignStubs       = NoStubs -- : ForeignStubs -- ???
  let hasForeignExported = False -- : Bool
  let dependency         = [] -- : List (UnitId, List ModuleName)
  let externalTopIds     = [] -- : List (UnitId, List (ModuleName, List idBnd))
  tyCons                 <- createPrimitiveTypesSection -- : List (UnitId, List (ModuleName, List tcBnd))
  topBindings            <- mapMaybe id <$> traverse compileTopBinding defs
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
constTag : Constant -> Int
-- 1 = ->, 2 = Type
constTag IntType = 3
constTag IntegerType = 4
constTag Bits8Type = 5
constTag Bits16Type = 6
constTag Bits32Type = 7
constTag Bits64Type = 8
constTag StringType = 9
constTag CharType = 10
constTag DoubleType = 11
constTag WorldType = 12 -- How to represent the World type in STG?
constTag _ = 0

public export
data PrimFn : Nat -> Type where
     Add : (ty : Constant) -> PrimFn 2
     Sub : (ty : Constant) -> PrimFn 2
     Mul : (ty : Constant) -> PrimFn 2
     Div : (ty : Constant) -> PrimFn 2
     Mod : (ty : Constant) -> PrimFn 2
     Neg : (ty : Constant) -> PrimFn 1
     ShiftL : (ty : Constant) -> PrimFn 2
     ShiftR : (ty : Constant) -> PrimFn 2

     BAnd : (ty : Constant) -> PrimFn 2
     BOr : (ty : Constant) -> PrimFn 2
     BXOr : (ty : Constant) -> PrimFn 2

     LT  : (ty : Constant) -> PrimFn 2
     LTE : (ty : Constant) -> PrimFn 2
     EQ  : (ty : Constant) -> PrimFn 2
     GTE : (ty : Constant) -> PrimFn 2
     GT  : (ty : Constant) -> PrimFn 2

     StrLength : PrimFn 1
     StrHead : PrimFn 1
     StrTail : PrimFn 1
     StrIndex : PrimFn 2
     StrCons : PrimFn 2
     StrAppend : PrimFn 2
     StrReverse : PrimFn 1
     StrSubstr : PrimFn 3

     DoubleExp : PrimFn 1
     DoubleLog : PrimFn 1
     DoubleSin : PrimFn 1
     DoubleCos : PrimFn 1
     DoubleTan : PrimFn 1
     DoubleASin : PrimFn 1
     DoubleACos : PrimFn 1
     DoubleATan : PrimFn 1
     DoubleSqrt : PrimFn 1
     DoubleFloor : PrimFn 1
     DoubleCeiling : PrimFn 1

     Cast : Constant -> Constant -> PrimFn 1
     BelieveMe : PrimFn 3
     Crash : PrimFn 2

https://gitlab.haskell.org/ghc/ghc/-/blob/master/compiler/GHC/Builtin/primops.txt.pp
-}

{-
How to represent String and idris FFI calls?
-}
