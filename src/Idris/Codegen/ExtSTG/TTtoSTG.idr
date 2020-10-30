module Idris.Codegen.ExtSTG.TTtoSTG -- where

import Compiler.ANF
import Idris.Codegen.ExtSTG.STG
import Core.Context
import Core.TT
import Data.Vect


--data TopBinding' idBnd idOcc dcOcc tcOcc
--  = StgTopLifted    (Binding' idBnd idOcc dcOcc tcOcc)
--  | StgTopStringLit idBnd String

--data Rhs' idBnd idOcc dcOcc tcOcc
--  = StgRhsClosure
--      UpdateFlag
--      (List idBnd)                    -- arguments; if empty, then not a function. The order is important
--      (Expr' idBnd idOcc dcOcc tcOcc) -- body
--  | StgRhsCon
--      dcOcc               -- DataCon
--      (List (Arg' idOcc)) -- Args

--data Binding' idBnd idOcc dcOcc tcOcc
--  = StgNonRec idBnd (Rhs' idBnd idOcc dcOcc tcOcc)
--  | StgRec    (List (idBnd, Rhs' idBnd idOcc dcOcc tcOcc))

--record Binder where
--  constructor MkBinder
--  BinderName : Name
--  Id         : BinderId
--  RepType    : RepType
--  TypeSig    : Name
--  Scope      : Scope
--  Details    : IdDetails
--  UnitId     : UnitId
--  Module     : ModuleName
--  TopLevel   : Bool

data Counter : Type where

export
mkCounter : Core (Ref Counter Int)
mkCounter = newRef Counter 0

mkUnique : {auto _ : Ref Counter Int} -> Core Unique
mkUnique = do
  x <- get Counter
  let u = MkUnique 'c' x
  put Counter (x + 1)
  pure u

stgRepType : RepType
stgRepType = SingleValue UnliftedRep

mkBinder : {auto _ : Ref Counter Int} -> Bool -> String -> Core Binder
mkBinder topLevel binderName = do
  binderId <- MkBinderId <$> mkUnique
  let typeSig     = "typeSig"
  let scope       = GlobalScope
  let details     = VanillaId -- ?
  let unitId      = MkUnitId "unitId"
  let mod         = MkModuleName "module"
  pure $ MkBinder
    binderName  --BinderName : Name
    binderId    --Id         : BinderId
    stgRepType  --RepType    : RepType
    typeSig     --TypeSig    : Name
    scope       --Scope      : Scope
    details     --Details    : IdDetails
    unitId      --UnitId     : UnitId
    mod         --Module     : ModuleName
    topLevel    --TopLevel   : Bool

mkBinderLocal : {auto _ : Ref Counter Int} -> Core.Name.Name -> Int -> Core Binder
mkBinderLocal n x = mkBinder False (show n ++ ":" ++ show x)

mkBinderName : {auto _ : Ref Counter Int} -> Core.Name.Name -> Core Binder
mkBinderName n = mkBinder True $ show n


  --public export
  --record DataCon where
  --  constructor MkDataCon
  --  Name   : Name
  --  Id     : DataConId
  --  UnitId : UnitId
  --  Module : ModuleName
  --  Rep    : DataConRep
  --  TyCon  : TyCon
  --  Worker : Binder

  --record TyCon where
  --  constructor MkTyCon
  --  Name     : Name
  --  Id       : TyConId
  --  UnitId   : UnitId
  --  Module   : ModuleName
  --  DataCons : List DataCon


mkBinderVar : {auto _ : Ref Counter Int} -> Core.Name.Name -> AVar -> Core Binder
mkBinderVar n (ALocal x) = mkBinder False (show n ++ ":" ++ show x)
mkBinderVar n ANull      = mkBinder False (show n ++ ":erased")

compileDataCon : {auto _ : Ref Counter Int} -> Maybe Int -> Core DataCon
compileDataCon Nothing = do
  worker <- mkBinder False "binderName"
  pure $
    MkDataCon
      ""
      (MkDataConId (MkUnique 'd' 0))
      (MkUnitId "unit")
      (MkModuleName "")
      (AlgDataCon []) -- Field primitive types
      (MkTyCon
        ""
        (MkTypeConId (MkUnique 't' 0))
        (MkUnitId "unit")
        (MkModuleName "")
        []) -- Data constructors
      worker
compileDataCon (Just t) = do
  worker <- mkBinder False "binderName"
  pure $
    MkDataCon
      ""
      (MkDataConId (MkUnique 'd' t))
      (MkUnitId "unit")
      (MkModuleName "")
      (AlgDataCon []) -- Field primitive types
      (MkTyCon
        ""
        (MkTypeConId (MkUnique 't' 0))
        (MkUnitId "unit")
        (MkModuleName "")
        []) -- Data constructors
      worker

{-
constTag IntType = 3
constTag IntegerType = 4
constTag Bits8Type = 5
constTag Bits16Type = 6
constTag Bits32Type = 7
constTag Bits64Type = 8
constTag StringType = 9
constTag CharType = 10
constTag DoubleType = 11
-}

compilePrimOp : Core.Name.Name -> PrimFn arity -> Vect arity AVar -> Core Expr
compilePrimOp funName (Add : (ty : Constant) -> PrimFn 2
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


compilePrimOp _ _ _ = pure $ StgLit (LitString "compilePrimOp")

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
  export
  compileANFDef : {auto counter : Ref Counter Int} -> (Core.Name.Name, ANFDef) -> Core (Maybe TopBinding)
  compileANFDef (funName,MkAFun args body) = do
    funBody       <- compileANF funName body
    funArguments  <- traverse (mkBinderLocal funName) args
    funNameBinder <- mkBinderName funName
    rhs           <- pure $ StgRhsClosure ReEntrant funArguments funBody
    binding       <- pure $ StgNonRec funNameBinder rhs
    pure $ Just $ StgTopLifted binding
  compileANFDef (_,MkACon tag arity) = do
    pure Nothing
  compileANFDef (_,MkAForeign css fargs rtype) = do
    pure Nothing
  compileANFDef (_,MkAError body) = do
    pure Nothing

  compileANF : {auto _ : Ref Counter Int} -> Core.Name.Name -> ANF -> Core Expr
  compileANF _ (AV _ var)
    = pure $ StgLit (LitString "AV")

  compileANF _ (AAppName _ fun args)
    = pure $ StgLit (LitString "AAppName")

  compileANF _ (AUnderApp _ fun _ args)
    = pure $ StgLit (LitString "AUnderApp")

  compileANF _ (AApp _ closure arg)
    = pure $ StgLit (LitString "AApp")

  compileANF funName (ALet _ var expr body) = do
    binding <- do
      binder  <- mkBinderLocal funName var
      stgExpr <- compileANF funName expr
      pure $ StgNonRec binder $ StgRhsClosure Updatable [] stgExpr
    stgBody <- compileANF funName body
    pure $ StgLet binding stgBody

  compileANF _ (ACon _ tag _ args)
    = pure $ StgLit (LitString "ACon")

  compileANF _ (AOp _ prim args)
    = pure $ StgLit (LitString "AOp")

  compileANF _ (AExtPrim _ name args)
    = pure $ StgLit (LitString "AExtPrim")

  compileANF funName (AConCase _ scrutinee alts mdef) = do
    let altType = PolyAlt -- TODO
    scrutBinder <- mkBinderVar funName scrutinee
    let stgScrutinee  = StgApp scrutBinder [] stgRepType ("fun-type-pp","res-type-pp","origin")
    binder <- mkBinderVar funName scrutinee
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
    scrutBinder <- mkBinderVar funName scrutinee
    let stgScrutinee = StgApp scrutBinder [] stgRepType ("fun-type-ppc","res-type-ppc","originc")
    binder <- mkBinderVar funName scrutinee
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

  compileConAlt : {auto _ : Ref Counter Int} -> Core.Name.Name -> AConAlt -> Core Alt
  compileConAlt funName (MkAConAlt name tag args body) = do
    stgArgs     <- traverse (mkBinderLocal funName) args
    stgBody     <- compileANF funName body
    stgDataCon  <- compileDataCon tag
    pure $ MkAlt (AltDataCon stgDataCon) stgArgs stgBody

  compileConstAlt : {auto _ : Ref Counter Int} -> Core.Name.Name -> AConstAlt -> Core Alt
  compileConstAlt funName (MkAConstAlt constant body) = do
    stgBody <- compileANF funName body
    lit <- compileConstant constant
    pure $ MkAlt (AltLit lit) [] stgBody

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
