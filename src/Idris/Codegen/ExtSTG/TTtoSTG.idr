module Idris.Codegen.ExtSTG.TTtoSTG -- where

import Compiler.ANF
import Idris.Codegen.ExtSTG.STG
import Core.Context
import Core.TT
import Data.Vect


data Counter : Type where

export
mkCounter : Core (Ref Counter Int)
mkCounter = newRef Counter 0

mkUnique : {auto _ : Ref Counter Int} -> Char -> Core Unique
mkUnique m = do
  x <- get Counter
  let u = MkUnique m x
  put Counter (x + 1)
  pure u

stgRepType : RepType
stgRepType = SingleValue UnliftedRep

mkSBinder : {auto _ : Ref Counter Int} -> Bool -> String -> Core SBinder
mkSBinder topLevel binderName = do
  binderId <- MkBinderId <$> mkUnique 's'
  let typeSig = "mkSBinder: typeSig"
  let scope   = GlobalScope
  let details = VanillaId
  let info    = "mkSBinder: IdInfo"
  let defLoc  = SsUnhelpfulSpan binderName
  pure $ MkSBinder
    binderName
    binderId
    stgRepType
    typeSig
    scope
    details
    info
    defLoc

mkSBinderLocal : {auto _ : Ref Counter Int} -> Core.Name.Name -> Int -> Core SBinder
mkSBinderLocal n x = mkSBinder False (show n ++ ":" ++ show x)

mkSBinderName : {auto _ : Ref Counter Int} -> Core.Name.Name -> Core SBinder
mkSBinderName n = mkSBinder True $ show n

--export
--addData : {auto c : Ref Ctxt Defs} ->
--          List Name -> Visibility -> Int -> DataDef -> Core Int
--addData vars vis tidx (MkData (MkCon dfc tyn arity tycon) datacons)
--public export
--data DataDef : Type where
--     MkData : (tycon : Constructor) -> (datacons : List Constructor) ->
--              DataDef

mkSBinderVar : {auto _ : Ref Counter Int} -> Core.Name.Name -> AVar -> Core SBinder
mkSBinderVar n (ALocal x) = mkSBinder False (show n ++ ":" ++ show x)
mkSBinderVar n ANull      = mkSBinder False (show n ++ ":erased")

mkBinderIdVar : {auto _ : Ref Counter Int} -> Core.Name.Name -> AVar -> Core BinderId
mkBinderIdVar n (ALocal x) = pure $ MkBinderId $ MkUnique 'b' x
mkBinderIdVar n ANull      = MkBinderId <$> mkUnique 'b'

compileDataConId : {auto _ : Ref Counter Int} -> Maybe Int -> Core DataConId
compileDataConId Nothing  = MkDataConId <$> mkUnique 'c'
compileDataConId (Just t) = pure $ MkDataConId $ MkUnique 'c' t

compilePrimOp : Core.Name.Name -> PrimFn arity -> Vect arity AVar -> Core Expr
--compilePrimOp funName (Add : (ty : Constant) -> PrimFn 2
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
  compileANFDef
    :  {auto counter : Ref Counter Int}
    -> (Core.Name.Name, ANFDef)
    -> Core (Maybe (String, STopBinding))
  compileANFDef (funName,MkAFun args body) = do
    funBody       <- compileANF funName body
    funArguments  <- traverse (mkSBinderLocal funName) args
    funNameBinder <- mkSBinderName funName
    rhs           <- pure $ StgRhsClosure ReEntrant funArguments funBody
    binding       <- pure $ StgNonRec funNameBinder rhs
    pure $ Just $ (show funName, StgTopLifted binding)
  compileANFDef (_,MkACon tag arity) = do
    pure Nothing
  compileANFDef (_,MkAForeign css fargs rtype) = do
    pure Nothing
  compileANFDef (_,MkAError body) = do
    pure Nothing

  compileANF : {auto _ : Ref Counter Int} -> Core.Name.Name -> ANF -> Core SExpr
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
      binder  <- mkSBinderLocal funName var
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


  compileConAlt : {auto _ : Ref Counter Int} -> Core.Name.Name -> AConAlt -> Core SAlt
  compileConAlt funName (MkAConAlt name tag args body) = do
    stgArgs     <- traverse (mkSBinderLocal funName) args
    stgBody     <- compileANF funName body
    stgDataCon  <- compileDataConId tag
    pure $ MkAlt (AltDataCon stgDataCon) stgArgs stgBody

  compileConstAlt : {auto _ : Ref Counter Int} -> Core.Name.Name -> AConstAlt -> Core SAlt
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
