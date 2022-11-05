module Idris.Codegen.ExtSTG.STG

import public Data.List
import public Idris.Codegen.ExtSTG.GHCPrimOp
import public Idris.Codegen.ExtSTG.Prelude
import public Idris.Codegen.ExtSTG.Representation

{-
This module contains the definitions of the STG. We mirror the current internals of GHC.
-}

public export
Name : Type
Name = String

public export
IdInfo : Type
IdInfo = String

||| Unique identifier for different things, such as TyConId, DataConId, BinderId, etc
|||
||| The character parameter is used in the different phases of
||| the code generator by GHC and it can be used at will.
public export
data Unique
  = MkUnique Char Int

export
Eq Unique where
  (MkUnique c0 i0) == (MkUnique c1 i1) = (c0,i0) == (c1,i1)

export
Show Unique where
  show (MkUnique c i) = "(MkUnique " ++ show c ++ " " ++ show i ++ ")"

export
Ord Unique where
  compare (MkUnique c1 i1) (MkUnique c2 i2) = case compare c1 c2 of
    LT => LT
    EQ => compare i1 i2
    GT => GT

public export
record RealSrcSpan where
  constructor MkRealSrcSpan
  SpanFile  : Name
  SpanSLine : Int
  SpanSCol  : Int
  SpanELine : Int
  SpanECol  : Int

public export
record BufSpan where
  constructor MkBufSpan
  BufSpanStart : Int
  BufSpanEnd   : Int

public export
data SrcSpan
  = SsRealSrcSpan   RealSrcSpan (Maybe BufSpan)
  | SsUnhelpfulSpan Name

public export
data TyConId = MkTyConId Unique

export
tyConUnique : TyConId -> Unique
tyConUnique (MkTyConId u) = u

export
Eq TyConId where
  (MkTyConId t0) == (MkTyConId t1) = t0 == t1

export
Show TyConId where
  show (MkTyConId t0) = "(MkTyConId " ++ show t0 ++ ")"

public export
data DataConRep
  = AlgDataCon      (List PrimRep)
  | UnboxedTupleCon Nat
  -- TODO: Add the PrimRep list here, or a different constructor.

export
Show DataConRep where
  showPrec p (AlgDataCon reps) = showCon p "AlgDataCon" $ showArg reps
  showPrec p (UnboxedTupleCon n) = showCon p "UnboxedTupleCon" $ showArg n

export
SemiDecEq DataConRep where
  semiDecEq (AlgDataCon xs) (AlgDataCon ys) = do
    Refl <- semiDecEq xs ys
    Just Refl
  semiDecEq (UnboxedTupleCon x) (UnboxedTupleCon y) = do
    Refl <- semiDecEq x y
    Just Refl
  semiDecEq _ _ = Nothing

public export
data DataConId : DataConRep -> Type where
  MkDataConId : {0 r : DataConRep} -> Unique -> DataConId r

export
Show (DataConId q) where
  show (MkDataConId u) = show u

public export
DataConIdSg : Type
DataConIdSg = (r : DataConRep ** DataConId r)

export
Eq DataConIdSg where
  (_ ** MkDataConId u1) == (_ ** MkDataConId u2) = u1 == u2

export
Ord DataConIdSg where
  compare (_ ** MkDataConId u1) (_ ** MkDataConId u2) = compare u1 u2

public export
mkDataConIdSg : {r : DataConRep} -> DataConId r -> DataConIdSg
mkDataConIdSg {r} d = (r ** d)

export
unsafeMkDataConIdSg : (r : DataConRep) -> Unique -> (r ** DataConId r)
unsafeMkDataConIdSg r u = (r ** MkDataConId u)

export
checkDataConIdRep : (r : DataConRep) -> DataConIdSg -> Maybe (DataConId r)
checkDataConIdRep r (q ** d) = case semiDecEq r q of
  Nothing   => Nothing
  Just Refl => Just d

export
dataConUnique : DataConId r -> Unique
dataConUnique (MkDataConId u) = u

public export
record ModuleName where
  constructor MkModuleName
  GetModuleName : Name

export
Eq ModuleName where
  (MkModuleName m1) == (MkModuleName m2) = m1 == m2

export
Ord ModuleName where
  compare (MkModuleName m1) (MkModuleName m2) = compare m1 m2

||| The package name of the module.
public export
record UnitId where
  constructor MkUnitId
  GetUnitId : Name

export
Eq UnitId where
  (MkUnitId u1) == (MkUnitId u2) = u1 == u2

export
Ord UnitId where
  compare (MkUnitId u1) (MkUnitId u2) = compare u1 u2

public export
data IdDetails
  = VanillaId
  | FExportedId
  | RecSelId
  | DataConWorkId DataConIdSg
  | DataConWrapId DataConIdSg
  | ClassOpId
  | PrimOpId
  | TickBoxOpId
  | DFunId
  | CoVarId
  | JoinId Int

public export
data Scope
  = LocalScope      -- visible for expression body
  | GlobalScope     -- visible for a single haskell module
  | HaskellExported -- visible for every haskell module
  | ForeignExported -- visible for foreign libraries

-- NOTE: TypeSig is used only in FFI
-- The parameters should be TODO: ...

public export
data BinderId : RepType -> Type where
  MkBinderId : {0 r : RepType} -> Unique -> BinderId r

export
Show (BinderId r) where
  show (MkBinderId u) = "binder-" ++ show u

export
getBinderIdUnique : BinderId r -> Unique
getBinderIdUnique (MkBinderId u) = u

public export
BinderIdSg : Type
BinderIdSg = (r : RepType ** BinderId r)

%hint
export
mkBinderIdSg : {r : RepType} -> BinderId r -> BinderIdSg
mkBinderIdSg {r} b = (r ** b)

namespace SBinder

  public export
  record SBinder (binderRep : RepType) where
    constructor MkSBinder
    binderName    : Name
    binderId      : BinderId binderRep
    binderTypeSig : String
    binderScope   : Scope
    binderDetails : IdDetails
    binderInfo    : IdInfo
    binderDefLoc  : SrcSpan

  %hint
  public export
  getBinderId : SBinder rep -> BinderId rep
  getBinderId = binderId

  public export
  SBinderSg : Type
  SBinderSg = (r : RepType ** SBinder r)

  public export
  LiftedRepBinder : Type
  LiftedRepBinder = SBinder (SingleValue LiftedRep)

  %hint
  export
  mkSBinderSg : {r : RepType} -> SBinder r -> SBinderSg
  mkSBinderSg {r} s = (r ** s)

  export
  binderRep : {r : RepType} -> SBinder r -> RepType
  binderRep x = r

export
getSBinderIdSg : SBinderSg -> BinderIdSg
getSBinderIdSg (r ** b) = (r ** binderId b)

namespace SDataCon

  public export
  record SDataCon (dataConRep : DataConRep) where
    constructor MkSDataCon
    name   : Name
    ident  : DataConId dataConRep
    worker : LiftedRepBinder
    defLoc : SrcSpan

  export
  mkSDataCon : (r : DataConRep) -> Name -> DataConId r -> LiftedRepBinder -> SrcSpan -> SDataCon r
  mkSDataCon r n d w s = MkSDataCon n d w s

  export
  rep : {r : DataConRep} -> SDataCon r -> DataConRep
  rep x = r

  public export
  SDataConSg : Type
  SDataConSg = (r ** SDataCon r)

  export
  mkSDataConSg : {r : DataConRep} -> SDataCon r -> SDataConSg
  mkSDataConSg {r} d = (r ** d)

  export
  identSg : SDataConSg -> DataConIdSg
  identSg (MkDPair r d) = MkDPair r (ident d)

export
Show (SDataCon r) where
  show s = show (name s)

public export
record STyCon where
  constructor MkSTyCon
  Name     : Name
  Id       : TyConId
  DataCons : (List SDataConSg)
  DefLoc   : SrcSpan

export
Show STyCon where
  show (MkSTyCon n i _ _) = "(MkSTyCon " ++ show n ++ " " ++ show i ++ " _ _)"

public export
data LitNumType
  = LitNumInt    -- Int#   according to target machine
  | LitNumInt64  -- Int64# exactly 64 bits
  | LitNumWord   -- Word#  according to target machine
  | LitNumWord64 -- Word64 exactly 64 bits

public export
data LabelSpec
  = FunctionLabel (Maybe Int) -- only for stdcall convention
    -- ^^ Can be called, CCallTarge, foreign call op.
  | DataLabel
    -- ^^ Can not be called.

public export
data Lit
  = LitChar     Char
  | LitString   String
    -- ^^ The String literal in STG is not the String literal in Haskell. This stands for literals which are byte strings.
    -- TODO: Describe how top level strings are the real String literals in STG programs, and how Strings are represented
    --       as ByteArrays.
    -- In Argument and expression literal when evaluated it will be put on the static data segment, same as
    -- top literals, but without binders. And the AddrRep is returned without any allocation.
    -- String literals are forbidden in matching alts, they should be represented as primitive
    -- functions
    -- Worker-Wrapper transformation, build a lazy-list Char.
  | LitNullAddr
  | LitFloat    Double -- TODO: Represent floats
  | LitDouble   Double
  | LitLabel    String LabelSpec
    -- ^^ Its representation type is AddrRep
    -- Label in assembly, which is a pointer. Static array from FFI, static string from FFI
    -- It contains the name of the symbol.
  | LitNumber   LitNumType Integer

export
Show LitNumType where
  showPrec _ LitNumInt    = "LitNumInt"
  showPrec _ LitNumInt64  = "LitNumInt64"
  showPrec _ LitNumWord   = "LitNumWord"
  showPrec _ LitNumWord64 = "LitNumWord64"

export
Show Lit where
  showPrec d (LitChar     c) = showCon d "LitChar" $ showArg c
  showPrec d (LitString   s) = showCon d "LitString" $ showArg s
  showPrec d LitNullAddr     = "LitNullAddr"
  showPrec d (LitFloat    x) = showCon d "LitFloat" $ showArg x
  showPrec d (LitDouble   x) = showCon d "LitDouble" $ showArg x
  showPrec d (LitLabel    s l) = showCon d "LitLabel" $ showArg s ++ " ..."
  showPrec d (LitNumber   l i) = showCon d "LitNumber" $ "\{showArg l} \{showArg i}"

public export
data AltType
  = PolyAlt -- Instead of ForceBoxed
  | MultiValAlt Nat
  | PrimAlt     PrimRep
  | AlgAlt      TyConId

public export
litPrimRep : Lit -> PrimRep
litPrimRep (LitChar x)                = CharRep
litPrimRep (LitString x)              = AddrRep
litPrimRep LitNullAddr                = AddrRep
litPrimRep (LitFloat x)               = FloatRep
litPrimRep (LitDouble x)              = DoubleRep
litPrimRep (LitLabel x y)             = AddrRep
litPrimRep (LitNumber LitNumInt y)    = IntRep
litPrimRep (LitNumber LitNumInt64 y)  = Int64Rep
litPrimRep (LitNumber LitNumWord y)   = WordRep
litPrimRep (LitNumber LitNumWord64 y) = Word64Rep

public export
litRepType : Lit -> RepType
litRepType l = SingleValue (litPrimRep l)

namespace LitRep

  public export
  data LitRep : Lit -> RepType -> Type where
    LitChar       : LitRep (LitChar _)    (SingleValue CharRep)
    LitString     : LitRep (LitString _)  (SingleValue AddrRep)
    LitNullAddr   : LitRep LitNullAddr    (SingleValue AddrRep)
    LitFloat      : LitRep (LitFloat _)   (SingleValue FloatRep)
    LitDouble     : LitRep (LitDouble _)  (SingleValue DoubleRep)
    LitLabel      : LitRep (LitLabel _ _) (SingleValue AddrRep)
    LitNumInt     : LitRep (LitNumber LitNumInt _) (SingleValue IntRep)
    LitNumInt64   : LitRep (LitNumber LitNumInt64  _) (SingleValue Int64Rep)
    LitNumWord    : LitRep (LitNumber LitNumWord   _) (SingleValue WordRep)
    LitNumWord64  : LitRep (LitNumber LitNumWord64 _) (SingleValue Word64Rep)

  export
  total
  litRepTypeF : {r : RepType} -> (l : Lit) -> LitRep l (litRepType l)
  litRepTypeF (LitChar x) = LitChar
  litRepTypeF (LitString x) = LitString
  litRepTypeF LitNullAddr = LitNullAddr
  litRepTypeF (LitFloat x) = LitFloat
  litRepTypeF (LitDouble x) = LitDouble
  litRepTypeF (LitLabel x y) = LitLabel
  litRepTypeF (LitNumber LitNumInt y) = LitNumInt
  litRepTypeF (LitNumber LitNumInt64 y) = LitNumInt64
  litRepTypeF (LitNumber LitNumWord y) = LitNumWord
  litRepTypeF (LitNumber LitNumWord64 y) = LitNumWord64

public export
data Arg : RepType -> Type where
  StgVarArg : BinderId r -> Arg r
  StgLitArg : {0 r : RepType} -> (l : Lit) -> (0 _ : r = litRepType l) => Arg r
  StgVoid   : Arg (SingleValue VoidRep)

public export
ArgSg : Type
ArgSg = (r : RepType ** Arg r)

%hint
export
mkArgSg : {r : RepType} -> Arg r -> ArgSg
mkArgSg {r} a = (r ** a)

namespace ArgList
  public export
  data ArgList : List PrimRep -> Type where
    Nil  : ArgList []
    (::) : Arg (SingleValue r) -> ArgList rs -> ArgList (r :: rs)

public export
StgConAppArgType : DataConRep -> Type
StgConAppArgType (AlgDataCon [])  = ()
StgConAppArgType (AlgDataCon [p]) = Arg (SingleValue p)
StgConAppArgType (AlgDataCon (p0 :: p1 :: ps)) = ArgList (p0 :: p1 :: ps)
StgConAppArgType (UnboxedTupleCon n) = Void

public export
StgOpArgType : List PrimRep -> Type
StgOpArgType [] = ()
StgOpArgType [r] = Arg (SingleValue r)
StgOpArgType (r1 :: r2 :: rs) = ArgList (r1 :: r2 :: rs)

namespace BinderList
  public export
  data BinderList : List PrimRep -> Type where
    Nil  :                                             BinderList []
    (::) : SBinder (SingleValue r) -> BinderList rs -> BinderList (r :: rs)

  export
  toArgList : BinderList rs -> ArgList rs
  toArgList [] = []
  toArgList (x :: xs) = StgVarArg (binderId x) :: toArgList xs

public export
data IsAltLit : Lit -> Type where
  CharAltLit   : IsAltLit (LitChar c)
  FloatAltLit  : IsAltLit (LitFloat f)
  DoubleAltLit : IsAltLit (LitDouble d)
  NumberAltLit : IsAltLit (LitNumber t n)

export
decAltLit : (l : Lit) -> Maybe (IsAltLit l)
decAltLit (LitChar c)     = Just CharAltLit
decAltLit (LitFloat f)    = Just FloatAltLit
decAltLit (LitDouble d)   = Just DoubleAltLit
decAltLit (LitNumber t n) = Just NumberAltLit
decAltLit other = Nothing

public export
data AltCon : RepType -> Type where
  AltDataCon         : DataConIdSg                     -> AltCon (SingleValue LiftedRep)
  AltUnboxedOneTuple : DataConId (UnboxedTupleCon 1)   -> AltCon (UnboxedTuple [LiftedRep])
  AltLit             : (l : Lit) -> (0 _ : IsAltLit l) => AltCon (litRepType l)
  AltDefault         : {0 r : RepType}                 -> AltCon r

public export
data UpdateFlag
  = ReEntrant
  | Updatable
  | SingleEntry

public export
data CCallConv
  = MkCCallConv
  | CApiConv
  | StdCallConv
  | PrimCallConv
  | JavaScriptCallConv

public export
data SourceText
  = MkSourceText String
  | NoSourceText

public export
data CCallTarget
  = StaticTarget SourceText String (Maybe UnitId) Bool
  | DynamicTarget

public export
data Safety
  = PlaySafe
  | PlayInterruptible
  | PlayRisky

public export
record ForeignCall where
  constructor MkForeignCall
  CTarget : CCallTarget
  CConv   : CCallConv
  CSafety : Safety

public export
data PrimCall = MkPrimCall String UnitId

-- TODO: Make a name and type description of STG primitive operations
-- as in Idris PrimOp
public export
data StgOp
  = StgPrimOp     Name
  | StgPrimCallOp PrimCall
  | StgFCallOp    ForeignCall

namespace BList
  ||| BinderList, specialized list for storing special Binders in STG alternatives.
  public export
  data BList : List PrimRep -> Type where
    Nil  : BList []
    (::) : SBinder (SingleValue p) -> BList ps -> BList (p :: ps)

public export
AltDataConRepType : DataConRep -> Type
AltDataConRepType (AlgDataCon [])     = ()
AltDataConRepType (AlgDataCon [p])    = SBinder (SingleValue p)
AltDataConRepType (AlgDataCon (p0 :: p1 :: ps)) = BList (p0 :: p1 :: ps)
AltDataConRepType (UnboxedTupleCon n) = Void

public export
AltBinderType : AltCon r -> Type
AltBinderType (AltDataCon d) = AltDataConRepType (fst d)
AltBinderType (AltLit l)     = ()
AltBinderType AltDefault     = ()
AltBinderType (AltUnboxedOneTuple d) = SBinder (SingleValue LiftedRep)

-- TODO: Make the MultiValAlt n impossible
public export
altRepType : AltType -> RepType
altRepType PolyAlt         = SingleValue LiftedRep -- Used only for forced values
altRepType (MultiValAlt 0) = SingleValue VoidRep -- For VoidRep PrimOps
altRepType (MultiValAlt 1) = UnboxedTuple [LiftedRep] -- For IO, Not general, only used for forcing thunk between Idris and GHC generated STG.
altRepType (MultiValAlt n) = UnboxedTuple (replicate n VoidRep) -- Invalid, shouldn't be used
altRepType (PrimAlt p)     = SingleValue p
altRepType (AlgAlt t)      = SingleValue LiftedRep

public export
decLitRepType : (l : Lit) -> (r : RepType) -> Maybe (litRepType l = r)
decLitRepType (LitChar   _)              (SingleValue WordRep)   = Just Refl -- CharRep = WordRep, but CharRep is not a constructor.
decLitRepType (LitString _)              (SingleValue AddrRep)   = Just Refl
decLitRepType (LitNullAddr)              (SingleValue AddrRep)   = Just Refl
decLitRepType (LitFloat  _)              (SingleValue FloatRep)  = Just Refl
decLitRepType (LitDouble _)              (SingleValue DoubleRep) = Just Refl
decLitRepType (LitLabel _ _)             (SingleValue AddrRep)   = Just Refl
decLitRepType (LitNumber LitNumInt    _) (SingleValue IntRep)    = Just Refl
decLitRepType (LitNumber LitNumInt64  _) (SingleValue Int64Rep)  = Just Refl
decLitRepType (LitNumber LitNumWord   _) (SingleValue WordRep)   = Just Refl
decLitRepType (LitNumber LitNumWord64 _) (SingleValue Word64Rep) = Just Refl
decLitRepType _ _ = Nothing

namespace GHCBinderIDs

  -- More from: https://github.com/grin-compiler/ghc-whole-program-compiler-project/blob/master/external-stg-interpreter/lib/Stg/Interpreter/Base.hs#L635

  export
  coercionTokenHashtag : BinderId (SingleValue VoidRep)
  coercionTokenHashtag = MkBinderId (MkUnique '0' 124)

  export
  voidHashtag : BinderId (SingleValue VoidRep)
  voidHashtag = MkBinderId (MkUnique '0' 21)

  export
  realWorldHashtag : BinderId (SingleValue VoidRep)
  realWorldHashtag = MkBinderId (MkUnique '0' 21) -- This should be 15, but ExtSTG interpreter does not recognizes it.

mutual

  public export
  data Expr : RepType {- Representation of return value -} -> Type where
    StgApp
      :  {q : RepType}
      -> BinderId q    -- function
      -> (List ArgSg)  -- arguments; may be empty, when arguments are empty, the application
                       -- is interpreted as variable lookup.
      -> (r : RepType) -- result type
      -> Expr r

    StgLit : (l : Lit) -> Expr (litRepType l)

      -- StgConApp is vital for returning unboxed tuples or sums
      -- which can't be let-bound first
    StgConApp
         -- TODO: Use the DataConRep info to determine the representation of the arguments
         -- and the content of the unboxed sum parameter
      :  {r : DataConRep}
      -> (DataConId r)        -- DataCon
      -> (StgConAppArgType r) -- Saturated
      -> Expr (SingleValue LiftedRep)

    StgOpApp
      :  {name : String} -> {args : List PrimRep} -> {ret : PrimRep}
      -> PrimOp name args ret
      -> StgOpArgType args
      -- Maybe TyConId -- Result type name (required only for tagToEnum wrapper generator)
      --               -- which we dont want to use in IdrisExtSTG
      -> Expr (SingleValue ret)

    StgCase
      :  {r : RepType}
      -> (a : AltType)
      -> Expr (altRepType a)
         -- The thing to examine
      -> SBinder (altRepType a)
         -- binds the result of evaluating the scrutinee
         -- The Representation of the Binder should have the same SingleValue as the PrimAlt
      -> (List (Alt (altRepType a) r))
         -- The DEFAULT case is always the first one, if there is any
      -> Expr r

    StgLet
      :  Binding -- right hand sides -- TODO
      -> Expr r  -- body
      -> Expr r

    StgLetNoEscape
      :  Binding -- right hand sides -- TODO
      -> Expr r  -- body
      -> Expr r

    -- Helper data constructor when deep investigation is needed.
    -- StgUndefined : (r : RepType) -> Expr r

  public export
  data Alt : (altConRep : RepType) -> (exprRep : RepType) -> Type where
    MkAlt
      :  (a : AltCon ar)
      -> (bs : AltBinderType a)
      -> Expr er
      -> Alt ar er

  public export
  data Rhs : Type where
    StgRhsClosure
      :  {r : RepType}
      -> UpdateFlag
        -- TODO: Use
      -> (List SBinderSg) -- arguments; if empty, then not a function. The order is important
      -> (Expr r) -- body
      -> Rhs
        -- (Expr r) -> Rhs r
    StgRhsCon
      : -- TODO: Use the DataConRep to determine the Argument list
         DataConIdSg -- DataCon
      -> (List ArgSg) -- Args
      -> Rhs
      -- LiftedRep, because we don't need to introduce Unlifted < Lifted subtyping relation

  public export
  data Binding
    = StgNonRec (SBinder (SingleValue LiftedRep)) Rhs
    | StgRec    (List (SBinder (SingleValue LiftedRep), Rhs))

  public export
  data TopBinding
    = StgTopLifted Binding -- SingleValue Lifted OR UnLifted, in HAskell is LiftedOnly
    | StgTopStringLit (SBinder (SingleValue AddrRep)) String
      -- SBinder binds a variable which will hold an Address in STG: AddrRep or Addr#

public export
data ForeignSrcLang
  = LangC
  | LangCxx
  | LangObjc
  | LangObjxcc
  | LangAsm
  | RawObject

||| These modules are the responsible for STG boxing the values that come from C.
public export
data ForeignStubs
  = NoStubs
  | MkForeignStubs
      String -- CHeader
      String -- CSource

public export
record Module where
  constructor MkModule
  Phase              : String       -- For Debug only
  ModuleUnitId       : UnitId       -- Haskell package, could be main
  Name               : ModuleName   -- It should be Main
  SourceFilePath     : String       -- For Debug only
  ForeignStubs       : ForeignStubs -- For FFI, to be improved
  HasForeignExported : Bool         -- Is Idris function exported through FFI
  Dependency         : List (UnitId, List ModuleName)
                       -- It should be empty for now
  ExternalTopIds     : List (UnitId, List (ModuleName, List SBinderSg))
                       -- Same as above, just referred named included
  TyCons             : List (UnitId, List (ModuleName, List STyCon))
                       -- The types that are referred in the module, even if they are defined here or somewhere else
  TopBindings        : List TopBinding
                       -- Definition of functions, found in top bindings.
  ForeignFiles       : List (ForeignSrcLang, FilePath)
                       -- To be clarified, this is something internal to GHC codegen. It should be empty for now.
