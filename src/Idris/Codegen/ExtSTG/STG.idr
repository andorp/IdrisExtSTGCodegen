module Idris.Codegen.ExtSTG.STG

{-
This module contains the definitions of the STG. We mirror the current internals of GHC.
-}

public export
Name : Type
Name = String

namespace FilePath
  export
  FilePath : Type
  FilePath = String

  export
  getFilePath : FilePath -> String
  getFilePath = id

  export
  mkFilePath : String -> FilePath
  mkFilePath = id

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

Eq Unique where
  (MkUnique c0 i0) == (MkUnique c1 i1) = (c0,i0) == (c1,i1)

Show Unique where
  show (MkUnique c i) = "(MkUnique " ++ show c ++ " " ++ show i ++ ")"

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
  BufSpanStart  : Int
  BufSpanEnd    : Int

public export
data SrcSpan
  = SsRealSrcSpan   RealSrcSpan (Maybe BufSpan)
  | SsUnhelpfulSpan Name

public export
data PrimElemRep
  = Int8ElemRep
  | Int16ElemRep
  | Int32ElemRep
  | Int64ElemRep
  | Word8ElemRep
  | Word16ElemRep
  | Word32ElemRep
  | Word64ElemRep
  | FloatElemRep
  | DoubleElemRep

public export
data PrimRep
  = VoidRep
  | LiftedRep   -- Boxed, in thunk or WHNF
  | UnliftedRep -- Boxed, in WHNF
  | Int8Rep     -- Unboxed, Signed, 8 bits value
  | Int16Rep    -- Unboxed, Signed, 16 bits value
  | Int32Rep    -- Unboxed, Signed, 32 bits value
  | Int64Rep    -- Unboxed, Signed, 64 bits value (with 32 bits words only)
  | IntRep      -- Unboxed, Signed, word-sized value
  | Word8Rep    -- Unboxed, Unsigned, 8 bits value
  | Word16Rep   -- Unboxed, Unsigned, 16 bits value
  | Word32Rep   -- Unboxed, Unsigned, 32 bits value
  | Word64Rep   -- Unboxed, Unisgned, 64 bits value (with 32 bits words only)
  | WordRep     -- Unboxed, Unisgned, word-sized value
  | AddrRep     -- A pointer, but *not* a Haskell value. Use (Un)liftedRep
  | FloatRep
  | DoubleRep
  | VecRep Int PrimElemRep -- A vector

public export
data RepType
  = SingleValue    PrimRep
  | UnboxedTuple   (List PrimRep)
  | PolymorphicRep

-- SingeValue LiftedRep = Simple Algebraic value

public export
data TyConId = MkTyConId Unique

export
Eq TyConId where
  (MkTyConId t0) == (MkTyConId t1) = t0 == t1

export
Show TyConId where
  show (MkTyConId t0) = "(MkTyConId " ++ show t0 ++ ")"

public export
data DataConId = MkDataConId Unique

public export
data DataConRep
  = AlgDataCon      (List PrimRep)
  | UnboxedTupleCon Int

public export
record ModuleName where
  constructor MkModuleName
  GetModuleName : Name

||| The package name of the module.
public export
record UnitId where
  constructor MkUnitId
  GetUnitId : Name

public export
data BinderId = MkBinderId Unique

public export
data IdDetails
  = VanillaId
  | FExportedId
  | RecSelId
  | DataConWorkId DataConId
  | DataConWrapId DataConId
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
record SBinder where
  constructor MkSBinder
  BinderName : Name
  Id         : BinderId
  RepType    : RepType
  TypeSig    : Name      -- Scaffolding, it will be removed
  Scope      : Scope
  Details    : IdDetails
  Info       : IdInfo
  DefLoc     : SrcSpan

public export
record Binder where
  constructor MkBinder
  BinderName : Name
  Id         : BinderId
  RepType    : RepType
  TypeSig    : Name       -- Scaffolding, it will be removed
  Scope      : Scope
  Details    : IdDetails
  UnitId     : UnitId
  Module     : ModuleName
  TopLevel   : Bool

public export
record SDataCon where
  constructor MkSDataCon
  Name   : Name
  Id     : DataConId
  Rep    : DataConRep
  Worker : SBinder -- TODO: It needs for the codegen, but it is not clear its real purpose.
  DefLoc : SrcSpan

public export
record STyCon where
  constructor MkSTyCon
  Name     : Name
  Id       : TyConId
  DataCons : (List SDataCon)
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
  | DataLabel

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
  | LitNumber   LitNumType Integer

public export
data Arg_ idOcc
  = StgVarArg idOcc
  | StgLitArg Lit
  | StgVoid

public export
data AltType_ tcOcc
  = PolyAlt
  | MultiValAlt Int
  | PrimAlt     PrimRep
  | AlgAlt      tcOcc

public export
data AltCon_ dcOcc
  = AltDataCon dcOcc
  | AltLit     Lit
  | AltDefault

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

public export
data StgOp
  = StgPrimOp     Name
  | StgPrimCallOp PrimCall
  | StgFCallOp    ForeignCall

mutual
  public export
  data Expr_ idBnd idOcc dcOcc tcOcc
    = StgApp
        idOcc               -- function
        (List (Arg_ idOcc)) -- arguments; may be empty, when arguments are empty, the application
                            -- is interpreted as variable lookup.
        RepType             -- result type

    | StgLit Lit

      -- StgConApp is vital for returning unboxed tuples or sums
      -- which can't be let-bound first
    | StgConApp
        dcOcc               -- DataCon
        (List (Arg_ idOcc)) -- Saturated
        (List RepType)      -- Types: Only needed for Unboxed sums, otherwise it should be an empty list

    | StgOpApp
        StgOp               -- Primitive operation or foreign call
        (List (Arg_ idOcc)) -- Saturated
        RepType             -- Result Type
        (Maybe tcOcc)       -- Result Type name (required for tagToEnum wrapper generator)

    | StgCase
        (Expr_ idBnd idOcc dcOcc tcOcc)       -- the thing to examine
        idBnd                                 -- binds the result of evaluating the scrutinee
        (AltType_ tcOcc)
        (List (Alt_ idBnd idOcc dcOcc tcOcc)) -- The DEFAULT case is always the first one, if there is any

    | StgLet
        (Binding_ idBnd idOcc dcOcc tcOcc) -- right hand sides
        (Expr_ idBnd idOcc dcOcc tcOcc)    -- body

    | StgLetNoEscape
        (Binding_ idBnd idOcc dcOcc tcOcc) -- right hand sides
        (Expr_ idBnd idOcc dcOcc tcOcc)    -- body

  public export
  record Alt_ (idBnd : Type) (idOcc : Type) (dcOcc : Type) (tcOcc : Type) where
    constructor MkAlt
    Con     : AltCon_ dcOcc
    Binders : List idBnd
    RHS     : Expr_ idBnd idOcc dcOcc tcOcc

  public export
  data Rhs_ idBnd idOcc dcOcc tcOcc
    = StgRhsClosure
        UpdateFlag
        (List idBnd)                    -- arguments; if empty, then not a function. The order is important
        (Expr_ idBnd idOcc dcOcc tcOcc) -- body
    | StgRhsCon
        dcOcc               -- DataCon
        (List (Arg_ idOcc)) -- Args

  public export
  data Binding_ idBnd idOcc dcOcc tcOcc
    = StgNonRec idBnd (Rhs_ idBnd idOcc dcOcc tcOcc)
    | StgRec    (List (idBnd, Rhs_ idBnd idOcc dcOcc tcOcc))

  public export
  data TopBinding_ idBnd idOcc dcOcc tcOcc
    = StgTopLifted    (Binding_ idBnd idOcc dcOcc tcOcc)
    | StgTopStringLit idBnd String -- idBnd binds a variable which will hold an Address in STG: AddrRep or Addr#

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
record Module_ idBnd idOcc dcOcc tcBnd tcOcc where
  constructor MkModule
  Phase              : String       -- For Debug only
  ModuleUnitId       : UnitId       -- Haskell package, could be main
  Name               : ModuleName   -- It should be Main
  SourceFilePath     : String       -- For Debug only
  ForeignStubs       : ForeignStubs -- For FFI, to be improved
  HasForeignExported : Bool         -- Is Idris function exported through FFI
  Dependency         : List (UnitId, List ModuleName)
                       -- It should be empty for now
  ExternalTopIds     : List (UnitId, List (ModuleName, List idBnd))
                       -- Same as above, just referred named included
  TyCons             : List (UnitId, List (ModuleName, List tcBnd))
                       -- The types that are reffered in the module, even if they are defined here or somewhere else
  TopBindings        : List (TopBinding_ idBnd idOcc dcOcc tcOcc)
                       -- Definition of functions, found in top bindings.
  ForeignFiles       : List (ForeignSrcLang, FilePath)
                       -- To be clarified, this is something internal to GHC codegen. It should be empty for now.

public export
SModule : Type
SModule = Module_ SBinder BinderId DataConId STyCon TyConId

public export
STopBinding : Type
STopBinding = TopBinding_ SBinder BinderId DataConId TyConId

public export
SBinding : Type
SBinding = Binding_ SBinder BinderId DataConId TyConId

public export
SExpr : Type
SExpr = Expr_ SBinder BinderId DataConId TyConId

public export
SRhs : Type
SRhs = Rhs_ SBinder BinderId DataConId TyConId

public export
SAlt : Type
SAlt = Alt_ SBinder BinderId DataConId TyConId

public export
SAltCon : Type
SAltCon = AltCon_ DataConId

public export
SAltType : Type
SAltType = AltType_ TyConId

public export
SArg : Type
SArg = Arg_ BinderId
