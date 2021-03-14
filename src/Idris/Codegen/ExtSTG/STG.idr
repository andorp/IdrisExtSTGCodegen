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

export
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
  BufSpanStart : Int
  BufSpanEnd   : Int

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
tyConUnique : TyConId -> Unique
tyConUnique (MkTyConId u) = u

export
Eq TyConId where
  (MkTyConId t0) == (MkTyConId t1) = t0 == t1

export
Show TyConId where
  show (MkTyConId t0) = "(MkTyConId " ++ show t0 ++ ")"

public export
data DataConId = MkDataConId Unique

export
dataConUnique : DataConId -> Unique
dataConUnique (MkDataConId u) = u

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

export
Show SDataCon where
  show s = show s.Name

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
data Arg
  = StgVarArg BinderId
  | StgLitArg Lit
  | StgVoid

public export
data AltType
  = PolyAlt -- Instead of ForceBoxed
  | MultiValAlt Int
  | PrimAlt     PrimRep
  | AlgAlt      TyConId

public export
data AltCon
  = AltDataCon DataConId
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
  data Expr
    = StgApp
        BinderId    -- function
        (List Arg)  -- arguments; may be empty, when arguments are empty, the application
                    -- is interpreted as variable lookup.
        RepType     -- result type

    | StgLit Lit

      -- StgConApp is vital for returning unboxed tuples or sums
      -- which can't be let-bound first
    | StgConApp
        DataConId      -- DataCon
        (List Arg)     -- Saturated
        (List RepType) -- Types: Only needed for Unboxed sums, otherwise it should be an empty list

    | StgOpApp
        StgOp           -- Primitive operation or foreign call
        (List Arg)      -- Saturated
        RepType         -- Result Type
        (Maybe TyConId) -- Result Type name (required for tagToEnum wrapper generator)

    | StgCase
        Expr       -- the thing to examine
        SBinder    -- binds the result of evaluating the scrutinee
        AltType
        (List Alt) -- The DEFAULT case is always the first one, if there is any

    | StgLet
        Binding -- right hand sides
        Expr    -- body

    | StgLetNoEscape
        Binding -- right hand sides
        Expr    -- body

  public export
  record Alt where
    constructor MkAlt
    Con     : AltCon
    Binders : List SBinder
    RHS     : Expr

  public export
  data Rhs
    = StgRhsClosure
        UpdateFlag
        (List SBinder) -- arguments; if empty, then not a function. The order is important
        Expr          -- body
    | StgRhsCon
        DataConId   -- DataCon
        (List Arg) -- Args

  public export
  data Binding
    = StgNonRec SBinder Rhs
    | StgRec    (List (SBinder, Rhs))

  public export
  data TopBinding
    = StgTopLifted Binding
    | StgTopStringLit SBinder String -- SBinder binds a variable which will hold an Address in STG: AddrRep or Addr#

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
  ExternalTopIds     : List (UnitId, List (ModuleName, List SBinder))
                       -- Same as above, just referred named included
  TyCons             : List (UnitId, List (ModuleName, List STyCon))
                       -- The types that are referred in the module, even if they are defined here or somewhere else
  TopBindings        : List TopBinding
                       -- Definition of functions, found in top bindings.
  ForeignFiles       : List (ForeignSrcLang, FilePath)
                       -- To be clarified, this is something internal to GHC codegen. It should be empty for now.
-- * Helpers

export
topLevel : SBinder -> List SBinder -> Expr -> TopBinding
topLevel n as body = StgTopLifted $ StgNonRec n $ StgRhsClosure ReEntrant as $ body
