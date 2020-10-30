module Idris.Codegen.ExtSTG.STG -- where

public export
Name : Type
Name = String

public export
FilePath : Type
FilePath = String

public export
IdInfo : Type
IdInfo = String

public export
data Unique
  = MkUnique Char Int

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
  | LiftedRep
  | UnliftedRep -- Unlifted pointer
  | Int8Rep     -- Signed, 8 bits value
  | Int16Rep    -- Signed, 16 bits value
  | Int32Rep    -- Signed, 32 bits value
  | Int64Rep    -- Signed, 64 bits value (with 32 bits words only)
  | IntRep      -- Signed, word-sized value
  | Word8Rep    -- Unsigned, 8 bits value
  | Word16Rep   -- Unsigned, 16 bits value
  | Word32Rep   -- Unsigned, 32 bits value
  | Word64Rep   -- Unisgned, 64 bits value (with 32 bits words only)
  | WordRep     -- Unisgned, word-sized value
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
data TyConId = MkTypeConId Unique

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

public export
record SBinder where
  constructor MkSBinder
  BinderName : Name
  Id         : BinderId
  RepType    : RepType
  TypeSig    : Name
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
  TypeSig    : Name
  Scope      : Scope
  Details    : IdDetails
  UnitId     : UnitId
  Module     : ModuleName
  TopLevel   : Bool

mutual
  public export
  record TyCon where
    constructor MkTyCon
    Name     : Name
    Id       : TyConId
    UnitId   : UnitId
    Module   : ModuleName
    DataCons : List DataCon

  public export
  record DataCon where
    constructor MkDataCon
    Name   : Name
    Id     : DataConId
    UnitId : UnitId
    Module : ModuleName
    Rep    : DataConRep
    TyCon  : TyCon
    Worker : Binder

public export
record SDataCon where
  constructor MkSDataCon
  Name   : Name
  Id     : DataConId
  Rep    : DataConRep
  Worker : SBinder
  DefLoc : SrcSpan

public export
record STyCon where
  constructor MkSTyCon
  Name     : Name
  Id       : TyConId
  DataCons : (List SDataCon)
  DefLoc   : SrcSpan

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
  | LitNullAddr
  | LitFloat    Double -- TODO: Represent floats
  | LitDouble   Double
  | LitLabel    String LabelSpec
  | LitNumber   LitNumType Integer

public export
data Arg' idOcc
  = StgVarArg idOcc
  | StgLitArg Lit

public export
data AltType' tcOcc
  = PolyAlt
  | MultiValAlt Int
  | PrimAlt     PrimRep
  | AlgAlt      tcOcc

public export
data AltCon' dcOcc
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
  data Expr' idBnd idOcc dcOcc tcOcc
    = StgApp
        idOcc               -- function
        (List (Arg' idOcc)) -- arguments; may be empty
        RepType             -- result type
        (Name,Name,Name)    -- (fun core type pp, result core type pp, StgApp origin (Var/Coercion/App)

    | StgLit Lit

      -- StgConApp is vital for returning unboxed tuples or sums
      -- which can't be let-bound first
    | StgConApp
        dcOcc               -- DataCon
        (List (Arg' idOcc)) -- Saturated
        (List RepType)      -- Types

    | StgOpApp
        StgOp               -- Primitive operation or foreign call
        (List (Arg' idOcc)) -- Saturated
        RepType             -- Result Type
        (Maybe tcOcc)       -- Result Type name (required for tagToEnum wrapper generator)

    | StgCase
        (Expr' idBnd idOcc dcOcc tcOcc)       -- the thing to examine
        idBnd                                 -- binds the result of evaluating the scrutinee
        (AltType' tcOcc)
        (List (Alt' idBnd idOcc dcOcc tcOcc)) -- The DEFAULT case is always the first one, if there is any

    | StgLet
        (Binding' idBnd idOcc dcOcc tcOcc) -- right hand sides
        (Expr' idBnd idOcc dcOcc tcOcc)    -- body

    | StgLetNoEscape
        (Binding' idBnd idOcc dcOcc tcOcc) -- right hand sides
        (Expr' idBnd idOcc dcOcc tcOcc)    -- body

  public export
  record Alt' (idBnd : Type) (idOcc : Type) (dcOcc : Type) (tcOcc : Type) where
    constructor MkAlt
    Con     : AltCon' dcOcc
    Binders : List idBnd
    RHS     : Expr' idBnd idOcc dcOcc tcOcc

  public export
  data Rhs' idBnd idOcc dcOcc tcOcc
    = StgRhsClosure
        UpdateFlag
        (List idBnd)                    -- arguments; if empty, then not a function. The order is important
        (Expr' idBnd idOcc dcOcc tcOcc) -- body
    | StgRhsCon
        dcOcc               -- DataCon
        (List (Arg' idOcc)) -- Args

  public export
  data Binding' idBnd idOcc dcOcc tcOcc
    = StgNonRec idBnd (Rhs' idBnd idOcc dcOcc tcOcc)
    | StgRec    (List (idBnd, Rhs' idBnd idOcc dcOcc tcOcc))

  public export
  data TopBinding' idBnd idOcc dcOcc tcOcc
    = StgTopLifted    (Binding' idBnd idOcc dcOcc tcOcc)
    | StgTopStringLit idBnd String

public export
data ForeignSrcLang
  = LangC
  | LangCxx
  | LangObjc
  | LangObjxcc
  | LangAsm
  | RawObject

public export
data ForeignStubs
  = NoStubs
  | MkForeignStubs
      String -- CHeader
      String -- CSource

public export
record Module' idBnd idOcc dcOcc tcBnd tcOcc where
  constructor MkModule
  Phase              : String
  ModuleUnitId       : UnitId
  Name               : ModuleName
  SourceFilePath     : String
  ForeignStubs       : ForeignStubs
  HasForeignExported : Bool
  Dependency         : List (UnitId, List ModuleName)
  ExternalTopIds     : List (UnitId, List (ModuleName, List idBnd))
  TyCons             : List (UnitId, List (ModuleName, List tcBnd))
  TopBindings        : List (TopBinding' idBnd idOcc dcOcc tcOcc)
  ForeignFiles       : List (ForeignSrcLang, FilePath)

public export
SModule : Type
SModule = Module' SBinder BinderId DataConId STyCon TyConId

public export
STopBinding : Type
STopBinding = TopBinding' SBinder BinderId DataConId TyConId

public export
SBinding : Type
SBinding = Binding' SBinder BinderId DataConId TyConId

public export
SExpr : Type
SExpr = Expr' SBinder BinderId DataConId TyConId

public export
SRhs : Type
SRhs = Rhs' SBinder BinderId DataConId TyConId

public export
SAlt : Type
SAlt = Alt' SBinder BinderId DataConId TyConId

public export
SAltCon : Type
SAltCon = AltCon' DataConId

public export
SAltType : Type
SAltType = AltType' TyConId

public export
SArg : Type
SArg = Arg' BinderId

Module : Type
Module = Module' Binder Binder DataCon TyCon TyCon

public export
TopBinding : Type
TopBinding = TopBinding' Binder Binder DataCon TyCon

Binding : Type
Binding = Binding' Binder Binder DataCon TyCon

public export
Expr : Type
Expr = Expr' Binder Binder DataCon TyCon

Rhs : Type
Rhs = Rhs' Binder Binder DataCon TyCon

public export
Alt : Type
Alt = Alt' Binder Binder DataCon TyCon

AltCon : Type
AltCon = AltCon' DataCon

AltType : Type
AltType = AltType' TyCon

Arg : Type
Arg = Arg' Binder
