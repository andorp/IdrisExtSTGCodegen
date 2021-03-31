module Idris.Codegen.ExtSTG.STG

import public Data.List

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

Show PrimElemRep where
  show Int8ElemRep   = "Int8ElemRep"
  show Int16ElemRep  = "Int16ElemRep"
  show Int32ElemRep  = "Int32ElemRep"
  show Int64ElemRep  = "Int64ElemRep"
  show Word8ElemRep  = "Word8ElemRep"
  show Word16ElemRep = "Word16ElemRep"
  show Word32ElemRep = "Word32ElemRep"
  show Word64ElemRep = "Word64ElemRep"
  show FloatElemRep  = "FloatElemRep"
  show DoubleElemRep = "DoubleElemRep"

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

export
Show PrimRep where
  showPrec d VoidRep      = "VoidRep"
  showPrec d LiftedRep    = "LiftedRep"
  showPrec d UnliftedRep  = "UnliftedRep"
  showPrec d Int8Rep      = "Int8Rep"
  showPrec d Int16Rep     = "Int16Rep"
  showPrec d Int32Rep     = "Int32Rep"
  showPrec d Int64Rep     = "Int64Rep"
  showPrec d IntRep       = "IntRep"
  showPrec d Word8Rep     = "Word8Rep"
  showPrec d Word16Rep    = "Word16Rep"
  showPrec d Word32Rep    = "Word32Rep"
  showPrec d Word64Rep    = "Word64Rep"
  showPrec d WordRep      = "WordRep"
  showPrec d AddrRep      = "AddrRep"
  showPrec d FloatRep     = "FloatRep"
  showPrec d DoubleRep    = "DoubleRep"
  showPrec d (VecRep n p) = showCon d "VecRep" $ showArg n ++ " " ++ showArg p

||| SingeValue LiftedRep = Simple Algebraic value
public export
data RepType
  = SingleValue    PrimRep
  | UnboxedTuple   (List PrimRep)
  | PolymorphicRep

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
  | UnboxedTupleCon Int
  -- TODO: Add the PrimRep list here, or a different constructor.

-- export
Show DataConRep where
  showPrec p (AlgDataCon reps) = showCon p "AlgDataCon" $ showArg reps
  showPrec p (UnboxedTupleCon n) = showCon p "UnboxedTupleCon" $ showArg n

public export
data DataConId : DataConRep -> Type where
  MkDataConId : {0 r : DataConRep} -> Unique -> DataConId r

export
Show (DataConId q) where
  show (MkDataConId u) = show u

public export
DataConIdPi : Type
DataConIdPi = (r : DataConRep ** DataConId r)

export
Show DataConIdPi where
  show (r ** d) = show r ++ " ** " ++ show d

public export
mkDataConIdPi : {r : DataConRep} -> DataConId r -> DataConIdPi
mkDataConIdPi {r} d = (r ** d)

public export
unsafeMkDataConIdPi : (r : DataConRep) -> Unique -> (r ** DataConId r)
unsafeMkDataConIdPi r u = (r ** MkDataConId u)

export
dataConUnique : DataConId r -> Unique
dataConUnique (MkDataConId u) = u

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
data IdDetails
  = VanillaId
  | FExportedId
  | RecSelId
  | DataConWorkId DataConIdPi
  | DataConWrapId DataConIdPi
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
getBinderIdUnique : BinderId r -> Unique
getBinderIdUnique (MkBinderId u) = u

public export
BinderIdPi : Type
BinderIdPi = (r : RepType ** BinderId r)

export
mkBinderIdPi : {r : RepType} -> BinderId r -> BinderIdPi
mkBinderIdPi {r} b = (r ** b)

namespace SBinder

  public export
  data SBinder : RepType -> Type where
    MkSBinder
      :  (binderName : Name)
      -> (binderRep : RepType)
      -> (binderId : BinderId binderRep)
      -> (binderTypeSig : Name)
      -> (binderScope : Scope)
      -> (binderDetails : IdDetails)
      -> (binderInfo : IdInfo)
      -> (binderDefLoc : SrcSpan)
      -> SBinder binderRep

  public export
  SBinderPi : Type
  SBinderPi = (r : RepType ** SBinder r)

  public export
  LiftedRepBinder : Type
  LiftedRepBinder = SBinder (SingleValue LiftedRep)

  public export
  mkSBinderPi : {r : RepType} -> SBinder r -> SBinderPi
  mkSBinderPi {r} s = (r ** s)

  public export
  binderName : SBinder r -> Name
  binderName (MkSBinder n r i t s d f c) = n

  public export
  binderId : SBinder r -> BinderId r
  binderId (MkSBinder n r i t s d f c) = i

  public export
  binderRep : SBinder r -> RepType
  binderRep (MkSBinder n r i t s d f c) = r

  public export
  binderTypeSig : SBinder r -> Name
  binderTypeSig (MkSBinder n r i t s d f c) = t

  public export
  binderScope : SBinder r -> Scope
  binderScope (MkSBinder n r i t s d f c) = s

  public export
  binderDetails : SBinder r -> IdDetails
  binderDetails (MkSBinder n r i t s d f c) = d

  public export
  binderInfo : SBinder r -> IdInfo
  binderInfo (MkSBinder n r i t s d f c) = f

  public export
  binderDefLoc : SBinder r -> SrcSpan
  binderDefLoc (MkSBinder n r i t s d f c) = c

export
getSBinderIdPi : SBinderPi -> BinderIdPi
getSBinderIdPi (r ** b) = (r ** binderId b)

namespace SDataCon

  public export
  data SDataCon : DataConRep -> Type where
    MkSDataCon
      :  (dataConName : Name)
      -> (dataConRep  : DataConRep)
      -> (dataConId   : DataConId dataConRep)
      -> (dataConWorker : LiftedRepBinder) -- TODO: It needs for the codegen, but it is not clear its real purpose.
      -> (dataConDefLoc : SrcSpan)
      -> SDataCon dataConRep

  public export
  name : SDataCon r -> Name
  name (MkSDataCon n i r b s) = n

  public export
  ident : SDataCon r -> DataConId r
  ident (MkSDataCon n r i b s) = i

  public export
  rep : SDataCon r -> DataConRep
  rep (MkSDataCon n r i b s) = r

  public export
  worker : SDataCon r -> LiftedRepBinder
  worker (MkSDataCon n r i b s) = b

  public export
  defLoc : SDataCon r -> SrcSpan
  defLoc (MkSDataCon n r i b s) = s

  public export
  SDataConPi : Type
  SDataConPi = (r ** SDataCon r)

  export
  mkSDataConPi : {r : DataConRep} -> SDataCon r -> SDataConPi
  mkSDataConPi {r} d = (r ** d)

export
Show (SDataCon r) where
  show s = show (name s)

export
Show SDataConPi where
  show (r ** d) = show d

public export
record STyCon where
  constructor MkSTyCon
  Name     : Name
  Id       : TyConId
  DataCons : (List SDataConPi)
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
Show Lit where
  showPrec d (LitChar     c) = showCon d "LitChar" $ showArg c
  showPrec d (LitString   s) = showCon d "LitString" $ showArg s
  showPrec d LitNullAddr     = "LitNullAddr"
  showPrec d (LitFloat    x) = showCon d "LitFloat" $ showArg x
  showPrec d (LitDouble   x) = showCon d "LitDouble" $ showArg x
  showPrec d (LitLabel    s l) = showCon d "LitLabel" $ showArg s ++ " ..."
  showPrec d (LitNumber   l i) = showCon d "LitNumber" $ "... " ++ showArg i

public export
data Arg
  = StgVarArg BinderIdPi -- TODO: Index Arg with RepType
  | StgLitArg Lit
  | StgVoid

public export
data AltType
  = PolyAlt -- Instead of ForceBoxed
  | MultiValAlt Nat
  | PrimAlt     PrimRep
  | AlgAlt      TyConId

public export
litRepType : Lit -> RepType
litRepType (LitChar   _) = SingleValue Word8Rep
litRepType (LitString _) = SingleValue AddrRep
litRepType (LitNullAddr) = SingleValue AddrRep
litRepType (LitFloat  _) = SingleValue FloatRep
litRepType (LitDouble _) = SingleValue DoubleRep
litRepType (LitLabel _ _) = SingleValue AddrRep
litRepType (LitNumber LitNumInt    _) = SingleValue IntRep
litRepType (LitNumber LitNumInt64  _) = SingleValue Int64Rep
litRepType (LitNumber LitNumWord   _) = SingleValue WordRep
litRepType (LitNumber LitNumWord64 _) = SingleValue Word64Rep

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
  AltDataCon : DataConIdPi                     -> AltCon (SingleValue LiftedRep)
  AltLit     : (l : Lit) -> (0 _ : IsAltLit l) => AltCon (litRepType l)
  AltDefault : {0 r : RepType}                 -> AltCon r

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

||| BinderList, specialized list for storing special Binders in STG alternatives.
public export
data BList : List PrimRep -> Type where
  Nil  : BList []
  (::) : SBinder (SingleValue p) -> BList ps -> BList (p :: ps)

public export
toBinderList : {ps : List PrimRep} -> BList ps -> List SBinderPi
toBinderList []        = []
toBinderList (x :: xs) = mkSBinderPi x :: toBinderList xs

public export
DataConRepType : DataConRep -> Type
DataConRepType (AlgDataCon [])     = ()
DataConRepType (AlgDataCon [p])    = SBinder (SingleValue p)
DataConRepType (AlgDataCon (p0 :: p1 :: ps)) = BList (p0 :: p1 :: ps)
DataConRepType (UnboxedTupleCon n) = Void

public export
AltBinderType : AltCon r -> Type
AltBinderType (AltDataCon d) = DataConRepType (fst d)
AltBinderType (AltLit l)     = ()
AltBinderType AltDefault     = ()

public export
altRepType : AltType -> RepType
altRepType PolyAlt         = SingleValue LiftedRep -- Used only for forced values
altRepType (MultiValAlt n) = UnboxedTuple (replicate n VoidRep) -- Invalid, but unused
altRepType (PrimAlt p)     = SingleValue p
altRepType (AlgAlt t)      = SingleValue LiftedRep

public export
decLitRepType : (l : Lit) -> (r : RepType) -> Maybe (litRepType l = r)
decLitRepType (LitChar   _)              (SingleValue Word8Rep)  = Just Refl
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


mutual

  public export
  data Expr : RepType {- Representation of return value -} -> Type where
    StgApp
      :  {q : RepType}
      -> BinderId q    -- function
      -> (List Arg)    -- arguments; may be empty, when arguments are empty, the application
                       -- is interpreted as variable lookup.
      -> (r : RepType) -- result type
      -> Expr r

    StgLit : (l : Lit) -> Expr (litRepType l)

      -- StgConApp is vital for returning unboxed tuples or sums
      -- which can't be let-bound first
    StgConApp
         -- TODO: Use the DataConRep info to determine the representation of the arguments
         -- and the content of the unboxed sum parameter
      :  DataConIdPi    -- DataCon
      -> (List Arg)     -- Saturated
      -> (List RepType) -- Types: Only needed for Unboxed sums, otherwise it should be an empty list
      -> Expr (SingleValue LiftedRep)

    StgOpApp
      :  StgOp           -- Primitive operation or foreign call
      -> (List Arg)      -- Saturated
      -> (r : RepType)   -- Result Type
      -> (Maybe TyConId) -- Result Type name (required for tagToEnum wrapper generator)
      -> Expr r

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
  data Rhs -- : RepType -> Type where
    = StgRhsClosure
        UpdateFlag
        -- TODO: Use
        (List SBinderPi) -- arguments; if empty, then not a function. The order is important
        (Expr (SingleValue LiftedRep)) -- body: TODO: This could be anything
        -- (Expr r) -> Rhs r
    | StgRhsCon
        -- TODO: Use the DataConRep to determine the Argument list
        DataConIdPi -- DataCon
        (List Arg) -- Args
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
  ExternalTopIds     : List (UnitId, List (ModuleName, List (SBinder (SingleValue LiftedRep))))
                       -- Same as above, just referred named included
  TyCons             : List (UnitId, List (ModuleName, List STyCon))
                       -- The types that are referred in the module, even if they are defined here or somewhere else
  TopBindings        : List TopBinding
                       -- Definition of functions, found in top bindings.
  ForeignFiles       : List (ForeignSrcLang, FilePath)
                       -- To be clarified, this is something internal to GHC codegen. It should be empty for now.
