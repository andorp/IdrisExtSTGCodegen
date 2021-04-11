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

public export
interface SemiDecEq t where
  semiDecEq : (x : t) -> (y : t) -> Maybe (x = y)

SemiDecEq Nat where
  semiDecEq Z      Z      = Just Refl
  semiDecEq (S n1) (S n2) = do
    Refl <- semiDecEq n1 n2
    Just Refl
  semiDecEq _      _      = Nothing

SemiDecEq a => SemiDecEq (List a) where
  semiDecEq []        []        = Just Refl
  semiDecEq (x :: xs) (y :: ys) = do
    Refl <- semiDecEq x y
    Refl <- semiDecEq xs ys
    Just Refl
  semiDecEq _ _ = Nothing

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

export
SemiDecEq PrimElemRep where
  semiDecEq Int8ElemRep   Int8ElemRep   = Just Refl
  semiDecEq Int16ElemRep  Int16ElemRep  = Just Refl
  semiDecEq Int32ElemRep  Int32ElemRep  = Just Refl
  semiDecEq Int64ElemRep  Int64ElemRep  = Just Refl
  semiDecEq Word8ElemRep  Word8ElemRep  = Just Refl
  semiDecEq Word16ElemRep Word16ElemRep = Just Refl
  semiDecEq Word32ElemRep Word32ElemRep = Just Refl
  semiDecEq Word64ElemRep Word64ElemRep = Just Refl
  semiDecEq FloatElemRep  FloatElemRep  = Just Refl
  semiDecEq DoubleElemRep DoubleElemRep = Just Refl
  semiDecEq _ _ = Nothing

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
  | VecRep Nat PrimElemRep -- A vector

export
SemiDecEq PrimRep where
  semiDecEq VoidRep     VoidRep     = Just Refl
  semiDecEq LiftedRep   LiftedRep   = Just Refl
  semiDecEq UnliftedRep UnliftedRep = Just Refl
  semiDecEq Int8Rep     Int8Rep     = Just Refl
  semiDecEq Int16Rep    Int16Rep    = Just Refl
  semiDecEq Int32Rep    Int32Rep    = Just Refl
  semiDecEq Int64Rep    Int64Rep    = Just Refl
  semiDecEq IntRep      IntRep      = Just Refl
  semiDecEq Word8Rep    Word8Rep    = Just Refl
  semiDecEq Word16Rep   Word16Rep   = Just Refl
  semiDecEq Word32Rep   Word32Rep   = Just Refl
  semiDecEq Word64Rep   Word64Rep   = Just Refl
  semiDecEq WordRep     WordRep     = Just Refl
  semiDecEq AddrRep     AddrRep     = Just Refl
  semiDecEq FloatRep    FloatRep    = Just Refl
  semiDecEq DoubleRep   DoubleRep   = Just Refl
  semiDecEq (VecRep n1 p1) (VecRep n2 p2) = do
    Refl <- semiDecEq p1 p2
    Refl <- semiDecEq n1 n2
    Just Refl
  semiDecEq _ _ = Nothing

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

export
Show RepType where
  showPrec d (SingleValue p)   = showCon d "SingleValue" $ showArg p
  showPrec d (UnboxedTuple ps) = showCon d "UnboxedTuple" $ showArg ps
  showPrec d PolymorphicRep    = "PolymorphicRep"

export
SemiDecEq RepType where
  semiDecEq (SingleValue p1) (SingleValue p2) = do
    Refl <- semiDecEq p1 p2
    Just Refl
  semiDecEq (UnboxedTuple p1) (UnboxedTuple p2) = do
    Refl <- semiDecEq p1 p2
    Just Refl
  semiDecEq PolymorphicRep PolymorphicRep = Just Refl
  semiDecEq _ _ = Nothing

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
getBinderIdUnique : BinderId r -> Unique
getBinderIdUnique (MkBinderId u) = u

public export
BinderIdSg : Type
BinderIdSg = (r : RepType ** BinderId r)

export
mkBinderIdSg : {r : RepType} -> BinderId r -> BinderIdSg
mkBinderIdSg {r} b = (r ** b)

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
  SBinderSg : Type
  SBinderSg = (r : RepType ** SBinder r)

  public export
  LiftedRepBinder : Type
  LiftedRepBinder = SBinder (SingleValue LiftedRep)

  public export
  mkSBinderSg : {r : RepType} -> SBinder r -> SBinderSg
  mkSBinderSg {r} s = (r ** s)

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
getSBinderIdSg : SBinderSg -> BinderIdSg
getSBinderIdSg (r ** b) = (r ** binderId b)

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
  SDataConSg : Type
  SDataConSg = (r ** SDataCon r)

  export
  mkSDataConSg : {r : DataConRep} -> SDataCon r -> SDataConSg
  mkSDataConSg {r} d = (r ** d)

export
Show (SDataCon r) where
  show s = show (name s)

export
Show SDataConSg where
  show (r ** d) = show d

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
Show Lit where
  showPrec d (LitChar     c) = showCon d "LitChar" $ showArg c
  showPrec d (LitString   s) = showCon d "LitString" $ showArg s
  showPrec d LitNullAddr     = "LitNullAddr"
  showPrec d (LitFloat    x) = showCon d "LitFloat" $ showArg x
  showPrec d (LitDouble   x) = showCon d "LitDouble" $ showArg x
  showPrec d (LitLabel    s l) = showCon d "LitLabel" $ showArg s ++ " ..."
  showPrec d (LitNumber   l i) = showCon d "LitNumber" $ "... " ++ showArg i

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
data Arg : RepType -> Type where
  StgVarArg : BinderId r -> Arg r
  StgLitArg : {0 r : RepType} -> (l : Lit) -> (0 _ : r = litRepType l) => Arg r
  StgVoid   : Arg (SingleValue VoidRep)

public export
ArgSg : Type
ArgSg = (r : RepType ** Arg r)

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
  AltDataCon : DataConIdSg                     -> AltCon (SingleValue LiftedRep)
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

namespace BList
  ||| BinderList, specialized list for storing special Binders in STG alternatives.
  public export
  data BList : List PrimRep -> Type where
    Nil  : BList []
    (::) : SBinder (SingleValue p) -> BList ps -> BList (p :: ps)

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
      :  StgOp           -- Primitive operation or foreign call
      -> (List ArgSg)    -- Saturated
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
        (List SBinderSg) -- arguments; if empty, then not a function. The order is important
        (Expr (SingleValue LiftedRep)) -- body: TODO: This could be anything
        -- (Expr r) -> Rhs r
    | StgRhsCon
        -- TODO: Use the DataConRep to determine the Argument list
        DataConIdSg -- DataCon
        (List ArgSg) -- Args
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
