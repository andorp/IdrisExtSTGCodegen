-- TODO: Remove this module.
module Idris.Codegen.ExtSTG.Pretty

import Idris.Codegen.ExtSTG.STG

import Text.PrettyPrint.Prettyprinter.Doc
import Text.PrettyPrint.Prettyprinter.Symbols

infixr 6 <$$>
||| Concatenates two documents with a line in between.
export
(<$$>) : Doc ann -> Doc ann -> Doc ann
x <$$> y = x <+> line <+> y

export
Pretty Unique where
  pretty (MkUnique c i) = pretty "MkUnique" <$$> indent 2 (pretty c <+> space <+> pretty i)

export
Pretty PrimElemRep where
  pretty Int8ElemRep   = pretty "Int8Rep"
  pretty Int16ElemRep  = pretty "Int16Rep"
  pretty Int32ElemRep  = pretty "Int32Rep"
  pretty Int64ElemRep  = pretty "Int64Rep"
  pretty Word8ElemRep  = pretty "Word8Rep"
  pretty Word16ElemRep = pretty "Word16Rep"
  pretty Word32ElemRep = pretty "Word32Rep"
  pretty Word64ElemRep = pretty "Word64Rep"
  pretty FloatElemRep  = pretty "FloatRep"
  pretty DoubleElemRep = pretty "DoubleRep"

export
Pretty PrimRep where
  pretty VoidRep      = pretty "VoidRep"
  pretty LiftedRep    = pretty "LiftedRep"
  pretty UnliftedRep  = pretty "UnliftedRep"
  pretty Int8Rep      = pretty "Int8Rep"
  pretty Int16Rep     = pretty "Int16Rep"
  pretty Int32Rep     = pretty "Int32Rep"
  pretty Int64Rep     = pretty "Int64Rep"
  pretty IntRep       = pretty "IntRep"
  pretty Word8Rep     = pretty "Word8Rep"
  pretty Word16Rep    = pretty "Word16Rep"
  pretty Word32Rep    = pretty "Word32Rep"
  pretty Word64Rep    = pretty "Word64Rep"
  pretty WordRep      = pretty "WordRep"
  pretty AddrRep      = pretty "AddrRep"
  pretty FloatRep     = pretty "FloatRep"
  pretty DoubleRep    = pretty "DoubleRep"
  pretty (VecRep n r) = pretty "VecRep" <$$> indent 2 (pretty n <+> space <+> pretty r)

export
Pretty RepType where
  pretty (SingleValue p)   = pretty "SingleValue"  <$$> indent 2 (pretty p)
  pretty (UnboxedTuple ps) = pretty "UnboxedTuple" <$$> indent 2 (pretty ps)
  pretty PolymorphicRep    = pretty "PolymorphicRep"

export
Pretty TyConId where
  pretty (MkTyConId u) = pretty "MkTyConId" <$$> indent 2 (pretty u)

export
Pretty DataConId where
  pretty (MkDataConId u) = pretty "MkDataConId" <$$> indent 2 (pretty u)

export
Pretty DataConRep where
  pretty (AlgDataCon      ps) = pretty "AlgDataCon"      <$$> indent 2 (pretty ps)
  pretty (UnboxedTupleCon n)  = pretty "UnboxedTupleCon" <$$> indent 2 (pretty n)

export
Pretty ModuleName where
  pretty (MkModuleName n) = pretty "MkModuleName" <$$> indent 2 (pretty n)

export
Pretty UnitId where
  pretty (MkUnitId n) = pretty "MkUnitId" <$$> indent 2 (pretty n)

export
Pretty BinderId where
  pretty (MkBinderId u) = pretty "MkBinderId" <$$> indent 2 (pretty u)

export
Pretty IdDetails where
  pretty VanillaId          = pretty "VanillaId"
  pretty FExportedId        = pretty "FExportedId"
  pretty RecSelId           = pretty "RecSelId"
  pretty (DataConWorkId u)  = pretty "DataConWorkId" <$$> indent 2 (pretty u)
  pretty (DataConWrapId u)  = pretty "DataConWrapId" <$$> indent 2 (pretty u)
  pretty ClassOpId          = pretty "ClassOpId"
  pretty PrimOpId           = pretty "PrimOpId"
  pretty TickBoxOpId        = pretty "TickBoxOpId"
  pretty DFunId             = pretty "DFunId"
  pretty CoVarId            = pretty "CoVarId"
  pretty (JoinId n)         = pretty "JoinId" <$$> indent 2 (pretty n)

export
Pretty Scope where
  pretty LocalScope       = pretty "LocalScope"
  pretty GlobalScope      = pretty "GlobalScope"
  pretty HaskellExported  = pretty "HaskellExported"
  pretty ForeignExported  = pretty "ForeignExported"

export
Pretty SBinder where
  pretty (MkSBinder b i r t s d _ _) = vsep
    [ pretty "MkSBinder"
    , pretty '{'
    , pretty "BinderName" <+> pretty " = " <+> pretty b
    , pretty "Id"         <+> pretty " = " <+> pretty i
    , pretty "RepType"    <+> pretty " = " <+> pretty r
    , pretty "TypeSig"    <+> pretty " = " <+> pretty t
    , pretty "Scope"      <+> pretty " = " <+> pretty s
    , pretty "Details"    <+> pretty " = " <+> pretty d
    , pretty '}'
    ]

export
Pretty Binder where
  pretty (MkBinder b i r t s d u m l) = vsep
    [ pretty "MkBinder"
    , pretty '{'
    , pretty "BinderName" <+> pretty " = " <+> pretty b
    , pretty "Id"         <+> pretty " = " <+> pretty i
    , pretty "RepType"    <+> pretty " = " <+> pretty r
    , pretty "TypeSig"    <+> pretty " = " <+> pretty t
    , pretty "Scope"      <+> pretty " = " <+> pretty s
    , pretty "Details"    <+> pretty " = " <+> pretty d
    , pretty "UnitId"     <+> pretty " = " <+> pretty u
    , pretty "Module"     <+> pretty " = " <+> pretty m
    , pretty "TopLevel"   <+> pretty " = " <+> pretty l
    , pretty '}'
    ]

mutual
  export
  Pretty TyCon where
    pretty (MkTyCon n i u m d) = vsep
      [ pretty "MkTyCon"
      , pretty '{'
      , pretty "Name"     <+> pretty " = " <+> pretty n
      , pretty "Id"       <+> pretty " = " <+> pretty i
      , pretty "UnitId"   <+> pretty " = " <+> pretty u
      , pretty "Module"   <+> pretty " = " <+> pretty m
      , pretty "DataCons" <+> pretty " = " <+> pretty d
      , pretty '}'
      ]

  export
  Pretty DataCon where
    pretty (MkDataCon n i u m r t w) = vsep
      [ pretty "MkDataCon"
      , pretty '{'
      , pretty "Name"   <+> pretty " = " <+> pretty n
      , pretty "Id"     <+> pretty " = " <+> pretty i
      , pretty "UnitId" <+> pretty " = " <+> pretty u
      , pretty "Module" <+> pretty " = " <+> pretty m
      , pretty "Rep"    <+> pretty " = " <+> pretty r
      , pretty "TyCon"  <+> pretty " = " <+> pretty t
      , pretty "Worker" <+> pretty " = " <+> pretty w
      , pretty '}'
      ]

export
Pretty SDataCon where
  pretty (MkSDataCon n i w _ _) = vsep
    [ pretty "MkSDataCon"
    , pretty '{'
    , pretty "Name"   <+> pretty " = " <+> pretty n
    , pretty "Id"     <+> pretty " = " <+> pretty i
    , pretty "Worker" <+> pretty " = " <+> pretty w
    , pretty '}'
    ]

export
Pretty LitNumType where
  pretty LitNumInt    = pretty "LitNumInt"
  pretty LitNumInt64  = pretty "LitNumInt64"
  pretty LitNumWord   = pretty "LitNumWord"
  pretty LitNumWord64 = pretty "LitNumWord64"

export
Pretty LabelSpec where
  pretty (FunctionLabel n) = pretty "FunctionLabel" <+> pretty n
  pretty DataLabel         = pretty "DataLabel"

export
Pretty Lit where
  pretty (LitChar     c)    = pretty "LitChar"    <+> indent 2 (pretty c)
  pretty (LitString   s)    = pretty "LitString"  <+> indent 2 (pretty s)
  pretty LitNullAddr        = pretty "LitNullAddr"
  pretty (LitFloat    d)    = pretty "LitFloat"   <+> indent 2 (pretty d)
  pretty (LitDouble   d)    = pretty "LitDouble"  <+> indent 2 (pretty d)
  pretty (LitLabel    s l)  = pretty "LitLabel"   <+> indent 2 (pretty s <+> space <+> pretty l)
  pretty (LitNumber   l i)  = pretty "LitNumber"  <+> indent 2 (pretty l <+> space <+> pretty i)

export
Pretty i => Pretty (Arg_ i) where
  pretty (StgVarArg idOcc) = pretty "StgVarArg" <$$> indent 2 (pretty idOcc)
  pretty (StgLitArg l)     = pretty "StgLitArg" <$$> indent 2 (pretty l)

export
Pretty t => Pretty (AltType_ t) where
  pretty PolyAlt         = pretty "PolyAlt"
  pretty (MultiValAlt n) = pretty "MultiValAlt" <$$> indent 2 (pretty n)
  pretty (PrimAlt     p) = pretty "PrimAlt"     <$$> indent 2 (pretty p)
  pretty (AlgAlt      t) = pretty "AlgAlt"      <$$> indent 2 (pretty t)

export
Pretty d => Pretty (AltCon_ d) where
  pretty (AltDataCon d) = pretty "AltDataCon" <$$> indent 2 (pretty d)
  pretty (AltLit     l) = pretty "AltLit"     <$$> indent 2 (pretty l)
  pretty AltDefault     = pretty "AltDefault"

export
Pretty UpdateFlag where
  pretty ReEntrant    = pretty "ReEntrant"
  pretty Updatable    = pretty "Updatable"
  pretty SingleEntry  = pretty "SingleEntry"

export
Pretty CCallConv where
  pretty MkCCallConv        = pretty "MkCCallConv"
  pretty CApiConv           = pretty "CApiConv"
  pretty StdCallConv        = pretty "StdCallConv"
  pretty PrimCallConv       = pretty "PrimCallConv"
  pretty JavaScriptCallConv = pretty "JavaScriptCallConv"

export
Pretty SourceText where
  pretty (MkSourceText s) = pretty "MkSourceText" <$$> indent 2 (pretty s)
  pretty (NoSourceText)   = pretty "NoSourceText"

export
Pretty CCallTarget where
  pretty (StaticTarget t s u b) = vsep
    [ pretty "StaticTarget"
    , pretty t
    , pretty s
    , pretty u
    , pretty b
    ]
  pretty DynamicTarget = pretty "DynamicTarget"

export
Pretty Safety where
  pretty PlaySafe           = pretty "PlaySafe"
  pretty PlayInterruptible  = pretty "PlayInterruptible"
  pretty PlayRisky          = pretty "PlayRisky"

export
Pretty ForeignCall where
  pretty (MkForeignCall t c s) = vsep
    [ pretty "MkForeignCall"
    , pretty t
    , pretty c
    , pretty s
    ]

export
Pretty PrimCall where
  pretty (MkPrimCall s u) = vsep
    [ pretty "MkPrimCall"
    , pretty s
    , pretty u
    ]

export
Pretty StgOp where
  pretty (StgPrimOp     n) = pretty "StgPrimOp"     <$$> indent 2 (pretty n)
  pretty (StgPrimCallOp p) = pretty "StgPrimCallOp" <$$> indent 2 (pretty p)
  pretty (StgFCallOp    f) = pretty "StgFCallOp"    <$$> indent 2 (pretty f)

mutual
  export
  (Pretty idBnd, Pretty idOcc, Pretty dcOcc, Pretty tcOcc) =>
  Pretty (Expr_ idBnd idOcc dcOcc tcOcc) where
    pretty (StgApp i a t n) = vsep
      [ pretty "StgApp"
      , pretty i
      , pretty a
      , pretty t
      , pretty n
      ]

    pretty (StgLit l) = vsep
      [ pretty "StgLit"
      , pretty l
      ]

    pretty (StgConApp d a t) = vsep
      [ pretty "StgConApp"
      , pretty d
      , pretty a
      , pretty t
      ]

    pretty (StgOpApp o a t rt) = vsep
      [ pretty "StgOpApp"
      , pretty o
      , pretty a
      , pretty t
      , pretty rt
      ]

    pretty (StgCase e i a as) = vsep
      [ pretty "StgCase"
      , pretty e
      , pretty i
      , pretty a
      , pretty as
      ]

    pretty (StgLet b e) = vsep
      [ pretty "StgLet"
      , pretty b
      , pretty e
      ]

    pretty (StgLetNoEscape b e) = vsep
      [ pretty "StgLetNoEscape"
      , pretty b
      , pretty e
      ]

  export
  (Pretty idBnd, Pretty idOcc, Pretty dcOcc, Pretty tcOcc) =>
  Pretty (Alt_ idBnd idOcc dcOcc tcOcc) where
    pretty (MkAlt c b r) = vsep
      [ pretty "MkAlt"
      , pretty c
      , pretty b
      , pretty r
      ]

  export
  (Pretty idBnd, Pretty idOcc, Pretty dcOcc, Pretty tcOcc) =>
  Pretty (Rhs_ idBnd idOcc dcOcc tcOcc) where
    pretty (StgRhsClosure u i e)  = pretty "StgRhsClosure" <$$> indent 2 (pretty u <$$> pretty i <$$> pretty e)
    pretty (StgRhsCon d a)        = pretty "StgRhsCon"     <$$> indent 2 (pretty d <$$> pretty a)

  export
  (Pretty idBnd, Pretty idOcc, Pretty dcOcc, Pretty tcOcc) =>
  Pretty (Binding_ idBnd idOcc dcOcc tcOcc) where
    pretty (StgNonRec idBnd rhs)  = pretty "StgNonRec"  <$$> indent 2 (pretty idBnd <$$> pretty rhs)
    pretty (StgRec    rhs)        = pretty "StgRec"     <$$> indent 2 (pretty rhs)

  export
  (Pretty idBnd, Pretty idOcc, Pretty dcOcc, Pretty tcOcc) =>
  Pretty (TopBinding_ idBnd idOcc dcOcc tcOcc) where
    pretty (StgTopLifted binding)       = pretty "StgTopLifted" <$$> indent 2 (pretty binding)
    pretty (StgTopStringLit idBnd lit)  = pretty "StgTopStringLit" <$$> indent 2 (pretty idBnd <$$> pretty lit)

export
Pretty ForeignSrcLang where
  pretty LangC      = pretty "LangC"
  pretty LangCxx    = pretty "LangCxx"
  pretty LangObjc   = pretty "LangObjc"
  pretty LangObjxcc = pretty "LangObjxcc"
  pretty LangAsm    = pretty "LangAsm"
  pretty RawObject  = pretty "RawObject"

export
Pretty ForeignStubs where
  pretty NoStubs              = pretty "NoStubs"
  pretty (MkForeignStubs h s) = pretty "MkForeignStubs" <$$> indent 2 (pretty h <++> pretty s)

export
Pretty STyCon where
  pretty (MkSTyCon n i d f) = vsep
    [ pretty "MkSTyCon"
    , pretty '{'
    , pretty "Name"     <+> pretty " = " <+> pretty n
    , pretty "Id"       <+> pretty " = " <+> pretty i
    , pretty "DataCons" <+> pretty " = " <+> pretty d
    , pretty '}'
    ]
