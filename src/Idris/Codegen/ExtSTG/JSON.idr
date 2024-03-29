module Idris.Codegen.ExtSTG.JSON

import Data.String

import Idris.Codegen.ExtSTG.JSONData
import Idris.Codegen.ExtSTG.STG

%default covering

export
interface ToJSON a where
  toJSON : a -> JSON

export
ToJSON a => ToJSON (List a) where
  toJSON as = JArray (map toJSON as)

export
(ToJSON a, ToJSON b) => ToJSON (a, b) where
  toJSON (a, b) = JArray [toJSON a, toJSON b]

export
-- This is a special case in the Haskell AEson library used by Ext-STG JSON.
ToJSON a => ToJSON (Maybe a) where
  toJSON Nothing = JNull
  toJSON (Just a) = toJSON a

export
ToJSON String where
  toJSON = JString

ToJSON Bool where
  toJSON = JBoolean

ToJSON Char where
  toJSON c = JString (singleton c)

ToJSON Double where
  toJSON = JNumber

ToJSON Nat where
  toJSON n = JInteger (cast n)

ToJSON Int where
  toJSON i = JInteger (cast i)

ToJSON Integer where
  toJSON i = JInteger i

ToJSON FilePath where
  toJSON = JString . getFilePath

ToJSON UnitId where
  toJSON u = JObject [ ("getUnitId", toJSON (GetUnitId u)) ]

ToJSON ModuleName where
  toJSON m = JObject [ ("getModuleName", toJSON (GetModuleName m)) ]

ToJSON ForeignStubs where
  toJSON NoStubs = JObject
    [ ("tag", toJSON "NoStubs") ]
  toJSON (MkForeignStubs h s) = JObject
    [ ("tag", toJSON "ForeignStubs")
    , ("fsCHeader", toJSON h)
    , ("fsCSource", toJSON s)
    ]

ToJSON BufSpan where
  toJSON b = JObject
    [ ("bufSpanStart", toJSON (BufSpanStart b))
    , ("bufSpanEnd", toJSON (BufSpanEnd b))
    ]

ToJSON RealSrcSpan where
  toJSON r = JObject
    [ ("srcSpanFile"  , toJSON (SpanFile  r))
    , ("srcSpanSLine" , toJSON (SpanSLine r))
    , ("srcSpanSCol"  , toJSON (SpanSCol  r))
    , ("srcSpanELine" , toJSON (SpanELine r))
    , ("srcSpanECol"  , toJSON (SpanECol  r))
    ]

-- Because of different names in STG rep in Ext-STG, we need different tags.
ToJSON SrcSpan where
  toJSON (SsRealSrcSpan r m) = JObject
    [ ("tag", JString "RealSrcSpan") -- TODO: Rename constructor
    , ("contents", JArray [toJSON r, toJSON m])
    ]
  toJSON (SsUnhelpfulSpan n) = JObject
    [ ("tag", JString "UnhelpfulSpan")
    , ("contents", toJSON n)
    ]

ToJSON Unique where
  toJSON (MkUnique c i) = JArray [toJSON c, toJSON i]

ToJSON (DataConId r) where
  toJSON (MkDataConId u) = toJSON u

ToJSON DataConIdSg where
  toJSON (r ** d) = toJSON d

ToJSON TyConId where
  toJSON (MkTyConId u) = toJSON u

ToJSON ForeignSrcLang where
  toJSON LangC      = JString "LangC"
  toJSON LangCxx    = JString "LangCxx"
  toJSON LangObjc   = JString "LangObjc"
  toJSON LangObjxcc = JString "LangObjxcc"
  toJSON LangAsm    = JString "LangAsm"
  toJSON RawObject  = JString "RawObject"

ToJSON IdDetails where
  toJSON (VanillaId)    = JObject [ ("tag", JString "VanillaId") ]
  toJSON (FExportedId)  = JObject [ ("tag", JString "FExportedId") ]
  toJSON (RecSelId)     = JObject [ ("tag", JString "RecSelId") ]
  toJSON (ClassOpId)    = JObject [ ("tag", JString "ClassOpId") ]
  toJSON (PrimOpId)     = JObject [ ("tag", JString "PrimOpId") ]
  toJSON (TickBoxOpId)  = JObject [ ("tag", JString "TickBoxOpId") ]
  toJSON (DFunId)       = JObject [ ("tag", JString "DFunId") ]
  toJSON (CoVarId)      = JObject [ ("tag", JString "CoVarId") ]
  toJSON (JoinId i) = JObject
    [ ("tag", JString "JoinId")
    , ("contents", toJSON i)
    ]
  toJSON (DataConWorkId d) = JObject
    [ ("tag", JString "DataConWorkId")
    , ("contents", toJSON d)
    ]
  toJSON (DataConWrapId d) = JObject
    [ ("tag", JString "DataConWrapId")
    , ("contents", toJSON d)
    ]

ToJSON Scope where
  toJSON LocalScope      = JString "LocalScope"
  toJSON GlobalScope     = JString "GlobalScope"
  toJSON HaskellExported = JString "HaskellExported"
  toJSON ForeignExported = JString "ForeignExported"

ToJSON UpdateFlag where
  toJSON ReEntrant    = JString "ReEntrant"
  toJSON Updatable    = JString "Updatable"
  toJSON SingleEntry  = JString "SingleEntry"

ToJSON PrimElemRep where
  toJSON Int8ElemRep    = JString "Int8ElemRep"
  toJSON Int16ElemRep   = JString "Int16ElemRep"
  toJSON Int32ElemRep   = JString "Int32ElemRep"
  toJSON Int64ElemRep   = JString "Int64ElemRep"
  toJSON Word8ElemRep   = JString "Word8ElemRep"
  toJSON Word16ElemRep  = JString "Word16ElemRep"
  toJSON Word32ElemRep  = JString "Word32ElemRep"
  toJSON Word64ElemRep  = JString "Word64ElemRep"
  toJSON FloatElemRep   = JString "FloatElemRep"
  toJSON DoubleElemRep  = JString "DoubleElemRep"

ToJSON PrimRep where
  toJSON VoidRep      = JObject [ ("tag", JString "VoidRep") ]
  toJSON LiftedRep    = JObject [ ("tag", JString "LiftedRep") ]
  toJSON UnliftedRep  = JObject [ ("tag", JString "UnliftedRep") ]
  toJSON Int8Rep      = JObject [ ("tag", JString "Int8Rep") ]
  toJSON Int16Rep     = JObject [ ("tag", JString "Int16Rep") ]
  toJSON Int32Rep     = JObject [ ("tag", JString "Int32Rep") ]
  toJSON Int64Rep     = JObject [ ("tag", JString "Int64Rep") ]
  toJSON IntRep       = JObject [ ("tag", JString "IntRep") ]
  toJSON Word8Rep     = JObject [ ("tag", JString "Word8Rep") ]
  toJSON Word16Rep    = JObject [ ("tag", JString "Word16Rep") ]
  toJSON Word32Rep    = JObject [ ("tag", JString "Word32Rep") ]
  toJSON Word64Rep    = JObject [ ("tag", JString "Word64Rep") ]
  toJSON WordRep      = JObject [ ("tag", JString "WordRep") ]
  toJSON AddrRep      = JObject [ ("tag", JString "AddrRep") ]
  toJSON FloatRep     = JObject [ ("tag", JString "FloatRep") ]
  toJSON DoubleRep    = JObject [ ("tag", JString "DoubleRep") ]
  toJSON (VecRep n p) = JObject
    [ ("tag", JString "VecRep")
    , ("contents", JArray [toJSON n, toJSON p])
    ]

ToJSON RepType where
  toJSON (SingleValue p) = JObject
    [ ("tag", JString "SingleValue")
    , ("contents", toJSON p)
    ]
  toJSON (UnboxedTuple ps) = JObject
    [ ("tag", JString "UnboxedTuple")
    , ("contents", toJSON ps)
    ]
  toJSON PolymorphicRep = JObject
    [ ("tag", JString "PolymorphicRep")
    ]

ToJSON (BinderId r) where
  toJSON (MkBinderId u) = toJSON u

ToJSON BinderIdSg where
  toJSON (r ** b) = toJSON b

{r : RepType} -> ToJSON (SBinder r) where
  toJSON b = JObject
    [ ("sbinderName"    , toJSON (binderName b))
    , ("sbinderId"      , toJSON (binderId b))
    , ("sbinderType"    , toJSON (binderRep b))
    , ("sbinderTypeSig" , toJSON "") -- Information field in ExtSTG
    , ("sbinderScope"   , toJSON (binderScope b))
    , ("sbinderDetails" , toJSON (binderDetails b))
    , ("sbinderInfo"    , toJSON (binderInfo b))
    , ("sbinderDefLoc"  , toJSON (binderDefLoc b))
    ]

ToJSON SBinderSg where
  toJSON (_ ** b) = toJSON b

ToJSON DataConRep where
  toJSON (AlgDataCon p) = JObject
    [ ("tag", JString "AlgDataCon")
    , ("contents", toJSON p)
    ]
  toJSON (UnboxedTupleCon p) = JObject
    [ ("tag", JString "UnboxedTupleCon")
    , ("contents", toJSON p)
    ]

mutual
  ToJSON STyCon where
    toJSON s = JObject
      [ ("stcName"    , toJSON (Name s))
      , ("stcId"      , toJSON (Id s))
      , ("stcDataCons", toJSON (DataCons s))
      , ("stcDefLoc"  , toJSON (DefLoc s))
      ]

  {r : DataConRep} -> ToJSON (SDataCon r) where
    toJSON s = JObject
      [ ("sdcName" , toJSON (name s))
      , ("sdcId" , toJSON (ident s))
      , ("sdcRep" , toJSON (rep s))
      , ("sdcWorker" , toJSON (worker s))
      , ("sdcDefLoc" , toJSON (defLoc s))
      ]

  ToJSON SDataConSg where
    toJSON (r ** d) = toJSON d

ToJSON LitNumType where
  toJSON LitNumInt    = JString "LitNumInt"
  toJSON LitNumInt64  = JString "LitNumInt64"
  toJSON LitNumWord   = JString "LitNumWord"
  toJSON LitNumWord64 = JString "LitNumWord64"

ToJSON LabelSpec where
  toJSON (FunctionLabel i) = JObject
    [ ("tag", JString "FunctionLabel")
    , ("contents", toJSON i)
    ]
  toJSON DataLabel = JObject
    [ ("tag", JString "DataLabel")
    ]

ToJSON Lit where
  toJSON (LitChar c) = JObject
    [ ("tag", JString "LitChar")
    , ("contents", toJSON c)
    ]
  toJSON (LitString s) = JObject
    [ ("tag", JString "LitString")
    , ("contents", toJSON s)
    ]
  toJSON LitNullAddr = JObject
    [ ("tag", JString "LitNullAddr")
    ]
  toJSON (LitFloat d) = JObject
    [ ("tag", JString "LitFloat")
    , ("contents", toJSON d)
    ]
  toJSON (LitDouble d) = JObject
    [ ("tag", JString "LitDouble")
    , ("contents", toJSON d)
    ]
  toJSON (LitLabel s l) = JObject
    [ ("tag", JString "LitLabel")
    , ("contents", JArray [toJSON s, toJSON l])
    ]
  toJSON (LitNumber l i) = JObject
    [ ("tag", JString "LitNumber")
    , ("contents", JArray [toJSON l, toJSON i])
    ]

ToJSON (Arg r) where
  toJSON (StgVarArg i) = JObject
    [ ("tag"     , JString "StgVarArg")
    , ("contents", toJSON i)
    ]
  toJSON (StgLitArg l) = JObject
    [ ("tag", JString "StgLitArg")
    , ("contents", toJSON l)
    ]
  toJSON StgVoid = JObject
    [ ("tag", JString "StgVoid")
    ]

ToJSON ArgSg where
  toJSON (r ** x) = toJSON x

ToJSON SourceText where
  toJSON (MkSourceText s) = JObject
    [ ("tag", JString "MkSourceText")
    , ("contents", toJSON s)
    ]
  toJSON (NoSourceText) = JObject
    [ ("tag", JString "NoSourceText")
    ]

ToJSON CCallTarget where
  toJSON (StaticTarget t s u b) = JObject
    [ ("tag", JString "StaticTarget")
    , ("contents", JArray [toJSON t, toJSON s, toJSON u, toJSON b])
    ]
  toJSON DynamicTarget = JObject
    [ ("tag", JString "DynamicTarget")
    ]

ToJSON CCallConv where
  toJSON MkCCallConv        = JString "MkCCallConv"
  toJSON CApiConv           = JString "CApiConv"
  toJSON StdCallConv        = JString "StdCallConv"
  toJSON PrimCallConv       = JString "PrimCallConv"
  toJSON JavaScriptCallConv = JString "JavaScriptCallConv"

ToJSON Safety where
  toJSON PlaySafe           = JString "PlaySafe"
  toJSON PlayInterruptible  = JString "PlayInterruptible"
  toJSON PlayRisky          = JString "PlayRisky"

ToJSON ForeignCall where
  toJSON f = JObject
    [ ("foreignCTarget", toJSON (CTarget f))
    , ("foreignCConv"  , toJSON (CConv f))
    , ("foreignCSafety", toJSON (CSafety f))
    ]

ToJSON PrimCall where
  toJSON (MkPrimCall s u) = JArray [toJSON s, toJSON u]

{n : String} -> ToJSON (PrimOp n as ret) where
  toJSON p = JObject
    [ ("tag", JString "StgPrimOp")
    , ("contents", toJSON $ PrimOp.name p)
    ]

ToJSON StgOp where
  toJSON (StgPrimOp p) = JObject
    [ ("tag", JString "StgPrimOp")
    , ("contents", toJSON p)
    ]
  toJSON (StgPrimCallOp p) = JObject
    [ ("tag", JString "StgPrimCallOp")
    , ("contents", toJSON p)
    ]
  toJSON (StgFCallOp p) = JObject
    [ ("tag", JString "StgFCallOp")
    , ("contents", toJSON p)
    ]

ToJSON AltType where
  toJSON (PolyAlt) = JObject
    [ ("tag", JString "PolyAlt")
    ]
  toJSON (MultiValAlt i) = JObject
    [ ("tag", JString "MultiValAlt")
    , ("contents", toJSON i)
    ]
  toJSON (PrimAlt p) = JObject
    [ ("tag", JString "PrimAlt")
    , ("contents", toJSON p)
    ]
  toJSON (AlgAlt t) = JObject
    [ ("tag", JString "AlgAlt")
    , ("contents", toJSON t)
    ]

ToJSON (AltCon r) where
  toJSON (AltDataCon d) = JObject
    [ ("tag", JString "AltDataCon")
    , ("contents", toJSON d)
    ]
  toJSON (AltLit l) = JObject
    [ ("tag", JString "AltLit")
    , ("contents", toJSON l)
    ]
  toJSON AltDefault = JObject
    [ ("tag", JString "AltDefault")
    ]
  toJSON (AltUnboxedOneTuple d) = JObject
    [ ("tag", JString "AltDataCon")
    , ("contents", toJSON d)
    ]

toBinderList : {ps : List PrimRep} -> BList ps -> List SBinderSg
toBinderList []        = []
toBinderList (x :: xs) = mkSBinderSg x :: toBinderList xs

toConArgList : {ps : List PrimRep} -> ArgList ps -> List ArgSg
toConArgList [] = []
toConArgList (x :: xs) = mkArgSg x :: toConArgList xs

mutual

  ToJSON (Expr r) where
    toJSON (StgApp f a r) = JObject
      [ ("tag", JString "StgApp")
      , ("contents", JArray [toJSON f, toJSON a, toJSON r, JArray [toJSON "",toJSON "",toJSON ""]])
        -- ^^ Haskell implementation of the ExtSTG constains an extra field
        -- of type (String, String, String) for debugging, we don't have it here
        -- but when we generate the JSON it is needed on the Haskell side.
      ]
    toJSON (StgLit l) = JObject
      [ ("tag", JString "StgLit")
      , ("contents", toJSON l)
      ]
    toJSON (StgConApp {r=AlgDataCon []} d s) = JObject
      [ ("tag", JString "StgConApp")
      , ("contents", JArray [toJSON d, JArray [], JArray []])
        -- No (List RepType) is given, as we don't support UnboxedTuples for now.
      ]
    toJSON (StgConApp {r=AlgDataCon [p]} d s) = JObject
      [ ("tag", JString "StgConApp")
      , ("contents", JArray [toJSON d, JArray [toJSON s], JArray []])
        -- No (List RepType) is given, as we don't support UnboxedTuples for now.
      ]
    toJSON (StgConApp {r=AlgDataCon (p0 :: p1 :: ps)} d s) = JObject
      [ ("tag", JString "StgConApp")
      , ("contents", JArray [toJSON d, toJSON (toConArgList s), JArray []])
        -- No (List RepType) is given, as we don't support UnboxedTuples for now.
      ]
    toJSON (StgConApp {r=UnboxedTupleCon n} d s) impossible
    toJSON (StgOpApp {args=[]} {ret} p s) = JObject
      [ ("tag", JString "StgOpApp")
      , ("contents", JArray
            [ toJSON p
            , JArray []
            , toJSON (SingleValue ret)
            , toJSON (the (Maybe TyConId) Nothing)
            ])
      ]
    toJSON (StgOpApp {args=[a1]} {ret} p s) = JObject
      [ ("tag", JString "StgOpApp")
      , ("contents", JArray
            [ toJSON p
            , JArray [toJSON s]
            , toJSON (SingleValue ret)
            , toJSON (the (Maybe TyConId) Nothing)
            ])
      ]
    toJSON (StgOpApp {args=(a1::a2::as)} {ret} p s) = JObject
      [ ("tag", JString "StgOpApp")
      , ("contents", JArray
            [ toJSON p
            , toJSON (toConArgList s)
            , toJSON (SingleValue ret)
            , toJSON (the (Maybe TyConId) Nothing)
            ])
      ]
    toJSON (StgCase d s i a) = JObject
      [ ("tag", JString "StgCase")
      , ("contents", JArray [toJSON s, toJSON i, toJSON d, toJSON a])
        -- The order of the contents is important for the ExtSTG compatibility
      ]
    toJSON (StgLet b e) = JObject
      [ ("tag", JString "StgLet")
      , ("contents", JArray [toJSON b, toJSON e])
      ]
    toJSON (StgLetNoEscape b e) = JObject
      [ ("tag", JString "StgLetNoEscape")
      , ("contents", JArray [toJSON b, toJSON e])
      ]

  ToJSON (Alt r q) where
    toJSON (MkAlt AltDefault () body) = JObject
      [ ("altCon"    , toJSON (the (AltCon r) AltDefault))
      , ("altBinders", JArray [])
      , ("altRHS"    , toJSON body)
      ]
    toJSON (MkAlt (AltLit l) () body) = JObject
      [ ("altCon"    , toJSON (AltLit l))
      , ("altBinders", JArray [])
      , ("altRHS"    , toJSON body)
      ]
    toJSON (MkAlt alt@(AltDataCon ((AlgDataCon []) ** _)) binders body) = JObject
      [ ("altCon"    , toJSON alt)
      , ("altBinders", JArray [])
      , ("altRHS"    , toJSON body)
      ]
    toJSON (MkAlt alt@(AltDataCon ((AlgDataCon [p]) ** _)) binder body) = JObject
      [ ("altCon"    , toJSON alt)
      , ("altBinders", JArray [toJSON binder])
      , ("altRHS"    , toJSON body)
      ]
    toJSON (MkAlt alt@(AltDataCon ((AlgDataCon (p0 :: p1 :: ps)) ** dc)) binders body) = JObject
      [ ("altCon"    , toJSON alt)
      , ("altBinders", toJSON $ toBinderList binders)
      , ("altRHS"    , toJSON body)
      ]
    toJSON (MkAlt alt@(AltUnboxedOneTuple d) binder body) = JObject
      [ ("altCon"    , toJSON alt)
      , ("altBinders", JArray [toJSON binder])
      , ("altRHS"    , toJSON body)
      ]

  ToJSON Rhs where
    toJSON (StgRhsClosure u a b) = JObject
      [ ("tag", JString "StgRhsClosure")
      , ("contents", JArray [JArray [], toJSON u, toJSON a, toJSON b])
      ]
    toJSON (StgRhsCon d a) = JObject
      [ ("tag", JString "StgRhsCon")
      , ("contents", JArray [toJSON d, toJSON a])
      ]

  ToJSON Binding where
    toJSON (StgNonRec i r) = JObject
      [ ("tag", JString "StgNonRec")
      , ("contents", JArray [toJSON i, toJSON r])
      ]
    toJSON (StgRec bs) = JObject
      [ ("tag", JString "StgRec")
      , ("contents", toJSON bs)
      ]

  export
  ToJSON TopBinding where
    toJSON (StgTopLifted b) = JObject
      [ ("tag"     , JString "StgTopLifted")
      , ("contents", toJSON b)
      ]
    toJSON (StgTopStringLit i l) = JObject
      [ ("tag", JString "StgTopStringLit")
      , ("contents", JArray [toJSON i, toJSON l])
      ]

export
ToJSON Module where
  toJSON m = JObject
    [ ("modulePhase"               , toJSON (Phase m))
    , ("moduleUnitId"              , toJSON (ModuleUnitId m))
    , ("moduleName"                , toJSON (Name m))
    , ("moduleSourceFilePath"      , toJSON (SourceFilePath m))
    , ("moduleForeignStubs"        , toJSON (ForeignStubs m))
    , ("moduleHasForeignExported"  , toJSON (HasForeignExported m))
    , ("moduleDependency"          , toJSON (Dependency m))
    , ("moduleExternalTopIds"      , toJSON (ExternalTopIds m))
    , ("moduleTyCons"              , toJSON (TyCons m))
    , ("moduleTopBindings"         , toJSON (TopBindings m))
    , ("moduleForeignFiles"        , toJSON (ForeignFiles m))
    ]
