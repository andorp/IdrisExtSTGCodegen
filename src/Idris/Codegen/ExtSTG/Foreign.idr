module Idris.Codegen.ExtSTG.Foreign

import Data.List1
import Data.String
import Core.Name
import Core.Context
import Data.List
import Core.TT
import Core.CompileExpr
import System.File
import Data.List.Views

import Idris.Codegen.ExtSTG.Core
import Idris.Codegen.ExtSTG.Prelude
import Idris.Codegen.ExtSTG.STG
import Idris.Codegen.ExtSTG.ExternalTopIds
import Idris.Codegen.ExtSTG.Context
import Idris.Codegen.ExtSTG.Configuration

import Idris.Codegen.ExtSTG.String

%default total

{-
For foreign implementation we use simple Haskell notation, but the foreign string starts with: 'stg:'
-}

{-
-- Argument type descriptors for foreign function calls
public export
data CFType : Type where
     CFUnit : CFType :: ()
     -- Unit type in haskell is represented ()
     CFInt : CFType :: Int
     -- Simple boxed integer
     CFUnsigned8 : CFType :: Word8
     -- Simple boxed word8
     CFUnsigned16 : CFType :: Word16
     -- simple boxed word16
     CFUnsigned32 : CFType :: Word32
     -- simple boxed word32
     CFUnsigned64 : CFType :: Word64
     -- simple boxed word64
     CFString : CFType -- magic
     CFDouble : CFType :: Double
     -- simple boxed double
     CFChar : CFType :: Char
     -- simple boxed char
     CFPtr : CFType -- not supported yet
     CFGCPtr : CFType -- not supported yet
     CFBuffer : CFType -- ???
     CFWorld : CFType -- ()
     -- we don't propagate information between the two worlds
     CFFun : CFType -> CFType -> CFType -- not supported yet
     CFIORes : CFType -> CFType :: IO
     -- IO computation
     CFStruct : String -> List (String, CFType) -> CFType -- not supported yet
     CFUser : Name -> List CFType -> CFType -- plain supported
     -- for the first iteration we need to support simple, non parametric types.
     -- This is mainly for user defined ADTs

     -- An interesting question is how to represent type classes from the Haskell world.
-}

{-
Question: How to represent CFTypes?
I think these should be Boxed datatypes in Haskell.

The interesting ones are:
CFString : CFType -- Simple string, can't be part of the Struct.
CFPtr : CFType -- Pointer to a complex type AnyPtr is compiled to this one.
CFGCPtr : CFType
  -- Produced by the onCollect : Ptr t -> (Ptr t -> IO ()) -> IO (GCPtr t)
  -- it creates a pointer which has a finaliser assoicated with it.
CFBuffer : CFType
  -- From Data.Buffer A Byte buffer of size N
CFFun : CFType -> CFType -> CFType
  -- Probably lambda function, coming from NBind with something Closure related.
CFIORes : CFType -> CFType
  -- Represents PrimIO operations such as:
  -- %foreign "C:idris2_getStr,libidris2_support"
              "node:support:getStr,support_system_file"
     prim__getStr : PrimIO String
  -- will lead to: IORes String
  -- ffiTopBinding: (Prelude.IO.prim__putStr, ([String, %World], IORes Unit))
  -- ffiTopBinding: (Prelude.IO.prim__getStr, ([%World], IORes String))
  -- pure functions does not include the %World type:
  -- ffiTopBinding: (Data.Strings.fastUnpack, ([String], Prelude.Types.List Char))
  -- In STG there is no difference in IO and pure. primitive functions may operate on
  -- WorldState, this needs more investigation.
CFStruct : String -> List (String, CFType) -> CFType
  -- Comes from System.FFI
CFUser : Name -> List CFType -> CFType
  -- User defined type.
  -- Some types needs to be mapped too. Like Prelude.Types.List
  -- Nat, Pair, Maybe, Dec, Either, List, Stream, (Cast?)

TODO: Find all [external] types, and report them, the foreign implementations
should be the reponsible for creating such parts.
TODO: What is the difference between %foreign and %extern
-}

{-
Foreign types will be represented as Boxed types, and there is an association, between CFTypes and
Simple types. the FFI is responsible to create the bridge between the STG side and the Idris side.
This is simple now, as we use the GHC's representation of the values, such as GHC.Word8
-}

record Foreign where
  constructor MkForeign
  name     : Name.Name
  fargs    : List CFType
  ret      : CFType
  topLevel : TopBinding

||| Matches and removes the "stg:" prefix
stgForeign : String -> Maybe String
stgForeign s =
  let xs = unpack s
  in if isPrefixOf "stg:" s
      then Just $ pack $ drop 4 xs
      else Nothing

data ForeignOp
  = ForeignExtName ExtName
  | ForeignPrimOp STG.Name

parsePrimOp : String -> Maybe STG.Name
parsePrimOp str = case unpack str of
  ('#' :: xs) => Just $ pack xs
  _           => Nothing

parseForeignStr : String -> Maybe ForeignOp
parseForeignStr str =
  (ForeignExtName <$> parseName str) <|>
  (ForeignPrimOp <$> parsePrimOp str)

argSgFromBinderSg : {r : RepType} -> SBinder r -> ArgSg
argSgFromBinderSg {r} b = (r ** (StgVarArg (binderId b)))

{-
Lookup if the give String is a primitive operation: no dots and ends in #
For primitives, unwrap the values, call the primitive, wrap the result
For non-primitives, every data like thing is considered as Lifted, just call the function
TODO: We need to handle the GHC calling convention right.
-}

toBinders
  :  Ref STGCtxt STGContext
  => Name.Name
  -> (cf : List (Nat,CFType))
  -> Core (BinderList (replicate (length cf) LiftedRep))
toBinders nm [] = pure []
toBinders nm ((k , x) :: xs) = do
  bdr <- mkSBinderLocal emptyFC nm (cast k)
  map (bdr ::) (toBinders nm xs)

toSBinderSgList : {rs : List PrimRep} -> BinderList rs -> List SBinderSg
toSBinderSgList [] = []
toSBinderSgList (x :: y) = mkSBinderSg x :: toSBinderSgList y

toArgSgList : {rs : List PrimRep} -> ArgList rs -> List ArgSg
toArgSgList [] = []
toArgSgList (x :: xs) = mkArgSg x :: toArgSgList xs

{-
System.IO.putStr : String -> IO ()
GHC.CString.unpackCString# : Addr# -> String
-}

getExtName
  :  Ref STGCtxt STGContext
  => ExtName -> Core (BinderId (SingleValue LiftedRep))
getExtName ename = do
  ((SingleValue LiftedRep) ** something) <- extName ename
    | _ => coreFail $ InternalError "....."
  pure something

{-
1. Create binders for all the parameters, there are special ones that needs transformation, for
   those we need to return a binder and an expression which bind the new value
2. For the return value we also need to do some kind of mapping.
   If the return value is non IO simple wrapping is enough, if we have IO we need to
   apply an extra void value to trigger the evaluation in GHCRuntime, the return
   value needs to be matched against an unboxed tuple.

Examples

1) Pure function
%foreign "stg: unit_Module.s.f"
haskellF : Int -> Int

This is a simple case, no need for wrapping values neither on the input, neither on the
output side. Its translation is like:

TopBinding haskellF [intBinder] = StgApp (mkExtName "unit" ["Module", "s"] "f") [intBinder]

2) IO () function
%foreign "stg: base_System.IO.putChar"
haskellF : Char -> IO ()

This is a simple case on the input side, but a complex on the output side as we need to use
an IO function, from the haskell library.

TopBinding haskellF [charBinder, worldBinder]
  = VoidBinder $ \voidBinder ->
    Seq  (StgApp (mkExtName "base" ["System", "IO"] "putChar") [charBinder, voidBinder]) $
    Bind (StgConApp mkUnitDataConId []) $ \unitBinder ->
    Done (StgConApp mkIOResDataConId [unitBinder, worldBinder])

3) IO with conversion
%foreign "stg: unit_Module.s.conversion"
haskellF : Double -> IO Int

This is similar to previous case, but it needs to return the right Integer coming from
the Haskell world.

TopBinding haskellF [doubleBinder, worldBinder]
  = VoidBinder $ \voidBinder ->
    Bind (StgApp (mkE "unit" ["Module", "s"] "conversion") [doubleBinder, voidBinder] $ \intBdrTpl ->
    Unwrap intBdrTpl $ \intBdr ->
    Done (StgConApp mkIOResDataConId [intBdr, worldBinder])

4) String function
%foreign "stg: unit_Module.s.func"
haskellF : String -> Int

Idris has different String representation than Haskell, the example below shows, first calling
the haskell function we need to convert the Idris String. When we get the Int that is
compatible with the Idris represented Int.

5) A function that returns a String
%foreign "stg: unit_Module.s.func"
haskellF : Int -> String

We need to write a function that traverses a Haskell list and calculates its length, and
creates an array, and copies the characters from the list.
Check one STG module where the list is used in some function, something from the base.

6) Handling [external] datatypes

-}

-- traverse the parameters, and create STG binders for them, build an STG -> STG transformation for every parameter
-- Our goal is to build a TopBinding in STG.

namespace IdrisRepresentation

  public export
  data IdrUnboxRep : CFType -> RepType -> Type where
    RepString : IdrUnboxRep CFString (SingleValue AddrRep)
    RepWorld  : IdrUnboxRep CFWorld  (SingleValue VoidRep)

namespace GHCRepresentation

  public export
  data GHCRep : CFType -> RepType -> Type where
    RepString : GHCRep CFString (SingleValue LiftedRep)
    RepWorld  : GHCRep CFWorld  (SingleValue VoidRep)

  export
  mkGHCRep : CFType -> RepType

record STGBinderHead (c : CFType) where
  constructor STGBinder
  {idrRep, ghcRep : RepType}
  {auto 0 idrRepPrf : IdrUnboxRep c idrRep}
  {auto 0 ghcRepPrf : GHCRep c ghcRep}
  idrisSideParam    : SBinder Core.stgRepType
  unwrapped         : SBinder idrRep
  ghcSideParam      : SBinder ghcRep
  stgCase           : Expr Core.stgRepType -> Expr Core.stgRepType

data STGBinderList : List CFType -> Type where
  Nil  : STGBinderList []
  (::) : (STGBinderHead c) -> STGBinderList cs -> STGBinderList (c::cs)

lastSTGBinder : {0 xs : List CFType} -> {auto 0 ok : NonEmpty xs} -> STGBinderList xs -> STGBinderHead (last xs)
lastSTGBinder {xs = (x :: [])}        {ok = IsNonEmpty} (y :: z) = y
lastSTGBinder {xs = (x :: (w :: xs))} {ok = IsNonEmpty} (y :: z) = lastSTGBinder z

createSTGBinderList : Ref STGCtxt STGContext => Name.Name -> (fs : List CFType) -> Core (STGBinderList fs)
createSTGBinderList _ [] = pure []
createSTGBinderList nm (CFUnit :: xs) = coreFail $ InternalError "argCFType; missing case "
createSTGBinderList nm (CFInt :: xs) = coreFail $ InternalError "argCFType; missing case "
createSTGBinderList nm (CFInteger :: xs) = coreFail $ InternalError "argCFType; missing case "
createSTGBinderList nm (CFInt8 :: xs) = coreFail $ InternalError "argCFType; missing case "
createSTGBinderList nm (CFInt16 :: xs) = coreFail $ InternalError "argCFType; missing case "
createSTGBinderList nm (CFInt32 :: xs) = coreFail $ InternalError "argCFType; missing case "
createSTGBinderList nm (CFInt64 :: xs) = coreFail $ InternalError "argCFType; missing case "
createSTGBinderList nm (CFUnsigned8 :: xs) = coreFail $ InternalError "argCFType; missing case "
createSTGBinderList nm (CFUnsigned16 :: xs) = coreFail $ InternalError "argCFType; missing case "
createSTGBinderList nm (CFUnsigned32 :: xs) = coreFail $ InternalError "argCFType; missing case "
createSTGBinderList nm (CFUnsigned64 :: xs) = coreFail $ InternalError "argCFType; missing case "
createSTGBinderList nm (CFString :: xs) = do
  -- CString: Binary literal:
  -- https://hackage.haskell.org/package/ghc-prim-0.6.1/docs/GHC-CString.html
  -- IO in STG is not a concept. We need to apply a Void there. We need to use Void primitive.
  -- It needs a special argument: Argument => BuiltIn Void
  ((SingleValue LiftedRep) ** ghcCStringUnpackString)
    <- extName $ MkExtName "ghc-prim" ["GHC","CString"] "unpackCString#"
        | _ => coreFail $ InternalError "....."
  param <- mkSBinderLocal emptyFC nm !(incCounter)
  local <- mkSBinderRepLocal (SingleValue AddrRep) emptyFC nm !(incCounter)
  stringBinder <- mkSBinderLocal emptyFC nm !(incCounter)
  tail <- createSTGBinderList nm xs
  addrFromStringFun <- addrFromStringBinderId
  let stgCase
        : Expr stgRepType -> Expr stgRepType
        := \body =>
            StgCase
              (PrimAlt AddrRep)
              (StgApp
                addrFromStringFun
                [mkArgSg (StgVarArg (binderId param))]
                (SingleValue AddrRep))
              local
              [ MkAlt AltDefault ()
              $ StgCase
                  PolyAlt
                  (StgApp
                    ghcCStringUnpackString
                    [mkArgSg (StgVarArg (binderId local))]
                    (SingleValue LiftedRep))
                  stringBinder
                  [ MkAlt AltDefault () body ]
              ]
  -- TODO: local or stringBinder?
  pure (STGBinder param local stringBinder stgCase :: tail)
createSTGBinderList nm (CFDouble :: xs) = coreFail $ InternalError "argCFType; missing case "
createSTGBinderList nm (CFChar :: xs) = coreFail $ InternalError "argCFType; missing case "
createSTGBinderList nm (CFPtr :: xs) = coreFail $ InternalError "argCFType; missing case "
createSTGBinderList nm (CFGCPtr :: xs) = coreFail $ InternalError "argCFType; missing case "
createSTGBinderList nm (CFBuffer :: xs) = coreFail $ InternalError "argCFType; missing case "
createSTGBinderList nm (CFForeignObj :: xs) = coreFail $ InternalError "argCFType; missing case "
createSTGBinderList nm (CFWorld :: xs) = do
  -- We pattern match on the Void but we don't use its value. Rather we use
  -- the built-in voidRep
  param <- mkSBinderLocal emptyFC nm !(incCounter)
  nonused <- mkSBinderRepLocal (SingleValue VoidRep) emptyFC nm !(incCounter)
  voidBinder <- mkSBinderHardcoded hardcodedVoidHash emptyFC
  tail <- createSTGBinderList nm xs
  let stgCase
        : Expr stgRepType -> Expr stgRepType
        := \body =>
            StgCase
              (PrimAlt VoidRep)            
              (StgApp (binderId param) [] (SingleValue VoidRep))
            nonused
            [ MkAlt AltDefault () body ]
  pure (STGBinder param nonused voidBinder stgCase :: tail)
createSTGBinderList nm ((CFFun x y) :: xs) = coreFail $ InternalError "argCFType; missing case "
createSTGBinderList nm ((CFIORes x) :: xs) = coreFail $ InternalError "argCFType; missing case "
createSTGBinderList nm ((CFStruct x ys) :: xs) = coreFail $ InternalError "argCFType; missing case "
createSTGBinderList nm ((CFUser x ys) :: xs) = coreFail $ InternalError "argCFType; missing case "

bindersListToVarArgs : (binders : STGBinderList fs) -> List ArgSg
bindersListToVarArgs [] = []
bindersListToVarArgs (x :: xs) = mkArgSg (StgVarArg (binderId x.ghcSideParam)) :: bindersListToVarArgs xs

binderListToFunArg : (binders : STGBinderList fs) -> List SBinderSg
binderListToFunArg [] = []
binderListToFunArg (x :: xs) = mkSBinderSg x.idrisSideParam :: binderListToFunArg xs

buildSTGFromBinders : (binders : STGBinderList fs) -> Expr Core.stgRepType -> Expr Core.stgRepType
buildSTGFromBinders []        e = e
buildSTGFromBinders (x :: xs) e = x.stgCase $ buildSTGFromBinders xs e

data SimpleCFType : CFType -> Type where
  SimpleCFUnit        : SimpleCFType CFUnit
  SimpleCFInt         : SimpleCFType CFInt
  SimpleCFInteger     : SimpleCFType CFInteger
  SimpleCFInt8        : SimpleCFType CFInt8
  SimpleCFInt16       : SimpleCFType CFInt16
  SimpleCFInt32       : SimpleCFType CFInt32
  SimpleCFInt64       : SimpleCFType CFInt64
  SimpleCFUnsigned8   : SimpleCFType CFUnsigned8
  SimpleCFUnsigned16  : SimpleCFType CFUnsigned16
  SimpleCFUnsigned32  : SimpleCFType CFUnsigned32
  SimpleCFUnsigned64  : SimpleCFType CFUnsigned64
  SimpleCFString      : SimpleCFType CFString
  SimpleCFDouble      : SimpleCFType CFDouble
  SimpleCFChar        : SimpleCFType CFChar

mkSimpleCFType : (t : CFType) -> Maybe (SimpleCFType t)
mkSimpleCFType CFUnit       = Just SimpleCFUnit
mkSimpleCFType CFInt        = Just SimpleCFInt
mkSimpleCFType CFInteger    = Just SimpleCFInteger
mkSimpleCFType CFInt8       = Just SimpleCFInt8
mkSimpleCFType CFInt16      = Just SimpleCFInt16
mkSimpleCFType CFInt32      = Just SimpleCFInt32
mkSimpleCFType CFInt64      = Just SimpleCFInt64
mkSimpleCFType CFUnsigned8  = Just SimpleCFUnsigned8
mkSimpleCFType CFUnsigned16 = Just SimpleCFUnsigned16
mkSimpleCFType CFUnsigned32 = Just SimpleCFUnsigned32
mkSimpleCFType CFUnsigned64 = Just SimpleCFUnsigned64
mkSimpleCFType CFString     = Just SimpleCFString
mkSimpleCFType CFDouble     = Just SimpleCFDouble
mkSimpleCFType CFChar       = Just SimpleCFChar
mkSimpleCFType other        = Nothing


data IORepr : CFType -> Type where
  IOUnit : IORepr CFUnit

data Repr : CFType -> Type where
  IOResRepr : (IORepr t) -> Repr (CFIORes t)
  StringRepr : Repr CFString

mkIORepr : (cf : CFType) -> Maybe (IORepr cf)
mkIORepr CFUnit = Just IOUnit
mkIORepr other  = Nothing

mkRepr : (cf : CFType) -> Maybe (Repr cf)
mkRepr (CFIORes t) = map IOResRepr (mkIORepr t)
mkRepr CFString    = Just StringRepr
mkRepr other       = Nothing

data ValidSignature : List CFType -> CFType -> Type where
  ValidSignatureIORet : (args : List CFType) -> (t : CFType) -> (0 repr : IORepr t)
    -> ValidSignature (args ++ [CFWorld]) (CFIORes t)
  ValidSignatureRet : (args : List CFType) -> (t : CFType) -> (0 repr : SimpleCFType t)
    -> ValidSignature args t

mkValidSignatureIORet : (args : List CFType) -> {t : CFType} -> (IORepr t) -> Maybe (ValidSignature args (CFIORes t))
mkValidSignatureIORet [] r = Nothing
mkValidSignatureIORet (CFWorld :: [])   r = Just $ ValidSignatureIORet [] t r
mkValidSignatureIORet (x :: [])         r = Nothing
mkValidSignatureIORet (y :: (x :: xs))  r = map (appendArg y) (mkValidSignatureIORet (x :: xs) r)
  where
    appendArg : (a : CFType) -> ValidSignature as ret -> ValidSignature (a :: as) ret
    appendArg a (ValidSignatureIORet args t x) = ValidSignatureIORet (a :: args) t x
    appendArg a (ValidSignatureRet   args t x) = ValidSignatureRet   (a :: args) t x

mkValidSignatureRet : (args : List CFType) -> {t : CFType} -> SimpleCFType t -> ValidSignature args t
mkValidSignatureRet args x = ValidSignatureRet args t x

mkValidSignature : (args : List CFType) -> (ret : CFType) -> Maybe (ValidSignature args ret)
mkValidSignature args (CFIORes t) = mkValidSignatureIORet args     !(mkIORepr t)
mkValidSignature args ret         = map (mkValidSignatureRet args) (mkSimpleCFType ret)

consInNonEmtpty : {1 xs : List a} -> {0 x : a} -> NonEmpty (xs ++ [x])
consInNonEmtpty {xs = []}         = IsNonEmpty
consInNonEmtpty {xs = (y :: xs)}  = IsNonEmpty

data IORetType : CFType -> Type where
  IORetUnit : IORetType CFUnit

mkIORetType : (ret : CFType) -> Maybe (IORetType ret)
mkIORetType CFUnit = Just IORetUnit
mkIORetType other  = Nothing

returnTyConId
  :  Ref STGCtxt STGContext
  => IORetType ret
  -> Core TyConId
returnTyConId IORetUnit = do
  Just typeConUnique <- lookupTypeNamespace "Builtin.Unit"
    | Nothing => coreFail $ InternalError "returnTyConId: Builtin.Unit type is not registered."
  pure (MkTyConId typeConUnique)

returnDataConId
  :  Ref STGCtxt STGContext
  => IORetType ret
  -> Core DataConIdSg
returnDataConId IORetUnit = do
  Just dataConUnique <- lookupTermNamespace "Builtin.MkUnit"
    | Nothing => coreFail $ InternalError "returnDataConId: Builtin.MkUnit constructor is not registered."
  case !(getDataCons dataConUnique) of
    Nothing   => coreFail $ InternalError "returnDataConId: Couldn't find Binder for Builtin.MkUnit."
    Just []   => coreFail $ InternalError "returnDataConId: Couldn't find Binder for Builtin.MkUnit. Empty list, this should not have happened."
    Just [d]  => pure (identSg d)
    Just ds   => coreFail $ InternalError "returnDataConId: Found more than one Binders for Builtin.MkUnit."

createReturnValue
  :  Ref STGCtxt STGContext
  => BinderId Core.stgRepType -> IORetType ret
  -> Core (Expr Core.stgRepType)
createReturnValue ffiCallResultArg IORetUnit = do
  Just dataConUnique <- lookupTermNamespace "Builtin.MkUnit"
    | Nothing => coreFail $ InternalError "createReturnValue: Builtin.MkUnit constructor is not registered."
  ((AlgDataCon []) ** dataConId) <- getUniqueDataCon dataConUnique
    | _ => coreFail $ InternalError "createReturnValue: Builtin.MkUnit had different signature than expected."
  pure (StgConApp (ident dataConId) ())

ioResRet
  :  Ref STGCtxt STGContext
  => Name.Name -> BinderId Core.stgRepType -> BinderId Core.stgRepType -> CFType
  -> Core (Expr Core.stgRepType)
ioResRet nameCtx ffiCallResultArg worldArg ret = do
  -- For wrapping the result
  let Just ioRetType = mkIORetType ret
    | _ => coreFail $ InternalError "ioResRet: is not a supported return type."
  ((AlgDataCon [LiftedRep, LiftedRep]) ** mkIOResDataConId) <- mkDataConIdStr "PrimIO.MkIORes"
    | _ => coreFail $ InternalError "Missing PrimIO.MkIORes data constructor."
  tyConId <- returnTyConId ioRetType
  stgConApp <- createReturnValue ffiCallResultArg ioRetType
  mkDataConResultBinder <- mkSBinderLocal emptyFC nameCtx !(incCounter)
  let retSTGExpr
        : Expr Core.stgRepType
        := StgCase
            (AlgAlt tyConId)
            stgConApp
            mkDataConResultBinder
            [ MkAlt AltDefault ()
            $ StgConApp mkIOResDataConId
                [ StgVarArg (binderId mkDataConResultBinder)
                , StgVarArg worldArg
                ]
            ]
  pure retSTGExpr

createFFICallSTG
  :  Ref STGCtxt STGContext
  => Name.Name -> ExtName -> (ValidSignature args ret) -> (binders : STGBinderList args)
  -> Core (Expr Core.stgRepType)
createFFICallSTG nameCtx extFFIName (ValidSignatureIORet xs ret@CFUnit IOUnit) binders = do

  let worldArgBinder = lastSTGBinder binders {ok = consInNonEmtpty}
  let (SingleValue LiftedRep **  worldArgGHCBinder) = mkBinderIdSg (binderId worldArgBinder.ghcSideParam)
      | (r ** _) => coreFail $ InternalError "WorldArg has \{show r} instead of LiftedRep"

  ((SingleValue LiftedRep) ** ffiFunctionBinder) <- extName extFFIName
    | _ => coreFail $ InternalError "..."
  ffiResultBinder <- mkSBinderLocal emptyFC nameCtx !(incCounter)
  retSTGExpr <- ioResRet nameCtx (binderId ffiResultBinder) worldArgGHCBinder ret

  let ffiArgs
        : List (r : RepType ** Arg r)
        := bindersListToVarArgs binders
  let callAndResult
        : Expr Core.stgRepType
        := StgCase
            PolyAlt
            (StgApp ffiFunctionBinder ffiArgs (SingleValue LiftedRep))
            ffiResultBinder
            [ MkAlt AltDefault () retSTGExpr ]
  pure callAndResult
createFFICallSTG nameCtx extFFIName (ValidSignatureRet args ret repr) binders = do
  ?h1_2 -- do

createFFITopLifted
  :  Ref STGCtxt STGContext
  => Name.Name -> ExtName -> {args : List CFType}
  -> (validSignature : ValidSignature args ret)
  -> (binders : STGBinderList args)
  -> Core TopBinding
createFFITopLifted funName extName (ValidSignatureIORet xs t z) binders = do
  let worldArgBinder = lastSTGBinder binders {ok = consInNonEmtpty}
  result <- createFFICallSTG funName extName (ValidSignatureIORet xs t z) binders
  funNameBinder <- mkSBinderName emptyFC funName 
  let top : TopBinding
        := StgTopLifted
         $ StgNonRec funNameBinder
         $ StgRhsClosure ReEntrant (binderListToFunArg binders)
         $ buildSTGFromBinders binders result
  pure top
createFFITopLifted funName extName (ValidSignatureRet args ret z) binders = do
  coreFail $ InternalError "createFFITopLifted is not implemented yet."

-- ||| Convert the given String to STG, if it doesn't parse it raises an InternalError
ffiTopBinding
  :  Ref STGCtxt STGContext
  => Name.Name -> (args : List CFType) -> (ret : CFType) -> String
  -> Core TopBinding
ffiTopBinding nm args ret ffiString = do
  let Just (ForeignExtName external) = parseForeignStr ffiString
    | Just (ForeignPrimOp po) => coreFail $ InternalError "Foreign primop has found \{show po} instead of external name."
    | Nothing => coreFail $ InternalError $ "FFI name parsing has failed for \{ffiString}"
  stgBinders <- createSTGBinderList nm args
  let Just validSignature = mkValidSignature args ret
      | Nothing => coreFail $ InternalError "BLAH!"
  createFFITopLifted nm external validSignature stgBinders

||| Search in the content of the file for 'name = stg' pattern
findForeign : String -> String -> Core String
findForeign name content = do
  let [stg] = mapMaybe
                (\ln => case break (=='=') ln of
                          ("", rest) => Nothing
                          (x, rest)  => if (trim x == trim name)
                                          then Just $ trim $ pack $ drop 1 $ unpack rest
                                          else Nothing)
            $ toList $ lines content
      | other => coreFail $ InternalError $ unwords
                  [ "Found none or more than one foreign for"
                  , name
                  , ":"
                  , show other
                  ]
  pure stg

||| Use the fully qualified name to create a path in the foreign, if the Foreign is not
||| found an InternalError is raised, if the foreign can not be parsed an InternalError is raised.
partial
findForeignInFile
  :  Ref Ctxt Defs
  => Ref STGCtxt STGContext
  => Name.Name -> List CFType -> CFType
  -> Core TopBinding
findForeignInFile nm fargs ret = do
  -- TODO: Make this more efficient with bulk loading of data.
  fn <- toFullNames nm
  -- logLine $ "Foreign name: " ++ show fn
  case fn of
    (NS ns (UN n)) => do
      foreignDir <- map (.foreignDirectory) getConfiguration
      let path = concat $ intersperse "/" $ (foreignDir ::) $ toList $ split (=='.') $ show ns
      let filePath = path ++ ".stgffi"
      -- logLine $ "looking up file: " ++ filePath
      Right content <- coreLift $ readFile filePath
        | Left err => coreFail $ InternalError $ unwords
                        [ "Trying to resolve foreign definition"
                        , show fn
                        , "and got file error searching in foreign files:"
                        , show err
                        ]
      expr <- findForeign n content
      ffiTopBinding fn fargs ret expr
    other => coreFail $ InternalError $ "Name not in namespace format: \{show other}"

||| Tries to find the definition in the given ccs parameter, if there is no Haskell definition
||| there, it tries to lookup in the .foreign/ files, the directory structure follows the
||| full qualified names path.
export
partial
foreign
  :  Ref Ctxt Defs
  => Ref STGCtxt STGContext
  => Name.Name -> (ccs : List String) -> (fargs : List CFType) -> (ret : CFType)
  -> Core TopBinding
foreign n css fargs ret = case mapMaybe stgForeign css of
  []    => findForeignInFile n fargs ret
  [str] => ffiTopBinding n fargs ret str
  _     => coreFail $ InternalError $ "More than one foreign definition for: \{show n}"
