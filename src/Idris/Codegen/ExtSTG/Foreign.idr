module Idris.Codegen.ExtSTG.Foreign

import Core.CompileExpr
import Core.Context
import Core.Name
import Core.TT
import Data.List
import Data.List.Views
import Data.List1
import Data.String
import Data.String.Extra
import System.File

import Idris.Codegen.ExtSTG.ADTAlias
import Idris.Codegen.ExtSTG.ADTs
import Idris.Codegen.ExtSTG.Configuration
import Idris.Codegen.ExtSTG.Context
import Idris.Codegen.ExtSTG.Core
import Idris.Codegen.ExtSTG.ExtName
import Idris.Codegen.ExtSTG.ForeignFile
import Idris.Codegen.ExtSTG.Prelude
import Idris.Codegen.ExtSTG.STG

%default total

record Foreign where
  constructor MkForeign
  name     : Name.Name
  fargs    : List CFType
  ret      : CFType
  topLevel : TopBinding

data ForeignOp
  = ForeignExtName ExtName

||| Parse names that are expected to have the following format:
||| package:namespace.entries.function
-- export
parseForeignStr : String -> Maybe ForeignOp
parseForeignStr str = ForeignExtName <$> parseName str
  where
    parseName : String -> Maybe ExtName
    parseName str = case break (=='_') $ unpack str of
      ([], something)   => Nothing
      (something, [])   => Nothing
      (package, names)  => parseModuleName package $ toList $ splitOn '.' $ drop 1 names
      where
        parseModuleName : List Char -> List (List Char) -> Maybe ExtName
        parseModuleName pkg xs with (snocList xs)
          parseModuleName pkg []          | Empty      = Nothing
          parseModuleName pkg (ys ++ [y]) | Snoc _ _ _ = Just $ MkExtName (pack pkg) (map pack ys) (pack y)

Interpolation CFType where
  interpolate CFUnit = "CFUnit"
  interpolate CFInt = "CFInt"
  interpolate CFInteger = "CFInteger"
  interpolate CFInt8 = "CFInt8"
  interpolate CFInt16 = "CFInt16"
  interpolate CFInt32 = "CFInt32"
  interpolate CFInt64 = "CFInt64"
  interpolate CFUnsigned8 = "CFUnsigned8"
  interpolate CFUnsigned16 = "CFUnisgned16"
  interpolate CFUnsigned32 = "CFUnisgned32"
  interpolate CFUnsigned64 = "CFUnisgned64"
  interpolate CFString = "CFString"
  interpolate CFDouble = "CFDouble"
  interpolate CFChar = "CFChar"
  interpolate CFPtr = "CFPtr"
  interpolate CFGCPtr = "CFGCPtr"
  interpolate CFBuffer = "CFBuffer"
  interpolate CFForeignObj = "CFForeignObj"
  interpolate CFWorld = "CFWorld"
  interpolate (CFFun x y) = "(CFFun " ++ interpolate x ++ " " ++ interpolate y ++ ")"
  interpolate (CFIORes x) = "(CFIORes " ++ interpolate x ++ ")"
  interpolate (CFStruct x xs) = "(CFStruct " ++ interpolate x ++ " TODO:xs)"
  interpolate (CFUser x xs) = "(CFUser " ++ show x ++ assert_total (concat (map interpolate xs)) ++ ")"

Interpolation a => Interpolation (List a) where
  interpolate xs = unwords $ map interpolate xs

{-
We need to create an STG expression which will invoke the function written in Haskell. The haskell implemented
FFI counterpart must be monomorphic, this is needed to not to introduce unnecessary complexity coming from the
type class constraints, and dictionary passing on the Haskell side. We don't want to force haskell type class
implementation details on the Idris side. For that reason we need monomorp functions.

On the Idris side, Idris uses primitive values in the ffi functions. The primitive values are represented as boxed
values on the Haskell side. For example Int in Idris side is represented as (I# i) in STG. This means the FFI
doesn't need to do wrapping and unwrapping in the most of the cases. Except for String, which is detailed below.

In the first version of FFI we are not going to support callbacks, although in STG it should be just an expression
so adding that later probably will not require any special treatment.

There are two different functions in terms of FFI. The pure functions and the PrimIO functions in Idris.
The PrimIO is encoded with the IORes constructor. At the Haskell side, with STG the actions are represented
as extra parameter to the function representing STG expression, which is a VoidRep, so a special value needs
to be passed in them.
Lets see some made up examples:

For IO computations, we need to represent the World somehow, and pass it as an extra parameter.
On the STG side, when primitive operations are invoked, they have similar form as this example:
readWord8Array# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, Word# #)
Where state is passed in and ignored the notation of the extra state information is phantom, and
it should be ignored. It is an important detail, but not here.


IO Examples:

Idris side:
Prelude.IO.prim__putStr = Foreign call [String, %World] -> IORes Unit
MkIORes : result -> %World -> IORes a

print          : String -> IO ()
prim_print     : String -> PrimIO ()
prim_print_ffi : CFString -> CFWorld -> CFIORes CFUnit
prim_print_stg : LiftedRep(IdrString) -> LifterRep(IdrWorld) -> LiftedRep(MkIORes IdrUnit IdrWorld)

getInt : IO Int
prim_getInt : PrimIO Int
prim_getInt_ffi : CFWorld -> CFIORes CFInt
prim_getInt_stg : LiftedRep(IdrWorld) -> LiftedRep(MkIORes Int IdrWorld)

Pure examples:
length : String -> Int
length_ffi : CFString -> CFInt
length_stg : LiftedRep(IdrString) -> LiftedRep(GHC.Int)

String needs special handling, with wrapping and unwrapping.
In Idris we represented the primitive String type as an ADT on the STG side with two different representation

For arguments on Haskell side we need to convert the IdrString representation to the [Char] representation of Haskell.
This is done via GHC.CString.unpackCString# : Addr# -> String, which requires to extract the Addr# part of the
IdrString, simple extracting the Addr# from a String constant, or converting the MutableCharArray# to Addr#

For a return value we get a [Char] and we need to turn that into an Addr# via MutableByteArray#, and the ByteArray#
needs to be wrapped with the right constructor Idr.String.Val ByteArray#

Primitive types in Idris represented as normal haskell types such as (I# i), this makes the foreign implementation
easier.

Lets see the corresponding Haskell types to CFTypes

CFUnit       is represented as our IdrUnit implementation
CFInt        is represented as GHC.Int
CFUnsigned8  is represented as GHC.Word8
CFUnsigned16 is represented as GHC.Word16
CFUnsigned32 is represented as GHC.Word32
CFUnsigned64 is represented as GHC.Word64
CFString     is represented as our IdrString implementation
CFDouble     is represented as GHC.Double
CFChar       is represented as GHC.Word8 !!!
CFPtr        is represented as any lifted type : CFType -- not supported yet
CFGCPtr      is not supported yet.
CFBuffer     is not supported yet.
CFWorld      is represented as Idr.World
CFFun        is not supported yet.
CFIORes      is represented as PrimIO.MkIORes mapping, but only supported at return value. Argument support is not planned yet.
CFStruct     is not supported yet.
CFUser       is not supported yet.

CFIORes (CFIORes ...) is not supported yet.

When generating the topLevel definition for the foreign call we need to create the optional conversion code
from String and maybe other types, and we need to call the function that is associated with the foreign
call. The return value needs to have a conversion if its an IORes a, String, or IORes String.

In case of String we need to convert the result value from [Char] -> ByteArray#
In case of IORes we need to add the extra World value to the result.
In case of IORes String we need to do both.

Foreign strings are loaded from files and parsed into package:Module.Path.function.

STG Examples:

No conversation needed:
StgTopLifted
StgNonRec funName
StgRhsClosure ReEntrant ffiArgs
StgApp ffiFunctionBinder ffiArgs

String argument with conversation:
StgTopLifted
StgNonRec funName
StgRhsClosure ReEntrant (ffiArgs1 ++ [strArg] ++ ffiArgs2)
... convertStringIdrisToHaskell functionArguments ghcStgArg
StgApp ffiFunctionBinder (ffiArgs1 ++ [ghcStgArg] ++ ffiArgs2

Sting return type:
StgTopLifted
StgNonRec funName
StgRhsClosure ReEntrant ffiArgs
StgCase (StgApp ffiFunctionBinder ffiArgs) ffiRes
  [ Default () (convertStringHaskellToIdris ffiRes)
  ]

IORes return type:
StgTopLifted
StgNonRec funName
StgRhsClosure ReEntrant (ffiArgs ++ [worldArg])
StgCase (StgApp ffiFunctionBinder (ffiArgs ++ [worldArg])) ffiRes
  [ Default () (MkIORes ffiRes worldArg)]

IORes String return type:
StgTopLifted
StgNonRec funName
StgRhsClosure ReEntrant (ffiArgs ++ [worldArg])
StgCase (StgApp ffiFunctionBinder (ffiArgs ++ [worldArg])) ffiRes
  [ Default () (StgCase (convertHaskellToIdrisString ffiRes) strRes
      [ Default () (MkIORes strRes2 worldArg)]
-}

||| Representable IO return type
data IORetRepr : CFType -> Type where
  IORetUnit   : IORetRepr (CFIORes CFUnit)
  IORetInt    : IORetRepr (CFIORes CFInt)
  IORetInt8   : IORetRepr (CFIORes CFInt8)
  IORetInt16  : IORetRepr (CFIORes CFInt16)
  IORetInt32  : IORetRepr (CFIORes CFInt32)
  IORetInt64  : IORetRepr (CFIORes CFInt64)
  IORetBits8  : IORetRepr (CFIORes CFUnsigned8)
  IORetBits16 : IORetRepr (CFIORes CFUnsigned16)
  IORetBits32 : IORetRepr (CFIORes CFUnsigned32)
  IORetBits64 : IORetRepr (CFIORes CFUnsigned64)
  IORetChar   : IORetRepr (CFIORes CFChar)
  IORetString : IORetRepr (CFIORes CFString)
  IORetDouble : IORetRepr (CFIORes CFDouble)
  IORetUser   : (n : Core.Name.Name) -> (as : List CFType) -> ExtName -> IORetRepr (CFIORes (CFUser n as))

||| Representable return type
data RetRepr : CFType -> Type where
  RetInt    : RetRepr CFInt
  RetInt8   : RetRepr CFInt8
  RetInt16  : RetRepr CFInt16
  RetInt32  : RetRepr CFInt32
  RetInt64  : RetRepr CFInt64
  RetBits8  : RetRepr CFUnsigned8
  RetBits16 : RetRepr CFUnsigned16
  RetBits32 : RetRepr CFUnsigned32
  RetBits64 : RetRepr CFUnsigned64
  RetString : RetRepr CFString
  RetChar   : RetRepr CFChar
  RetDouble : RetRepr CFDouble
  RetPtr    : RetRepr CFPtr
  RetUser   : (n : Core.Name.Name) -> (as : List CFType) -> ExtName -> RetRepr (CFUser n as)

data SupportedArg : CFType -> Type where
  IntArg    : SupportedArg CFInt
  Int8Arg   : SupportedArg CFInt8
  Int16Arg  : SupportedArg CFInt16
  Int32Arg  : SupportedArg CFInt32
  Int64Arg  : SupportedArg CFInt64
  Bits8Arg  : SupportedArg CFUnsigned8
  Bits16Arg : SupportedArg CFUnsigned16
  Bits32Arg : SupportedArg CFUnsigned32
  Bits64Arg : SupportedArg CFUnsigned64
  DblArg    : SupportedArg CFDouble
  CharArg   : SupportedArg CFChar
  StringArg : SupportedArg CFString
  UserArg   : (n : Core.Name.Name) -> (as : List CFType) -> ExtName -> SupportedArg (CFUser n as)
  PtrArg    : SupportedArg CFPtr

||| Representable arguments
data ReprArgs : List CFType -> CFType -> Type where
  IORet
    :  (b : SBinder (SingleValue LiftedRep))
    -> (r : CFType)
    -> (ret : IORetRepr r)
    -> ReprArgs [CFWorld] r
  PureRet
    :  (r : CFType)
    -> (ret : RetRepr r)
    -> ReprArgs [] r
  Argument
    :  (b : SBinder (SingleValue LiftedRep))
    -> (a : CFType)
    -> (s : SupportedArg a)
    -> (rs : ReprArgs as r)
    -> ReprArgs (a :: as) r

||| Check if the given argument list and return type is supported.
covering
parseTypeDesc : Ref Ctxt Defs => Ref STGCtxt STGContext => (as : List CFType) -> (r : CFType) -> Core (ReprArgs as r)
parseTypeDesc [] CFInt         = pure $ PureRet CFInt RetInt
parseTypeDesc [] CFInt8        = pure $ PureRet CFInt8 RetInt8
parseTypeDesc [] CFInt16       = pure $ PureRet CFInt16 RetInt16
parseTypeDesc [] CFInt32       = pure $ PureRet CFInt32 RetInt32
parseTypeDesc [] CFInt64       = pure $ PureRet CFInt64 RetInt64
parseTypeDesc [] CFUnsigned8   = pure $ PureRet CFUnsigned8 RetBits8
parseTypeDesc [] CFUnsigned16  = pure $ PureRet CFUnsigned16 RetBits16
parseTypeDesc [] CFUnsigned32  = pure $ PureRet CFUnsigned32 RetBits32
parseTypeDesc [] CFUnsigned64  = pure $ PureRet CFUnsigned64 RetBits64
parseTypeDesc [] CFString      = pure $ PureRet CFString RetString
parseTypeDesc [] CFDouble      = pure $ PureRet CFDouble RetDouble
parseTypeDesc [] CFChar        = pure $ PureRet CFChar RetChar
parseTypeDesc [] CFPtr         = pure $ PureRet CFPtr RetPtr
parseTypeDesc [] (CFUser n [CFInt]) = case !(typeExtName n) of
  Nothing => coreFail $ InternalError "Foreign, unsupported user type [] \{CFUser n [CFInt]}"
  Just ex => pure $ PureRet (CFUser n [CFInt]) (RetUser n [CFInt] ex)
parseTypeDesc [] (CFUser n [CFChar]) = case !(typeExtName n) of
  Nothing => coreFail $ InternalError "Foreign, unsupported user type [] \{CFUser n [CFChar]}"
  Just ex => pure $ PureRet (CFUser n [CFChar]) (RetUser n [CFChar] ex)
parseTypeDesc [] r             = coreFail $ InternalError "Foreign, unsupported type: [] \{r}"
parseTypeDesc [CFWorld] (CFIORes CFUnit)        = pure $ IORet !(localBinderRep emptyFC (SingleValue LiftedRep)) (CFIORes CFUnit)       IORetUnit
parseTypeDesc [CFWorld] (CFIORes CFInt)         = pure $ IORet !(localBinderRep emptyFC (SingleValue LiftedRep)) (CFIORes CFInt)        IORetInt
parseTypeDesc [CFWorld] (CFIORes CFInt8)        = pure $ IORet !(localBinderRep emptyFC (SingleValue LiftedRep)) (CFIORes CFInt8)       IORetInt8
parseTypeDesc [CFWorld] (CFIORes CFInt16)       = pure $ IORet !(localBinderRep emptyFC (SingleValue LiftedRep)) (CFIORes CFInt16)      IORetInt16
parseTypeDesc [CFWorld] (CFIORes CFInt32)       = pure $ IORet !(localBinderRep emptyFC (SingleValue LiftedRep)) (CFIORes CFInt32)      IORetInt32
parseTypeDesc [CFWorld] (CFIORes CFInt64)       = pure $ IORet !(localBinderRep emptyFC (SingleValue LiftedRep)) (CFIORes CFInt64)      IORetInt64
parseTypeDesc [CFWorld] (CFIORes CFUnsigned8)   = pure $ IORet !(localBinderRep emptyFC (SingleValue LiftedRep)) (CFIORes CFUnsigned8)  IORetBits8
parseTypeDesc [CFWorld] (CFIORes CFUnsigned16)  = pure $ IORet !(localBinderRep emptyFC (SingleValue LiftedRep)) (CFIORes CFUnsigned16) IORetBits16
parseTypeDesc [CFWorld] (CFIORes CFUnsigned32)  = pure $ IORet !(localBinderRep emptyFC (SingleValue LiftedRep)) (CFIORes CFUnsigned32) IORetBits32
parseTypeDesc [CFWorld] (CFIORes CFUnsigned64)  = pure $ IORet !(localBinderRep emptyFC (SingleValue LiftedRep)) (CFIORes CFUnsigned64) IORetBits64
parseTypeDesc [CFWorld] (CFIORes CFChar)        = pure $ IORet !(localBinderRep emptyFC (SingleValue LiftedRep)) (CFIORes CFChar)       IORetChar
parseTypeDesc [CFWorld] (CFIORes CFString)      = pure $ IORet !(localBinderRep emptyFC (SingleValue LiftedRep)) (CFIORes CFString)     IORetString
parseTypeDesc [CFWorld] (CFIORes CFDouble)      = pure $ IORet !(localBinderRep emptyFC (SingleValue LiftedRep)) (CFIORes CFDouble)     IORetDouble
parseTypeDesc [CFWorld] (CFIORes (CFUser n [CFChar])) = case !(typeExtName n) of
  Nothing => coreFail $ InternalError "Foreign, unsupported user type [CFWorld] \{CFUser n [CFChar]}}"
  Just ex => pure $ IORet !(localBinderRep emptyFC (SingleValue LiftedRep)) (CFIORes (CFUser n [CFChar])) (IORetUser n [CFChar] ex)
parseTypeDesc [CFWorld] r
  = coreFail $ InternalError "Foreign, unsupported type: [CFWorld] \{r}"
parseTypeDesc (CFInt :: xs) r
  = pure $ Argument !(localBinderRep emptyFC (SingleValue LiftedRep)) CFInt IntArg !(parseTypeDesc xs r)
parseTypeDesc (CFInt8 :: xs) r
  = pure $ Argument !(localBinderRep emptyFC (SingleValue LiftedRep)) CFInt8 Int8Arg !(parseTypeDesc xs r)
parseTypeDesc (CFInt16 :: xs) r
  = pure $ Argument !(localBinderRep emptyFC (SingleValue LiftedRep)) CFInt16 Int16Arg !(parseTypeDesc xs r)
parseTypeDesc (CFInt32 :: xs) r
  = pure $ Argument !(localBinderRep emptyFC (SingleValue LiftedRep)) CFInt32 Int32Arg !(parseTypeDesc xs r)
parseTypeDesc (CFInt64 :: xs) r
  = pure $ Argument !(localBinderRep emptyFC (SingleValue LiftedRep)) CFInt64 Int64Arg !(parseTypeDesc xs r)
parseTypeDesc (CFUnsigned8 :: xs) r
  = pure $ Argument !(localBinderRep emptyFC (SingleValue LiftedRep)) CFUnsigned8 Bits8Arg !(parseTypeDesc xs r)
parseTypeDesc (CFUnsigned16 :: xs) r
  = pure $ Argument !(localBinderRep emptyFC (SingleValue LiftedRep)) CFUnsigned16 Bits16Arg !(parseTypeDesc xs r)
parseTypeDesc (CFUnsigned32 :: xs) r
  = pure $ Argument !(localBinderRep emptyFC (SingleValue LiftedRep)) CFUnsigned32 Bits32Arg !(parseTypeDesc xs r)
parseTypeDesc (CFUnsigned64 :: xs) r
  = pure $ Argument !(localBinderRep emptyFC (SingleValue LiftedRep)) CFUnsigned64 Bits64Arg !(parseTypeDesc xs r)
parseTypeDesc (CFChar :: xs) r
  = pure $ Argument !(localBinderRep emptyFC (SingleValue LiftedRep)) CFChar CharArg !(parseTypeDesc xs r)
parseTypeDesc (CFString :: xs) r
  = pure $ Argument !(localBinderRep emptyFC (SingleValue LiftedRep)) CFString StringArg !(parseTypeDesc xs r)
parseTypeDesc (CFDouble :: xs) r
  = pure $ Argument !(localBinderRep emptyFC (SingleValue LiftedRep)) CFDouble DblArg !(parseTypeDesc xs r)
parseTypeDesc (CFPtr :: xs) r
  = pure $ Argument !(localBinderRep emptyFC (SingleValue LiftedRep)) CFPtr PtrArg !(parseTypeDesc xs r)
parseTypeDesc (CFUser n [CFInt] :: xs) r
  = case !(typeExtName n) of
      Nothing =>
        coreFail $ InternalError "Foreign, unsupported user type in arguments \{CFUser n [CFInt]}"
      Just ex => pure
        $ Argument
            !(localBinderRep emptyFC (SingleValue LiftedRep))
            (CFUser n [CFInt])
            (UserArg n [CFInt] ex)
            !(parseTypeDesc xs r)
parseTypeDesc (CFUser n [CFChar] :: xs) r
  = case !(typeExtName n) of
      Nothing =>
        coreFail $ InternalError "Foreign, unsupported user type in arguments \{CFUser n [CFChar]}"
      Just ex => pure
        $ Argument
            !(localBinderRep emptyFC (SingleValue LiftedRep))
            (CFUser n [CFChar])
            (UserArg n [CFChar] ex)
            !(parseTypeDesc xs r)
parseTypeDesc (x :: xs) r
  = coreFail $ InternalError "Foreign, unsupported type: \{x :: xs} \{r}"

covering
mkUnitDataCon : Ref Ctxt Defs => Ref STGCtxt STGContext => Core DataConIdSg
mkUnitDataCon = do
  let unitName = NS (mkNamespace "Builtin") (UN (Basic "MkUnit"))
  map (identSg . fst) $ lookupDTCon unitName

||| Render the function call part of the FFI expression.
|||
||| This is the simple case, when no conversion is needed for the return value.
renderIORetNoConvExpr
  :  Ref STGCtxt STGContext
  => BinderId Core.stgRepType
  -> List ArgSg
  -> Core (Expr Core.stgRepType)
renderIORetNoConvExpr fun args = do
  resBinder <- localBinderRep emptyFC (UnboxedTuple [LiftedRep])
  unboxed <- mkFreshSBinderStr LocalScope emptyFC "resultForce"
  (UnboxedTupleCon 1 ** dataConId) <- map identSg $ lookupExtNameDTCon soloExtName
    | (rep ** _) => coreFail $ InternalError "Unexpected rep type: \{show rep}"
  pure
    $ StgCase
        (MultiValAlt 1)
        -- Call the Haskell code and unwrap the IO
        (StgApp fun args (UnboxedTuple [LiftedRep]))
        resBinder
        [ MkAlt (AltUnboxedOneTuple dataConId) unboxed $ StgApp (getBinderId unboxed) [] _
        ]

covering
renderIORetExpr
  :  Ref Ctxt Defs
  => Ref STGCtxt STGContext
  => {r : CFType}
  -> BinderId Core.stgRepType -- Use external instead???
  -> List ArgSg
  -> IORetRepr r
  -> Core (Expr Core.stgRepType)
renderIORetExpr fun args IORetUnit = do
  resBinder <- localBinderRep emptyFC (SingleValue LiftedRep)
  ((AlgDataCon []) ** mkUnitDataConId) <- mkUnitDataCon
    | _ => coreFail $ InternalError "Missing PrimIO.MkUnit data constructor."
  pure
    $ StgCase
        PolyAlt
        (StgApp fun args (SingleValue LiftedRep))
        !nonused
        [ MkAlt AltDefault ()
          $ StgCase
              PolyAlt
              (StgConApp mkUnitDataConId ())
              resBinder
              [ MkAlt AltDefault ()
                $ StgApp (getBinderId resBinder) [] _
              ]
        ]
renderIORetExpr fun args IORetInt    = renderIORetNoConvExpr fun args
renderIORetExpr fun args IORetInt8   = renderIORetNoConvExpr fun args
renderIORetExpr fun args IORetInt16  = renderIORetNoConvExpr fun args
renderIORetExpr fun args IORetInt32  = renderIORetNoConvExpr fun args
renderIORetExpr fun args IORetInt64  = renderIORetNoConvExpr fun args
renderIORetExpr fun args IORetBits8  = renderIORetNoConvExpr fun args
renderIORetExpr fun args IORetBits16 = renderIORetNoConvExpr fun args
renderIORetExpr fun args IORetBits32 = renderIORetNoConvExpr fun args
renderIORetExpr fun args IORetBits64 = renderIORetNoConvExpr fun args
renderIORetExpr fun args IORetChar   = renderIORetNoConvExpr fun args
renderIORetExpr fun args IORetString = renderIORetNoConvExpr fun args
renderIORetExpr fun args IORetDouble = renderIORetNoConvExpr fun args
renderIORetExpr fun args (IORetUser n as ex) = renderIORetNoConvExpr fun args

-- ||| Binders for arguments of the Foreign definition in Idris
argumentBinders : ReprArgs as r -> List (SBinder Core.stgRepType)
argumentBinders (IORet b r ret)     = [b]
argumentBinders (PureRet r ret)     = []
argumentBinders (Argument b a s rs) = b :: argumentBinders rs

||| Create arguments for foreign call, when PrimIO is defined for the
||| the last argument on the Haskell side it must be Void as the Haskell
||| IO function expects a VoidRep for IO functions.
functionArguments : Ref STGCtxt STGContext => ReprArgs as r -> Core (List ArgSg)
functionArguments (IORet b r ret)     = pure $ [mkArgSg $ StgVarArg $ binderId !realWorldHashBinder]
functionArguments (PureRet r ret)     = pure []
functionArguments (Argument b a s rs) = map (mkArgSg (StgVarArg (getBinderId b)) ::) (functionArguments rs)

covering
renderReturnExpr
  :  Ref Ctxt Defs
  => Ref STGCtxt STGContext
  => {as : List CFType}
  -> {r : CFType}
  -> BinderId Core.stgRepType
  -> List ArgSg
  -> ReprArgs as r
  -> Core (Expr Core.stgRepType)
renderReturnExpr fun args (IORet b r ret)     = renderIORetExpr fun args ret
renderReturnExpr fun args (PureRet r ret)     = pure $ StgApp fun args (SingleValue LiftedRep)
renderReturnExpr fun args (Argument b a s rs) = renderReturnExpr fun args rs

findCSSDefinition
  :  Ref Ctxt Defs
  => Ref STGCtxt STGContext
  => List String
  -> Core (Maybe (BinderId Core.stgRepType))
findCSSDefinition [] = pure Nothing
findCSSDefinition (def :: defs) = case isPrefixOf "stg:" def of
  False => findCSSDefinition defs
  True  => do
    let idrisEqHaskallName = drop 4 def
    let Just (ForeignExtName external) = parseForeignStr idrisEqHaskallName
      | Nothing => coreFail $ InternalError $ "FFI name parsing has failed for \{idrisEqHaskallName}"
    ffiFunctionBinder <- extNameLR external
    pure $ Just ffiFunctionBinder.binderId

covering
lookupFFIExtName : Ref STGCtxt STGContext => Ref Ctxt Defs => Name.Name -> Core (ExtName, BinderId (SingleValue LiftedRep))
lookupFFIExtName n = do
  Just (mdl, nm) <- nameToPath n
    | Nothing => coreFail $ InternalError "Name resolution during FFI lookup failed. Expected user defined name."
  ctx <- get STGCtxt
  foreignDir <- map (.foreignDirectory) getConfiguration
  (e, f) <- ffiExtName ctx.ffiFiles foreignDir mdl nm
  modifySTGCtxt ({ ffiFiles := f})
  binder <- extNameLR e
  pure (e, binder.binderId)

||| Tries to find the definition in the given ccs parameter, if there is no Haskell definition
||| there, it tries to lookup in the .foreign/ files, the directory structure follows the
||| full qualified names path.
export
partial -- TODO: Remove after holes are resolved -- Core.Context.lookupCtxtExactI
foreign
  :  Ref Ctxt Defs
  => Ref STGCtxt STGContext
  => FC -> List String -> Name.Name -> List CFType -> CFType
  -> Core TopBinding
foreign fc css funName args ret = do
  ffiReprArgs <- parseTypeDesc args ret
  stgFunName <- maybe (map snd (lookupFFIExtName funName)) pure =<< findCSSDefinition css
  retExpr <- renderReturnExpr stgFunName !(functionArguments ffiReprArgs) ffiReprArgs
  funNameBinder <- lookupFunctionBinder funName
  pure
    $ StgTopLifted
    $ StgNonRec funNameBinder
    $ StgRhsClosure ReEntrant (map mkSBinderSg (argumentBinders ffiReprArgs))
    $ retExpr

||| Try to find the ExtName definition in the stgffi file.
|||
||| If not found it throws an internal error.
export
covering
extPrimName
  :  Ref Ctxt Defs
  => Ref STGCtxt STGContext
  => Name.Name
  -> Core ExtName
extPrimName = map fst . lookupFFIExtName
