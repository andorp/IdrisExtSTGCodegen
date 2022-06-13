module Idris.Codegen.ExtSTG.Foreign

import Data.List1
import Data.String
import Data.String.Extra
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
import Idris.Codegen.ExtSTG.ExtName
import Idris.Codegen.ExtSTG.ADTAlias

%default total

{-
TODO:
Add IO annotation on the stgffi side.
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
  | ForeignPrimOp STG.Name -- TODO: Remove

parsePrimOp : String -> Maybe STG.Name
parsePrimOp str = case unpack str of
  ('#' :: xs) => Just $ pack xs
  _           => Nothing

parseForeignStr : String -> Maybe ForeignOp
parseForeignStr str =
  (ForeignExtName <$> parseName str) <|>
  (ForeignPrimOp <$> parsePrimOp str)

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
      | []     => coreFail $ InternalError "No foreign is found for \{show name}"
      | others => coreFail $ InternalError "More than one foreigns are found for \{show name} \{show others}"
  pure stg

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
parseTypeDesc : Ref STGCtxt STGContext => (as : List CFType) -> (r : CFType) -> Core (ReprArgs as r)
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
parseTypeDesc [] (CFUser n [CFInt]) = case typeExtName n of
  Nothing => coreFail $ InternalError "Foreign, unsupported user type [] \{CFUser n [CFInt]}"
  Just (ex, _) => pure $ PureRet (CFUser n [CFInt]) (RetUser n [CFInt] ex)
parseTypeDesc [] (CFUser n [CFChar]) = case typeExtName n of
  Nothing => coreFail $ InternalError "Foreign, unsupported user type [] \{CFUser n [CFChar]}"
  Just (ex, _) => pure $ PureRet (CFUser n [CFChar]) (RetUser n [CFChar] ex)
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
parseTypeDesc [CFWorld] (CFIORes (CFUser n [CFChar])) = case typeExtName n of
  Nothing => coreFail $ InternalError "Foreign, unsupported user type [CFWorld] \{CFUser n [CFChar]}}"
  Just (ex, _) => pure $ IORet !(localBinderRep emptyFC (SingleValue LiftedRep)) (CFIORes (CFUser n [CFChar])) (IORetUser n [CFChar] ex)
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
  = case typeExtName n of
      Nothing =>
        coreFail $ InternalError "Foreign, unsupported user type in arguments \{CFUser n [CFInt]}"
      Just (ex, _) => pure
        $ Argument
            !(localBinderRep emptyFC (SingleValue LiftedRep))
            (CFUser n [CFInt])
            (UserArg n [CFInt] ex)
            !(parseTypeDesc xs r)
parseTypeDesc (CFUser n [CFChar] :: xs) r
  = case typeExtName n of
      Nothing =>
        coreFail $ InternalError "Foreign, unsupported user type in arguments \{CFUser n [CFChar]}"
      Just (ex, _) => pure
        $ Argument
            !(localBinderRep emptyFC (SingleValue LiftedRep))
            (CFUser n [CFChar])
            (UserArg n [CFChar] ex)
            !(parseTypeDesc xs r)
parseTypeDesc (x :: xs) r
  = coreFail $ InternalError "Foreign, unsupported type: \{x :: xs} \{r}"

mkUnitDataCon : Ref STGCtxt STGContext => Core DataConIdSg
mkUnitDataCon = do
  Just dataConUnique <- lookupIdrisTermNamespace "Builtin.MkUnit"
    | Nothing => coreFail $ InternalError "returnDataConId: Builtin.MkUnit constructor is not registered."
  case !(getDataCons dataConUnique) of
    Nothing   => coreFail $ InternalError "returnDataConId: Couldn't find Binder for Builtin.MkUnit."
    Just []   => coreFail $ InternalError "returnDataConId: Couldn't find Binder for Builtin.MkUnit. Empty list, this should not have happened."
    Just [d]  => pure (identSg d)
    Just ds   => coreFail $ InternalError "returnDataConId: Found more than one Binders for Builtin.MkUnit."

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
  (UnboxedTupleCon 1 ** dataConId) <- mkDataConIdExtName soloExtName
    | (rep ** _) => coreFail $ InternalError "Unexpected rep type: \{show rep}"
  pure
    $ StgCase
        (MultiValAlt 1)
        -- Call the Haskell code and unwrap the IO
        (StgApp fun args (UnboxedTuple [LiftedRep]))
        resBinder
        [ MkAlt (AltUnboxedOneTuple dataConId) unboxed $ StgApp (getBinderId unboxed) [] _
        ]

total
renderIORetExpr
  :  Ref STGCtxt STGContext
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
functionArguments : ReprArgs as r -> List ArgSg
functionArguments (IORet b r ret)     = [mkArgSg (StgVarArg realWorldHashtag)]
functionArguments (PureRet r ret)     = []
functionArguments (Argument b a s rs) = mkArgSg (StgVarArg (getBinderId b)) :: functionArguments rs

total
renderReturnExpr
  :  Ref STGCtxt STGContext
  => {as : List CFType}
  -> {r : CFType}
  -> BinderId Core.stgRepType
  -> List ArgSg
  -> ReprArgs as r
  -> Core (Expr Core.stgRepType)
renderReturnExpr fun args (IORet b r ret)     = renderIORetExpr fun args ret
renderReturnExpr fun args (PureRet r ret)     = pure $ StgApp fun args (SingleValue LiftedRep)
renderReturnExpr fun args (Argument b a s rs) = renderReturnExpr fun args rs

covering
findForeignInFile
  :  Ref Ctxt Defs
  => Ref STGCtxt STGContext
  => Name.Name
  -> Core (BinderId Core.stgRepType)
findForeignInFile nm = do
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
      idrisEqHaskallName <- findForeign (displayUserName n) content
      let Just (ForeignExtName external) = parseForeignStr idrisEqHaskallName
        | Just (ForeignPrimOp po) => coreFail $ InternalError "Foreign primop has found \{show po} instead of external name."
        | Nothing => coreFail $ InternalError $ "FFI name parsing has failed for \{idrisEqHaskallName}"
      ((SingleValue LiftedRep) ** ffiFunctionBinder) <- extName external
        | _ => coreFail $ InternalError "..."
      pure ffiFunctionBinder
    other => coreFail $ InternalError $ "Name not in namespace format: \{show other}"

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
      | Just (ForeignPrimOp po) => coreFail $ InternalError "Foreign primop has found \{show po} instead of external name."
      | Nothing => coreFail $ InternalError $ "FFI name parsing has failed for \{idrisEqHaskallName}"
    ((SingleValue LiftedRep) ** ffiFunctionBinder) <- extName external
      | _ => coreFail $ InternalError "..."
    pure $ Just ffiFunctionBinder

||| Tries to find the definition in the given ccs parameter, if there is no Haskell definition
||| there, it tries to lookup in the .foreign/ files, the directory structure follows the
||| full qualified names path.
export
partial -- TODO: Remove after holes are resolved -- Core.Context.lookupCtxtExactI
foreign
  :  Ref Ctxt Defs
  => Ref STGCtxt STGContext
  => List String
  -> Name.Name -> List CFType -> CFType
  -> Core TopBinding
foreign css funName args ret = do
  ffiReprArgs <- parseTypeDesc args ret
  stgFunName <- maybe (findForeignInFile funName) pure =<< findCSSDefinition css
  retExpr <- renderReturnExpr stgFunName (functionArguments ffiReprArgs) ffiReprArgs
  funNameBinder <- mkSBinderName emptyFC funName
  pure
    $ StgTopLifted
    $ StgNonRec funNameBinder
    $ StgRhsClosure ReEntrant (map mkSBinderSg (argumentBinders ffiReprArgs))
    $ retExpr
