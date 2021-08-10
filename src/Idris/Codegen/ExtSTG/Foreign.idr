module Idris.Codegen.ExtSTG.Foreign

import Data.List1
import Data.String
import Core.Name
import Core.Context
import Data.List
import Core.TT
import Core.CompileExpr
import System.File

import Idris.Codegen.ExtSTG.Core
import Idris.Codegen.ExtSTG.Prelude
import Idris.Codegen.ExtSTG.STG
import Idris.Codegen.ExtSTG.ExternalTopIds

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
I think if these should be Boxed datatypes in Haskell.

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
  -- exprFromString: (Prelude.IO.prim__putStr, ([String, %World], IORes Unit))
  -- exprFromString: (Prelude.IO.prim__getStr, ([%World], IORes String))
  -- pure functions does not include the %World type:
  -- exprFromString: (Data.Strings.fastUnpack, ([String], Prelude.Types.List Char))
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
  :  UniqueMapRef
  => Ref Counter Int
  => Name.Name
  -> (cf : List (Nat,CFType))
  -> Core (BinderList (replicate (length cf) LiftedRep))
toBinders nm [] = pure []
toBinders nm ((k,x) :: xs) = do
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
  :  Ref ExternalBinder ExtBindMap
  => UniqueMapRef
  => Ref Counter Int
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

||| Convert the given String to STG, if it doesn't parse raise an InternalError
partial
exprFromString
  :  UniqueMapRef
  => Ref Counter Int
  => Ref ExternalBinder ExtBindMap
  => DataTypeMapRef
  => Name.Name -> (List CFType) -> CFType -> String
  -> Core TopBinding
exprFromString nm [CFString, CFWorld] (CFIORes CFUnit) str = do
  let Just en = parseForeignStr str
    | Nothing => coreFail $ InternalError $ "FFI name parsing has failed for " ++ str
  -- TODO: File location in the FFI file.
  -- TODO: Make this use of Vector
  [stringArg, worldArg] <- toBinders nm (numberFrom 0 [CFString, CFWorld])
    | _ => coreFail $ InternalError "..."
  addrBinder <- mkSBinderRepLocal (SingleValue AddrRep) emptyFC nm 3
  stringBinder <- mkSBinderLocal emptyFC nm 4
  mkUnitBinder <- mkSBinderLocal emptyFC nm 5
  voidBinder <- mkSBinderHardcoded hardcodedVoidHash emptyFC
  ((SingleValue LiftedRep) ** ghcCStringUnpackString)
    <- extName $ MkExtName "ghc-prim" ["GHC","CString"] "unpackCString#"
        | _ => coreFail $ InternalError "....."
  ((SingleValue LiftedRep) ** systemIOputStr)
    <- extName $ MkExtName "base" ["System", "IO"] "putStr"
        | _ => coreFail $ InternalError "..."
  ((AlgDataCon []) ** mkUnitDataConId) <- mkDataConIdStr "Builtin.MkUnit"
    | _ => coreFail $ InternalError "Missing Builtin.MkUnit data constructor."
  ((AlgDataCon [LiftedRep, LiftedRep]) ** mkIOResDataConId) <- mkDataConIdStr "PrimIO.MkIORes"
    | _ => coreFail $ InternalError "Missing PrimIO.MkIORes data constructor."
  pure
    $ StgTopLifted
    $ StgNonRec !(mkSBinderName emptyFC nm)
    $ StgRhsClosure ReEntrant (toSBinderSgList [stringArg, worldArg])
    $ StgCase
        (PrimAlt AddrRep)
        (StgApp
          !addrFromStringBinderId
          [mkArgSg (StgVarArg (binderId stringArg))]
          (SingleValue AddrRep))
        addrBinder
        [ MkAlt AltDefault ()
        $ StgCase
            PolyAlt -- because we only do default pattern matching
            (StgApp
              ghcCStringUnpackString
              [mkArgSg (StgVarArg (binderId addrBinder))]
              (SingleValue LiftedRep))
            stringBinder
            [ MkAlt AltDefault ()
            $ StgCase
                PolyAlt
                (StgApp
                  systemIOputStr
                  [ mkArgSg (StgVarArg (binderId stringBinder))
                  , mkArgSg (StgVarArg (binderId voidBinder))
                  ]
                  (SingleValue LiftedRep))
                !nonused
                [ MkAlt AltDefault ()
                $ StgCase
                    PolyAlt
                    (StgConApp mkUnitDataConId ())
                    mkUnitBinder
                    [ MkAlt AltDefault ()
                    $ StgConApp mkIOResDataConId
                        [ StgVarArg (binderId mkUnitBinder)
                        , StgVarArg (binderId worldArg)
                        ]
                    ]
                ]
                -- [ MkAlt AltDefailt () $ StgConApp MkIORes [StgVarArg MkUnit, StgVarArg worldArg] ]
            ]
        ]
exprFromString nm fargs ret str = do
  coreFail $ InternalError $ "exprFromString: " ++ show (nm, fargs, ret, str)

-- CString: Binary literal:
-- https://hackage.haskell.org/package/ghc-prim-0.6.1/docs/GHC-CString.html
-- IO in STG is not a concept. We need to apply a Void there. We need to use Void primitive.
-- It needs a special argument: Argument => BuiltIn Void

FOREIGN_DIR : String
FOREIGN_DIR = "./.foreign"

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
  :  UniqueMapRef
  => Ref Counter Int
  => Ref Ctxt Defs
  => Ref ExternalBinder ExtBindMap
  => DataTypeMapRef
  => Name.Name -> List CFType -> CFType
  -> Core TopBinding
findForeignInFile nm fargs ret = do
  -- TODO: Make this more efficient with bulk loading of data.
  fn <- toFullNames nm
  -- logLine $ "Foreign name: " ++ show fn
  case fn of
    (NS ns (UN n)) => do
      let path = concat $ intersperse "/" $ (FOREIGN_DIR ::) $ toList $ split (=='.') $ show ns
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
      exprFromString fn fargs ret expr
    other => coreFail $ InternalError $ "Name not in namespace format: " ++ show other

||| Tries to find the definition in the given ccs parameter, if there is no Haskell definition
||| there, it tries to lookup in the .foreign/ files, the directory structure follows the
||| full qualified names path.
export
partial
foreign
  :  UniqueMapRef
  => Ref Counter Int
  => Ref Ctxt Defs
  => Ref ExternalBinder ExtBindMap
  => DataTypeMapRef
  => Name.Name -> (ccs : List String) -> (fargs : List CFType) -> (ret : CFType)
  -> Core TopBinding
foreign n css fargs ret = case mapMaybe stgForeign css of
  []    => findForeignInFile n fargs ret
  [str] => exprFromString n fargs ret str
  _     => coreFail $ InternalError $ "More than one foreign definition for: " ++ show n
