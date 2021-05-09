module Idris.Codegen.ExtSTG.Foreign

import Data.List1
import Data.Strings
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
     CFUnit : CFType
     CFInt : CFType
     CFUnsigned8 : CFType
     CFUnsigned16 : CFType
     CFUnsigned32 : CFType
     CFUnsigned64 : CFType
     CFString : CFType
     CFDouble : CFType
     CFChar : CFType
     CFPtr : CFType
     CFGCPtr : CFType
     CFBuffer : CFType
     CFWorld : CFType
     CFFun : CFType -> CFType -> CFType
     CFIORes : CFType -> CFType
     CFStruct : String -> List (String, CFType) -> CFType
     CFUser : Name -> List CFType -> CFType
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

||| Convert the given String to STG, if it doesn't parse raise an InternalError
partial
exprFromString
  :  UniqueMapRef
  => Ref Counter Int
  => Ref ExternalBinder ExtBindMap
  => DataTypeMapRef
  => Name.Name -> (List CFType) -> CFType -> String
  -> Core TopBinding
exprFromString nm fargs ret str = do
  let Just en = parseForeignStr str
    | Nothing => coreFail $ InternalError $ "FFI name parsing has failed for " ++ str
  -- TODO: File location in the FFI file.
  -- TODO: Make this use of Vector
  -- GHC.CString.unpackCString#
  -- args <- traverse (mkSBinderLocal emptyFC nm . cast) [0..length fargs]
  -- args <- toBinders nm (numberFrom 0 fargs)
  -- args0 <- toBinders nm (numberFrom 0 [CFString, CFWorld])
  arg0    <- mkSBinderRepLocal (SingleValue LiftedRep) emptyFC nm 0
  arg1    <- mkSBinderRepLocal (SingleValue LiftedRep) emptyFC nm 1
  voidArg <- mkSBinderRepLocal (SingleValue VoidRep)   emptyFC nm 2
  let args : BinderList [LiftedRep, LiftedRep, VoidRep]
      args = [arg0, arg1, voidArg]
  pure
    $ StgTopLifted
    $ StgNonRec !(mkSBinderName emptyFC nm)
    $ StgRhsClosure ReEntrant (toSBinderSgList args)
    $ StgCase
        (MultiValAlt 0)
        !(case (en, args) of
            (ForeignPrimOp _, [strArg, worldArg, voidArg]) => do
              pure
                $ StgOpApp
                    PutStr
                    [ StgVarArg (binderId strArg)
                    , StgVarArg (binderId worldArg)
                    , StgVarArg (binderId voidArg)
                    ]
            _ => coreFail $ InternalError $ "BLAH!")
        !(nonusedRep (SingleValue VoidRep))
        [MkAlt AltDefault () $ StgConApp !unitDataConId ()]

{-
$ StgCase
        (AlgAlt !unitTyConId)
        !(case (en, args) of
          (ForeignExtName n, _) => do
            (r ** extFunName) <- extName n
            pure $ StgApp extFunName (toArgSgList (toArgList args)) (SingleValue LiftedRep)
          (ForeignPrimOp n, [strArg,worldArg]) => do -- This is a hack, this needs to be removed.
            pure

          _ => coreFail
                $ InternalError
                $ "Foreign too many arguments.: " ++ show fargs)
        !nonused
        [ MkAlt AltDefault () $ StgConApp !unitDataConId ()] -- TODO: Use different unit type
        -}

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
