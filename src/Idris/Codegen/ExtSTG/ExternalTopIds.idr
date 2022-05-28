module Idris.Codegen.ExtSTG.ExternalTopIds

import Data.String
import Data.List
import Data.List1
import Data.List.Views
import Core.Context
import Libraries.Data.StringMap
import Idris.Codegen.ExtSTG.Core
import Idris.Codegen.ExtSTG.STG
import Idris.Codegen.ExtSTG.ExtName
import Idris.Codegen.ExtSTG.Context

{-
When referring a function from a different module, a binder must be created for the
name and it this binder must be registered in the ExternalTopIds of the module.

The STGApp needs a BinderId which will be the BinderId locally associated with
the binder in ExternalTopIds. The code generation for the binders in the ExternalTopIds
are special and the 'Unique's are not used.

During the compilation we need to collect the Binders and only use their Ids in the StgApp.

How do we store the Binders already? We associate Names with Uniques and we get the BinderIds
for free. But still we have to collect the externals for this name.
-}

%default total

renderName : ExtName -> String
renderName (MkExtName pkg mdl fn) = pkg ++ "_" ++ concat (intersperse "." mdl) ++ "." ++ fn

||| Parse names that are expected to have the following format:
||| package:namespace.entries.function
export
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

||| Ask for a BinderId for the given name, if there is, if not create a one Binder and
||| register in the ExtBindMap
export
extName
  :  Ref STGCtxt STGContext
  => ExtName
  -> Core BinderIdSg
extName e@(MkExtName pkg mdl fn) = do
  let entryName = renderName e
  extBind <- lookupExtBinds entryName
  case extBind of
    Nothing => do
      binder <- map mkSBinderSg $ mkSBinderStr emptyFC fn -- TODO: Fix this HaskellExported etc
      insertExtBinds entryName (e, binder)
      pure $ getSBinderIdSg binder
    Just (_, b) => pure $ getSBinderIdSg b

||| Ask for a BinderId for the given name, if there is, if not create a one Binder and
||| register in the ExtBindMap
export
extNameLR
  :  Ref STGCtxt STGContext
  => ExtName
  -> Core (BinderId (SingleValue LiftedRep))
extNameLR e = do
  ((SingleValue LiftedRep) ** binderId) <- extName e
    | _ => coreFail $ InternalError "extNameLR: Unexpected rep"
  pure binderId

export
registerHardcodedExtTopIds
  :  Ref STGCtxt STGContext
  => Core ()
registerHardcodedExtTopIds = do
  binder <- map mkSBinderSg $ mkSBinderHardcoded hardcodedVoidHash emptyFC
  let (unt,mod,fn,_,_) = hardcodedVoidHash
  let e = MkExtName unt mod fn
  let entryName = renderName e
  insertExtBinds entryName (e,binder)

||| Generate External Top Ids for an STG module.
export
genExtTopIds
  :  Ref STGCtxt STGContext
  => Core (List (UnitId, ModuleName, SBinderSg))
genExtTopIds = do
  map ( map
            (\(key, (MkExtName pck mdl fn, binder)) =>
              (MkUnitId pck, MkModuleName (concat (intersperse "." mdl)), binder))
        . StringMap.toList
        )
      $ getExtBinds


||| Create an StgCase which will represent and force the result for IO external function.
|||
||| Use this function when the external haskell function needs an IO computation.
export
createExtSTGIOApp
  :  Ref STGCtxt STGContext
  => ExtName -> List ArgSg
  -> Core (Expr Core.stgRepType)
createExtSTGIOApp ext args = do
  extCallResult <- mkFreshSBinderRepStr LocalScope (UnboxedTuple [LiftedRep]) emptyFC "extCallIOResult"
  extCallResult2 <- mkFreshSBinderStr LocalScope emptyFC "extCallIOResultForce"
  extNameBinderId <- extNameLR ext
  (UnboxedTupleCon 1 ** dataConId) <- mkDataConIdExtName soloExtName
    | (rep ** _) => coreFail $ InternalError "Unexpected rep type: \{show rep}"
  pure
    $ StgCase
        (MultiValAlt 1) -- IO
        (StgApp extNameBinderId args (UnboxedTuple [LiftedRep]))
        extCallResult
        [ MkAlt (AltUnboxedOneTuple dataConId) extCallResult2
          $ (StgApp (getBinderId extCallResult2) [] (SingleValue LiftedRep))
        ]

||| Create an StgCase which will represent a pure function call on the Haskell side.
|||
||| Use this function when the external does not represent an IO call.
export
createExtSTGPureApp
  :  Ref STGCtxt STGContext
  => ExtName -> List ArgSg
  -> Core (Expr Core.stgRepType)
createExtSTGPureApp ext args = do
  extCallResult <- mkFreshSBinderStr LocalScope emptyFC "extCallPureResult"
  extNameBinderId <- extNameLR ext
  pure
    $ StgCase
        PolyAlt
        (StgApp extNameBinderId args (SingleValue LiftedRep))
        extCallResult
        [ MkAlt AltDefault ()
          $ StgApp (getBinderId extCallResult) [] (SingleValue LiftedRep)
        ]
