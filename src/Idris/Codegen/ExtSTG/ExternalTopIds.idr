module Idris.Codegen.ExtSTG.ExternalTopIds

import Data.Strings
import Data.List
import Data.List1
import Data.List.Views
import Core.Context
import Data.StringMap
import Idris.Codegen.ExtSTG.Core
import Idris.Codegen.ExtSTG.STG

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

||| Name for module dependency with fully qualified name.
public export
data ExtName = MkExtName String (List String) String

Show ExtName where
  show (MkExtName p m f) = "MkExtName " ++ show p ++ show m ++ show f

export
data ExternalBinder : Type where

export
ExtBindMap : Type
ExtBindMap = StringMap (ExtName, SBinder)

export
mkExternalBinders : Core (Ref ExternalBinder ExtBindMap)
mkExternalBinders = newRef ExternalBinder empty

renderName : ExtName -> String
renderName (MkExtName pkg mdl fn) = pkg ++ ":" ++ concat (intersperse "." mdl) ++ "." ++ fn

||| Parse names that are expected to have the following format:
||| package:namespace.entries.function
export
parseName : String -> Maybe ExtName
parseName str = case break (==':') $ unpack str of
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
  :  {auto _ : Ref ExternalBinder ExtBindMap}
  -> {auto _ : UniqueMapRef}
  -> {auto _ : Ref Counter Int}
  -> ExtName
  -> Core BinderId
extName e@(MkExtName pkg mdl fn) = do
  extBindMap <- get ExternalBinder
  let entryName = renderName e
  case lookup entryName extBindMap of
    Nothing => do
      binder <- mkSBinderStr emptyFC fn
      put ExternalBinder $ insert entryName (e,binder) extBindMap
      pure $ Id binder
    Just (_, b) => pure $ Id b

||| Generate External Top Ids for an STG module.
export
genExtTopIds
  : {auto _ : Ref ExternalBinder ExtBindMap}
  -> Core (List (UnitId, ModuleName, SBinder))
genExtTopIds
  = map ( map
            (\(key, (MkExtName pck mdl fn, binder)) =>
              (MkUnitId pck, MkModuleName (concat (intersperse "." mdl)), binder))
        . StringMap.toList
        )
  $ get ExternalBinder
