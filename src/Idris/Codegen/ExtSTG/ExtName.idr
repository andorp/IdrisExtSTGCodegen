module Idris.Codegen.ExtSTG.ExtName

import Idris.Codegen.ExtSTG.STG


||| Name for module dependency with fully qualified name.
public export
data ExtName = MkExtName String (List String) String

export
Show ExtName where
  show (MkExtName p m f) = "MkExtName " ++ show p ++ show m ++ show f

export
extNameString : ExtName -> String
extNameString (MkExtName m p f) = m ++ "_" ++ concat (intersperse "." p) ++ "." ++ f

export
extNameFunction : ExtName -> String
extNameFunction (MkExtName x xs f) = f

-- ExtName

export
mkUnitId : ExtName -> UnitId
mkUnitId (MkExtName u _ _) = MkUnitId u

export
mkModuleName : ExtName -> ModuleName
mkModuleName (MkExtName _ m _) = MkModuleName (concat (intersperse "." m))

export
soloExtName : ExtName
soloExtName = MkExtName "ghc-prim" ["GHC", "Prim"] "Solo#"

export
erasedExtName : ExtName
erasedExtName = MkExtName "main" ["Idris","Runtime","Erased"] "Erased"
