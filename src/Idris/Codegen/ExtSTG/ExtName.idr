module Idris.Codegen.ExtSTG.ExtName

import Idris.Codegen.ExtSTG.STG

%default total

||| Name for module dependency with fully qualified name.
public export
data ExtName = MkExtName String (List String) String

export
Eq ExtName where
  (MkExtName m1 ns1 n1) == (MkExtName m2 ns2 n2) = m1 == m2 && ns1 == ns2 && n1 == n2

export
Ord ExtName where
  compare (MkExtName m1 ns1 n1) (MkExtName m2 ns2 n2) = case compare m1 m2 of
    LT => LT
    EQ => case compare ns1 ns2 of
      LT => LT
      EQ => compare n1 n2
      GT => GT
    GT => GT

export
Show ExtName where
  show (MkExtName p m f) = "MkExtName " ++ show p ++ show m ++ show f

export
extNameString : ExtName -> String
extNameString (MkExtName m p f) = m ++ "_" ++ concat (intersperse "." p) ++ "." ++ f

-- export
-- extNameFunction : ExtName -> String
-- extNameFunction (MkExtName x xs f) = f

export
stgName : ExtName -> STG.Name
stgName (MkExtName _ _ n) = n

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
