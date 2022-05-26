||| Information about aliasing Haskell defined datatypes with Idris one.
module Idris.Codegen.ExtSTG.ADTAlias

import Core.TT
import Idris.Codegen.ExtSTG.Context

parseName : Core.Name.Name -> Maybe (List String, String)
parseName (NS ns (UN (Basic n))) = Just (unsafeUnfoldNamespace ns, n)
parseName other                  = Nothing

public export
Arity : Type
Arity = Nat

typeExtNameStr : String -> Maybe (ExtName, Arity)
typeExtNameStr "Prelude.Basics.List" = Just (MkExtName "ghc-prim" ["GHC", "Types"] "[]", 1)
typeExtNameStr "Prelude.Basics.Bool" = Just (MkExtName "ghc-prim" ["GHC", "Types"] "Bool", 0)
typeExtNameStr other = Nothing

termExtNameStr : String -> Maybe (ExtName, Arity)
termExtNameStr "Prelude.Basics.Nil"   = Just (MkExtName "ghc-prim" ["GHC", "Types"] "[]", 0)
termExtNameStr "Prelude.Basics.::"    = Just (MkExtName "ghc-prim" ["GHC", "Types"] "(:)", 2)
termExtNameStr "Prelude.Basics.True"  = Just (MkExtName "ghc-prim" ["GHC", "Types"] "True", 0)
termExtNameStr "Prelude.Basics.False" = Just (MkExtName "ghc-prim" ["GHC", "Types"] "False", 0)
termExtNameStr other = Nothing

export
typeExtName : Core.Name.Name -> Maybe (ExtName, Arity)
typeExtName = typeExtNameStr . show

export
constructorExtName : Core.Name.Name -> Maybe (ExtName, Arity)
constructorExtName = termExtNameStr . show 
