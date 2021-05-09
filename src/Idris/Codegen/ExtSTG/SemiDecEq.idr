module Idris.Codegen.ExtSTG.SemiDecEq

public export
interface SemiDecEq t where
  semiDecEq : (x : t) -> (y : t) -> Maybe (x = y)

export
SemiDecEq Nat where
  semiDecEq Z      Z      = Just Refl
  semiDecEq (S n1) (S n2) = do
    Refl <- semiDecEq n1 n2
    Just Refl
  semiDecEq _      _      = Nothing

export
SemiDecEq a => SemiDecEq (List a) where
  semiDecEq []        []        = Just Refl
  semiDecEq (x :: xs) (y :: ys) = do
    Refl <- semiDecEq x y
    Refl <- semiDecEq xs ys
    Just Refl
  semiDecEq _ _ = Nothing
