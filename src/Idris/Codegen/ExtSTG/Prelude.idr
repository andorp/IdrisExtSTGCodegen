module Idris.Codegen.ExtSTG.Prelude


namespace FilePath
  export
  FilePath : Type
  FilePath = String

  export
  getFilePath : FilePath -> String
  getFilePath = id

  export
  mkFilePath : String -> FilePath
  mkFilePath = id

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

public export
numberFrom : Nat -> List a -> List (Nat, a)
numberFrom n xs = go xs n [] where
  go : List a -> Nat -> List (Nat, a) -> List (Nat, a)
  go [] k ys = ys
  go (x :: xs) k ys = (k, x) :: go xs (S k) ys

