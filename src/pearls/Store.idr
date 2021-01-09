-- https://blog.cofree.coffee/2020-10-17-bounded-space-automata/

module Store

import Data.Vect
import Data.Fin

interface Comonad (w : Type -> Type) where
  extract : w a -> a
  extend  : (w a -> b) -> w a -> w b

interface Functor f => Representable (f : Type -> Type) (rep : Type) | f where
  tabulate : (rep -> a) -> f a
  index    : f a -> (rep -> a)

{n : Nat} -> Representable (Vect n) (Fin n) where
  tabulate f {n = Z}     = []
  tabulate f {n = (S k)} = f FZ :: tabulate (f . FS)
  index (x :: _) FZ {n = (S k)} = x
  index (_ :: xs) (FS x) {n = (S k)} = index xs x

data Store : (f : Type -> Type) -> (rep : Type) -> (a : Type) -> Type where
  MkStore : rep -> f a -> Store f rep a

pos : Store f r a -> r
pos (MkStore r f) = r

peek : Representable f r => r -> Store f r a -> a
peek r (MkStore _ f) = index f r

peeks : Representable f r => (r -> r) -> Store f r a -> a
peeks g (MkStore r f) = index f (g r)

seek : Representable f r => r -> Store f r a -> Store f r a
seek r (MkStore _ f) = MkStore r f

seeks : Representable f r => (r -> r) -> Store f r a -> Store f r a
seeks g (MkStore r f) = MkStore (g r) f

experiment : (Representable f r, Functor g) => (r -> g r) -> Store f r a -> g a
experiment f s = map (`peek` s) (f (pos s))

Representable f r => Comonad (Store f r) where
  extract (MkStore r f) = index f r
  extend g (MkStore r f) = MkStore r (tabulate (\r' => g (MkStore r' f)))

up : {k : Nat} -> Fin (S k) -> Fin (S k)
up = either (const FZ) FS . strengthen

down : {k : Nat} -> Fin (S k) -> Fin (S k)
down FZ     = last
down (FS k) = weaken k

indices : {k : Nat} -> Fin (3 + k) -> Vect 3 (Fin (3 + k))
indices x = [down x, x, up x]

neighbors : {k : Nat} -> Store (Vect (3 + k)) (Fin (3 + k)) Bool -> Vect 3 Bool
neighbors = experiment indices

