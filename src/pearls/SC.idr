module SC

-- import Data.Stream

{-
interface Comonad (w : Type -> Type) where
  extract   : w a -> a
  duplicate : w a -> w (w a)

Comonad Stream where
  extract   (x :: _)  = x
  duplicate (x :: xs) = (x :: xs) :: duplicate xs
-}

data Cofree : (Type -> Type) -> Type -> Type where
  (::) : a -> Inf (f (Cofree f a)) -> Cofree f a

Functor f => Functor (Cofree f) where
  map f (x :: xs) = f x :: map (map f) xs

duplicate : (Functor f) => Cofree f a -> Cofree f (Cofree f a)
duplicate (x :: xs) = (x :: xs) :: map duplicate xs

pure : Cofree f a -> a
pure (x :: _) = x

(>>=) : (Functor f) => Cofree f a -> (Cofree f a -> b) -> Cofree f b
w >>= g = map g (duplicate w)

extend : (Functor f) => (Cofree f a -> b) -> (Cofree f a) -> Cofree f b
extend g w = map g (duplicate w)

data Id a = MkId a

Functor Id where
  map f (MkId x) = MkId (f x)

Strm : Type -> Type
Strm = Cofree Id

iterate : (f : a -> a) -> (x : a) -> Strm a
iterate f x = x :: MkId (iterate f (f x))

zeros : Strm Int
zeros = 0 :: MkId zeros

test : Strm Int
test = do
  x <- zeros
  pure x

data Cofree : (Type -> Type) -> Type -> Type where
  (::) : a -> Inf (f (Cofree f a)) -> Cofree f a

data ZP a = MkZP a a
data MV = L | R

Functor ZP where
  map f (MkZP x y) = MkZP (f x) (f y)

browsing : (forall b . f b -> i -> b) -> Cofree f a -> List i -> a
browsing idx (a :: as) []        = a
browsing idx (a :: as) (k :: ks) = browsing idx (idx as k) ks

Zipper : Type -> Type
Zipper = Cofree ZP

move : ZP a -> MV -> a
move (MkZP l r) L = l
move (MkZP l r) R = r

moves : List MV -> Zipper a -> Zipper a
moves m = extend (\x => browsing move x m)


-- https://chrispenner.ca/posts/representable-cofree-zippers
-- moveLeft : Zipper a -> Zipper a

