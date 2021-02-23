module Main

import Data.Nat
import Data.DPair

fib : (n : Nat) -> Int
fib Z = 0
fib (S Z) = 1
fib (S (S k)) = fib k + fib (S k)

fibc : (n : Nat) -> Int
fibc n = loop n 0 1
  where
    loop : (n : Nat) -> (f0 : Int) -> (f1 : Int) -> Int
    loop Z     f0 _  = f0
    loop (S n) f0 f1 = loop n f1 (f0 + f1)

data Fib : (0 _ : Nat) -> Int -> Type where
  Fib0 : Fib 0 0
  Fib1 : Fib 1 1
  FibN :  {0 n : Nat}
       -> {r0, r1 : Int}
       -> (0 _ : Fib n r0) -> (0 _ : Fib (S n) r1)
       -> Fib (S (S n)) (r0 + r1)

fibCert : (n : Nat) -> (r : Int ** Fib n r)
fibCert 0         = (_ ** Fib0)
fibCert (S 0)     = (_ ** Fib1)
fibCert (S (S k)) =
  case (fibCert k , fibCert (S k)) of
    ((r0 ** p0) , (r1 ** p1)) => (r0 + r1 ** FibN p0 p1)

fibcCert : (n : Nat) -> Int
fibcCert n = fst calc
  where
    loop : {r0, r1 : Int} -> (i : Nat) -> (ni : Nat) -> Fib i r0 -> Fib (S i) r1 -> Subset Int (Fib (ni + i))
    loop i Z     pf0 pf1 = (Element _ pf0)
    loop i (S k) pf0 pf1 = rewrite (plusSuccRightSucc k i) in loop (S i) k pf1 (FibN pf0 pf1)

    calc : Subset Int (Fib n)
    calc = rewrite (sym (plusZeroRightNeutral n)) in loop 0 n Fib0 Fib1

main : IO ()
main = printLn $ fibcCert 40
